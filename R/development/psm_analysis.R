library(isds)
library(isdb)

getPatientData <- function(SELECT = "*", WHERE = NULL){
  srv     <- "AZCWDA0008"
  db      <- "CUSTOM_QTI"
  tbl_cat <- "Hospital"
  tbl_nam <- "Fact_CASReport_Hospital"
  tbl     <- paste0(paste0("[", db, "]"), ".", paste0("[", tbl_cat, "]"), ".", paste0("[", tbl_nam, "]"))
  query   <- paste0("SELECT ", SELECT, "\nFROM ", tbl, c("\nWHERE ")[!is.null(WHERE)], WHERE)
  rawDT   <- getData("QTI", query)

  if(nrow(rawDT) == 1 & ncol(rawDT) == 1 & rawDT[1,1] == -1)
    stop("Query caused error: ", query, call. = FALSE)

  resDT <- rawDT[
    PRIMARY_PROCEDURE != "_none"
    & EMERGENT_NONEMERGENT != "emergent"
    & BMI_CATEGORY_PSM != "Not Present"
    & PATIENT_TYPE %in% c("I", "O")
    ]

  # MAKE FRIENDLY PAT ID
  resDT[, PID := .GRP, PATIENT_ID_DEIDENTIFIED]
  resDT[, PATIENT_ID_DEIDENTIFIED := NULL]

  # ADD C SCORE AS FACTOR, MAKE A BOOLEAN FOR ROBOTIC MODALITY
  resDT[, CHARLSON_SCORE := as.factor(CHARLSON_SCORE)]
  # resDT[, IS_ROBOTIC := (MODALITY == "Robotic")]
  return(resDT[])
}


# SUBSET DATA FOR TWO MODALITIES AND DESIRED HOSP -------------------------

rawDT <- getPatientData()

DT <- rawDT[MODALITY %in% c("Robotic", "Open") & HOSPITAL_ID == 10112]



# DEFINE INPUTS THAT WILL STRATIFY DATA FOR PSM ---------------------------


treatment <- c("MODALITY")
split_var <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")

covars    <- c("BMI",
               "PATIENT_AGE",
               "CHARLSON_SCORE",
               "PATIENT_GENDER")

outcome   <- c("LOS_HOURS", "OR_TIME_MINS")[[1]]




# RUN PSM ANALYSIS --------------------------------------------------------


getPropScore <- function(DT, treatment, split_var, covars, outcome){
  
  
  # CONSTRUCT MODEL DATA AND FORMULA, THEN GET MODEL AND CALC PROPENSITY SCORES
  keepCols <- c("HOSPITAL_ID", "PID", outcome, split_var, treatment, covars)
  psmDT <- DT[BMI > 0, keepCols, with=FALSE]
  psmDT[, CID := .GRP, c(split_var)]
  psmDT[, CID := as.factor(CID)]
  
  setkeyv(psmDT, "CID")
  tmp <- dcast(psmDT[, .N < 10, c("CID", treatment)][V1 == FALSE, !"V1"], 
               CID ~ MODALITY, 
               value.var = "MODALITY",
               fun.aggregate = length)
  cDT <- psmDT[CID %in% as.numeric(tmp[which(rowSums(tmp[, !"CID"]) == 2), CID])]
  
  tmp   <- split(cDT, cDT$CID)
  mList <- tmp[sapply(tmp, nrow) > 0]
  
  
  RESULT <- rbindlist(
    lapply(mList, function(mDT){
      mDT[, IS_ROBOTIC := MODALITY == "Robotic"]
      
      # if covariates don't vary, need to remove them. If not left, PSM shouldn't be applied
      valid_covars <- covars[sapply(covars, function(i) length(unique(mDT[, get(i)]))) > 1]
      if(length(valid_covars) == 0)
        return(NULL)
      
      ## GET MODEL EQUATION FOR EACH RUN
      env <- rlang::caller_env()
      
      f <- rlang::new_formula(
        rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(valid_covars, collapse = " + "))),
        lhs = as.name("IS_ROBOTIC"),
        env = env
      )
      
      psmll <- tryCatch({
        psm.ml    <- matchit(formula = f, mDT, method = "nearest", ratio = 1)
        
        index_rob <- as.numeric(row.names(psm.ml$match.matrix))
        index_ctr <- as.numeric(psm.ml$match.matrix[, 1])
        
        list(
          r = mDT[ index_rob ],
          c = mDT[ index_ctr ]
        )
        
      }, warning = function(c){
        
        if(stringr::str_detect(c$message, "Fewer control than treated units")){
          
          
          mDT[, IS_CONTROL := !IS_ROBOTIC]
          
          f <- rlang::new_formula(
            rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(valid_covars, collapse = " + "))),
            lhs = as.name("IS_CONTROL"),
            env = env
          )
          psm.ml    <- matchit(formula = f, mDT, method = "nearest", ratio = 1)
          
          index_rob <- as.numeric(psm.ml$match.matrix[, 1])
          index_ctr <- as.numeric(row.names(psm.ml$match.matrix))
          
          list(
            r = mDT[ index_rob ],
            c = mDT[ index_ctr ]
          )
          
        } 
      }, error = function(c){
        if(stringr::str_detect(c$message, "No units were matched"))
          return(NULL)
      })
      
      if( is.null(psmll) )
        return(NULL)
      
      
      # define list of functions to apply to stratified groups
      FUNS   <- list(
        PSM_COUNT = length,
        MEAN      = mean,
        MEDIAN    = median,
        STD       = sd,
        VAR       = var
      )
      
      names(FUNS)[-1] <- paste0(outcome, "_", names(FUNS)[-1])
      sdata <- rbindlist(psmll)[ , fapply(funs = FUNS, 
                                          vector = get(outcome), 
                                          min.obs = 10), 
                                 by = c(TREATMENT, "CID", "HOSPITAL_ID", GRP_VARS)]
      
      p_val <- data.table(
        round(t.test(psmll$r[, get(outcome)], psmll$c[, get(outcome)])$p.value, 8)
      )
      setnames(p_val, paste0(outcome, "_PVALUE"))
      return(cbind(sdata, p_val))
    })
  )
}
  
  
  
  # morph_stat(DAT = sDT, 
  #            GRP_VARS = "PRIMARY_PROCEDURE", 
  #            OUTCOME = ovar, 
  #            TREATMENT = "modality")
  # 
  # split(mDT, mDT[, get(treatment)])
  # 
#   # 
#   # 
#   # lapply(outcome, function(i, data) )
#   # procList <- split(psmDT, psmDT$PRIMARY_PROCEDURE)
#   # bmList   <- split(procList$Hyst, hdt$BENIGN_MALIGNANT)
#   # 
#   # 
#   # # mtch each
#   # i <- bmList[[1]]
#   # 
#   # iList <- split(i, i$IS_ROBOTIC)
#   # 
#   # rDT <- iList$`TRUE`
#   # oDT <- iList$`FALSE`
# 
# 
# match_data <- data.table::rbindlist(apply(rDT, 1, function(i){
# 
#   i <- rDT[1]
# 
#   p_score <- as.numeric(i[, "Propensity_Score", with=FALSE])
# 
#   indexMatch <- oDT[, which.min(p_score - Propensity_Score)][1]
#   oDT[62]
# 
#   library(ggplot)
#   (oDT) + geom_hist(aes(BMI_CATEGORY_PSM, Propensity_Score))
#   res <- cbind(row, data.table(match_pid = oDT[indexMatch, PID]))
#   oDT <<- oDT[-indexMatch]
#   return(res)
# }))
# 
# oDT <- iList$`FALSE`
# statDT <- oDT[PID %in% match_data$match_pid]
# 
# morph_stat()
# 
# 
# 
# 
# 
# getPropScore(sDT, covars, treatment, outcome)
