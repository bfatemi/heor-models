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
  resDT[, IS_ROBOTIC := (MODALITY == "Robotic")]
  return(resDT[])
}


# SUBSET DATA FOR TWO MODALITIES AND DESIRED HOSP -------------------------

rawDT <- getPatientData()

DT <- rawDT[MODALITY %in% c("Robotic", "Open") & HOSPITAL_ID == 10112]



# DEFINE INPUTS THAT WILL STRATIFY DATA FOR PSM ---------------------------


treatment <- "IS_ROBOTIC"
split_var <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT")

covars    <- c("PATIENT_TYPE",
               "BMI_CATEGORY_PSM",
               "AGE_CATEGORY_PSM")
outcome   <- c("LOS_HOURS", "OR_TIME_MINS")




# RUN PSM ANALYSIS --------------------------------------------------------


getPropScore <- function(DT, covars, treatment, outcome){


  # CONSTRUCT MODEL DATA AND FORMULA, THEN GET MODEL AND CALC PROPENSITY SCORES
  psmDT <- DT[, c("PID", outcome, split_var, treatment, covars), with=FALSE]

  env <- rlang::caller_env()

  f <- rlang::new_formula(
    rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(covars, collapse = " + "))),
    lhs = as.name(treatment),
    env = env
  )

  ml <- lm(formula = f, data = psmDT)
  summary(ml)
  psmDT[, Propensity_Score := predict.glm(ml, psmDT)]

  # MATCH PATIENTS THEN DO T-TEST
  procList <- split(psmDT, psmDT$PRIMARY_PROCEDURE)
  bmList   <- split(procList$Hyst, hdt$BENIGN_MALIGNANT)


  # match each
  i <- bmList[[1]]

  iList <- split(i, i$IS_ROBOTIC)

  rDT <- iList$`TRUE`
  oDT <- iList$`FALSE`


match_data <- data.table::rbindlist(apply(rDT, 1, function(i){

  i <- rDT[1]

  p_score <- as.numeric(i[, "Propensity_Score", with=FALSE])

  indexMatch <- oDT[, which.min(p_score - Propensity_Score)][1]
  oDT[62]

  library(ggplot)
  (oDT) + geom_hist(aes(BMI_CATEGORY_PSM, Propensity_Score))
  res <- cbind(row, data.table(match_pid = oDT[indexMatch, PID]))
  oDT <<- oDT[-indexMatch]
  return(res)
}))

oDT <- iList$`FALSE`
statDT <- oDT[PID %in% match_data$match_pid]

morph_stat()





getPropScore(sDT, covars, treatment, outcome)
