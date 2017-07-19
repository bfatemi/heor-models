#' Test Difference of Means with Balancing
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param DT [ARGUMENT DEFINITION NEEDED]
#' @param treat_var [ARGUMENT DEFINITION NEEDED]
#' @param split_vars [ARGUMENT DEFINITION NEEDED]
#' @param covariates [ARGUMENT DEFINITION NEEDED]
#' @param outcome_var [ARGUMENT DEFINITION NEEDED]
#'
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import stats
#' @import data.table
#' 
#' @export
getStatPSM <- function(DT = NULL, 
                       treat_var = NULL, 
                       split_vars = NULL, 
                       covariates = NULL, 
                       outcome_var = NULL){
  
  if(!"Robotic" %in% DT[, unique(get(treat_var))])
    stop("No Robotic modality observations detected in the treatment column", call. = FALSE)
  
  # INCLUDE ADDITIONAL VARS FROM DT:
  inc_vars <- c("HOSPITAL_ID", "PID")
  
  # CONSTRUCT MODEL DATA AND FORMULA, THEN GET MODEL AND CALC PROPENSITY SCORES
  keepCols <- c(inc_vars, 
                outcome_var, 
                split_vars, 
                treat_var, 
                covariates)
  
  # ENSURE VARS EXIST IN DT
  data_check(DT, keepCols)
  
  # TRIM DATA AND MAKE CID A NEW FACTOR COLUMN
  # psmDT <- DT[, keepCols, with=FALSE]
  # psmDT[, CID := .GRP, c(split_vars)]
  # setkeyv(psmDT, "CID")
  DT[, CID := .GRP, c(split_vars)]
  setkeyv(DT, "CID")
  
  # candidate data (cohorts that have at least 10 patients for each modality pair)
  N <- DT[, length(unique(get(treat_var)))] # number of modalities to get all pairs for
  
  # count patients for each modality and get all modality pairs that have at least 10
  cDT <- DT[CID %in% DT[, .N >= 10, c("CID", treat_var)][, sum(V1)>1, "CID"][, CID[which(V1)]]]
  
  if(nrow(cDT) == 0)
    stop("no modality pairs with at least 10 patients each", call. = FALSE)

  cDT[, IS_ROBOTIC := get(treat_var) == "Robotic"]
  cDT[, CID := as.factor(CID)]
  cDT[, MODALITY := as.factor(MODALITY)]
  
  # Split each cohort and run psm analysis
  iter <- 0
  matched_list <- lapply(split( cDT, cDT$CID ), function(mDT){
    iter <<- iter + 1
    print(iter)

    keep_cols <- c("CID", "HOSPITAL_ID", treat_var, split_vars, outcome_var, covariates)
    new_covars <- unlist(lapply(covariates, function(i) i[length(unique(mDT[, get(i)])) > 1]))
    control <- mDT[, .N, treat_var][which.max(N), get(treat_var)]
    f <- rlang::parse_expr(paste0("IS_TREATMENT ~ ", paste0(new_covars, collapse = " + ")))
    
    psDT <- mDT[, keep_cols, with = FALSE]
    psDT[, IS_TREATMENT := get(treat_var) != control]
    
    indNA <- which(Reduce(`|`, lapply(psDT, is.na)))
    
    if(length(indNA) == 0){
      cleanDT <- psDT  
    }else{
      cleanDT <- psDT[ -indNA ]
    }
    
    ml.psm <- MatchIt::matchit(data = cleanDT, 
                               formula = as.formula(f),
                               method = "nearest", 
                               ratio = 1, 
                               distance = "logit")
    
    matched_data <- MatchIt::get_matches(ml.psm, cleanDT)
    setDT(matched_data)
    return(matched_data)
  })
  result <- rbindlist(matched_list)
  return(result)
  #   # define list of functions to apply to stratified groups
  #   FUNS   <- list(
  #     PSM_COUNT = length,
  #     MEAN      = mean,
  #     MEDIAN    = median,
  #     STD       = sd,
  #     VAR       = var
  #   )
  #   o <- outcome_var[[1]]
  #   lapply(outcome_var, function(o){
  #     names(FUNS)[-1] <- paste0(o, "_", names(FUNS)[-1])
  #     
  #     keyby <- 
  #     sdata <- rbindlist(psmll)[ , fapply(FUNS, get(o), 10), by = keyby]  
  #   })
  #   
  #   
  #   # RUN T-TEST ACROSS OUTCOMES FOR BOTH MODALITY GROUPS
  #   y <- psmll$c[, get(o)]
  #   x <- psmll$r[, get(o)]
  #   t.ml <- t.test(x, y)
  #   
  #   # EXTRACT P-VALUE FROM DIFF OF MEANS T-TEST
  #   p_val <- data.table(round(t.ml$p.value, 8))
  #   setnames(p_val, paste0(o, "_PVALUE"))
  #   resDT <- cbind(sdata, p_val)
  #   setkeyv(resDT, keyby)
  #   return(resDT)
  #   # if covariates don't vary, need to remove them. If not left, PSM shouldn't be applied
  #   valid_covars <- covariates[sapply(covariates, function(i) length(unique(mDT[, get(i)]))) > 1]
  #   if(length(valid_covars) == 0)
  #     return(NULL)
  #   
  #   ## GET MODEL EQUATION FOR EACH RUN
  #   env <- rlang::caller_env()
  #   
  #   f <- rlang::new_formula(
  #     rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(valid_covars, collapse = " + "))),
  #     lhs = as.name("IS_ROBOTIC"),
  #     env = env
  #   )
  #   
  #   #
  #   # matchit spits out an error if ANY columns have missing values, including the columns
  #   # that are not even relevant. To work around, reduce the dataset to only the relevant
  #   # columns for each run
  #   #
  #   outList <- lapply(outcome_var, function(o){
  #     
  #     psm_data <- mDT[, c("IS_ROBOTIC", valid_covars, o), with=FALSE]
  #     
  #     psmll <- tryCatch({
  #       psm.ml <- MatchIt::matchit(formula = f, mDT, method = "nearest", ratio = 1)
  #       
  #       index_rob <- as.numeric(row.names(psm.ml$match.matrix))
  #       index_ctr <- as.numeric(psm.ml$match.matrix[, 1])
  #       
  #       list(
  #         r = mDT[ index_rob ],
  #         c = mDT[ index_ctr ]
  #       )
  #       
  #     }, warning = function(c){
  #       
  #       if(stringr::str_detect(c$message, "Fewer control than treated units")){
  #         
  #         
  #         mDT[, c("IS_CONTROL") := !get("IS_ROBOTIC")]
  #         
  #         f <- rlang::new_formula(
  #           rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(valid_covars, collapse = " + "))),
  #           lhs = as.name("IS_CONTROL"),
  #           env = env
  #         )
  #         
  #         psm.ml <- MatchIt::matchit(formula = f, 
  #                                    data = mDT, 
  #                                    method = "nearest", 
  #                                    ratio = 1)
  #         
  #         index_rob <- as.numeric(psm.ml$match.matrix[, 1])
  #         index_ctr <- as.numeric(row.names(psm.ml$match.matrix))
  #         
  #         return( list(r=mDT[index_rob], c=mDT[index_ctr]) )
  #         
  #       } 
  #     }, error = function(c){
  #       if(stringr::str_detect(c$message, "No units were matched"))
  #         return(NULL)
  #     })
  #     
  #     # RETURN NULL IF PSM IS A LIST OF NULLS (NO DATA MATCHED)
  #     if(is.null(psmll)) 
  #       return(NULL)
  #     
  #     # define list of functions to apply to stratified groups
  #     FUNS   <- list(
  #       PSM_COUNT = length,
  #       MEAN      = mean,
  #       MEDIAN    = median,
  #       STD       = sd,
  #       VAR       = var
  #     )
  #     names(FUNS)[-1] <- paste0(o, "_", names(FUNS)[-1])
  #     
  #     keyby <- c(treat_var, "CID", "HOSPITAL_ID", split_vars)
  #     sdata <- rbindlist(psmll)[ , fapply(FUNS, get(o), 10), by = keyby]
  #     
  #     # RUN T-TEST ACROSS OUTCOMES FOR BOTH MODALITY GROUPS
  #     y <- psmll$c[, get(o)]
  #     x <- psmll$r[, get(o)]
  #     t.ml <- t.test(x, y)
  #     
  #     # EXTRACT P-VALUE FROM DIFF OF MEANS T-TEST
  #     p_val <- data.table(round(t.ml$p.value, 8))
  #     setnames(p_val, paste0(o, "_PVALUE"))
  #     resDT <- cbind(sdata, p_val)
  #     setkeyv(resDT, keyby)
  #     return(resDT)
  #   })
  #   return( Reduce(function(x, y) x[y], outList) )
  # }))
  # return(RESULT)
}

data_check <- function(cols, DT){
  data_env <- rlang::as_env(DT)
  cols_missing <- cols[which(!rlang::env_has(data_env, cols))]
  if( length(cols_missing) > 0 ){
    return( cols_missing )
  }
  return( character(0) )
}