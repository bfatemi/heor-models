#' Run Full Analysis with PSM Matching
#' 
#' Use runPSM() to execute entire workflow. Alternatively utilize modular functions for
#' an interactive execution of this PSM analysis
#'
#' @param hospID A numeric variable representing the hospital id
#' @param verbose A boolean indicating whether to print information about the connection and query being executed
#' @param wide A boolean indicating whether the data should be tranformed (default is TRUE per requirements)
#' @param hospDT Hospital data
#' @param idcols Columns to keep as IDs
#' @param stratby Columns to stratify the hospital data before PSM
#' @param covars Covariates to use to calculate the propensity score to match on
#' @param outcomes Outcome column variables to run statistics for across groups matched with PSM
#' 
#' @param n internal argument for helper \code{caller_env}
#'
#' @import RODBC
#' @import stats
#' @import data.table
#' 
#' @importFrom stringr str_split_fixed str_detect str_c
#' 
#' @name PSM_Functions
NULL



#' @describeIn PSM_Functions Helper function for determining the parent caller (from package:rlang)
caller_env <- function (n = 1) parent.frame(n + 1)



#' @describeIn PSM_Functions Execute entire workflow
#' @export
runPSM <- function(hospID = NULL, verbose = TRUE, wide = TRUE){

  
  # EXEC ANALYSIS FOR EACH HOSPITAL
  idcols    <- c("HospitalID", "HospitalName", "PatientIDDeidentified")
  stratby   <- c("ProcedurePrimary", "BenignMalignant", "InpatientOutpatient")
  covars    <- c("PatientBMI", "PatientAge", "PatientCharlsonScore", "PatientGender")
  outcomes  <- c("LOSDays", "ORTimeMins")
  match_var <- c("Modality")
  
  DT <- getDataQTI(hospID  = hospID, 
                   cols    = c(idcols, stratby, covars, outcomes, match_var),
                   verbose = verbose)
  
  # SPLIT AND DROP HOSPITALS WITH FEWER THAN 10 CASES FOR EACH MODAL
  psmInput <- split_check_hosp(DT = DT, 
                               modal_A = "Open", 
                               modal_B = "Robotic")

  new_ids <- c("HospitalID", "PatientID")
  psmResLL <- lapply(psmInput, function(i) run_match_stats(i, new_ids, stratby, covars, outcomes, match_var))
  
  # BIND ALL HOSP RESULTS TOGETHER AND JOIN TO BRING IN HOSP NAME
  nDT    <- rbindlist(psmResLL)
  htable <- DT[, .N, c("HospitalID", "HospitalName")][, !"N"]
  outDT  <- htable[nDT, on = "HospitalID"]
  
  # TRANSFORM DATA PER REQUIREMENTS (NOT OPTIMAL FOR FURTHER ANALYSIS OR VISUALIZATION)
  if(wide)
    return( transformResult(outDT) )
  return( outDT )
  
}



#' @describeIn PSM_Functions Function that takes one hospital's data and appropriate argument to execute 
#'   the matching (using internal function \code{getMatchedData}), then runs statistics across matched 
#'   groups (using internal function \code{getStats})
#' @export
run_match_stats <- function(hospDT, idcols, stratby, covars, outcomes, match_var){

  outcomes <- names( which(apply( hospDT[, outcomes, with=FALSE], 2, function(col) sum(!is.na(col)) > 0 )) )
  
  if(length(outcomes) == 0){
    warning("All selected outcome variables have only NA values", call. = FALSE)
    return(NULL)
  }
  
  matchDT <- getMatchedData(hospDT    = hospDT, 
                            stratby   = stratby, 
                            covars    = covars, 
                            outcomes  = outcomes, 
                            match_var = match_var)
  
  statDT <- getStatData(matchDT  = matchDT, 
                        outcomes = outcomes,
                        groupby  = c("CID", match_var))
  
  grpCols <- c("CID", "HospitalID", match_var, stratby)
  dtab <- matchDT[, list(PSMCount = .N), grpCols]
  
  keyCols <- c("CID", "PSMCount", match_var)
  setkeyv(dtab,   keyCols)
  setkeyv(statDT, keyCols)
  
  return(statDT[dtab])
}







