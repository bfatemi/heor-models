#' Run Full Analysis with PSM Matching
#' 
#' Use runPSM() to execute entire workflow. Alternatively utilize modular functions for
#' an interactive execution of this PSM analysis
#'
#' @param hospID A numeric variable representing the hospital id
#' 
#' @param hospDT Hospital data
#' @param idcols Columns to keep as IDs
#' @param strat Columns to stratify the hospital data before PSM
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
runPSM <- function(hospID = NULL){

  # QUERY AND CLEAN DATA
  idcols   <- c("HospitalID", "HospitalName", "PatientIDDeidentified", "Modality")
  strat    <- c("ProcedurePrimary", "BenignMalignant", "InpatientOutpatient")
  covars   <- c("PatientBMI", "PatientAge", "PatientCharlsonScore", "PatientGender")
  outcomes <- c("LOSDays", "ORTimeMins")
  
  DT <- getDataQTI(hospID, c(idcols, strat, covars, outcomes))
  
  # SPLIT AND DROP HOSPITALS WITH FEWER THAN 10 CASES FOR EACH MODAL
  psmInput <- split_check_hosp(DT, modal_A = "Open", modal_B = "Robotic")
  
  # EXEC ANALYSIS FOR EACH HOSPITAL
  psmDataList <- lapply(psmInput, run_match_stats, idcols, strat, covars, outcomes)
  
  # BIND ALL HOSP RESULTS TOGETHER AND JOIN TO BRING IN HOSP NAME
  nDT    <- rbindlist(psmDataList)
  htable <- DT[, .N, c("HospitalID", "HospitalName")][, !"N"]
  outDT  <- htable[nDT, on = "HospitalID"]
  
  # TRANSFORM DATA PER REQUIREMENTS (NOT OPTIMAL FOR FURTHER ANALYSIS OR VISUALIZATION)
  return( transformResult(outDT) )
  
}



#' @describeIn PSM_Functions Function that takes one hospital's data and appropriate argument to execute 
#'   the matching (using internal function \code{getMatchedData}), then runs statistics across matched 
#'   groups (using internal function \code{getStats})
#' @export
run_match_stats <- function(hospDT, idcols, strat, covars, outcomes){
  
  # update oucomes as some may be all NAs
  outcomes <- names( which(apply( hospDT[, outcomes, with=FALSE], 2, function(col) sum(!is.na(col)) > 0 )) )
  
  if(length(outcomes) == 0){
    warning("All selected outcome variables have only NA values", call. = FALSE)
    return(NULL)
  }
  
  mDT <- getMatchedData(DT = hospDT, 
                        strat_vars = strat, 
                        covariates = covars, 
                        outcome_vars = outcomes)
  
  res <- getStatData(DT = mDT, outcome_vars = outcomes)
  
  grpCols <- c("CID", "Modality", "HospitalID", "ProcedurePrimary", "BenignMalignant","InpatientOutpatient")
  dtab <- mDT[, list(PSMCount = .N), grpCols]
  
  
  keyCols <- c("CID", "PSMCount", "Modality")
  setkeyv(dtab, keyCols)
  setkeyv(res,  keyCols)
  
  return(res[dtab])
}







