#' Run Full Analysis with PSM Matching
#'
#' @param hosp_id A numeric variable representing the hospital id
#'
#' @return [DOCUMENT RETURN VALUE]
#' @export
#'
#' @import data.table
runPSM <- function(hosp_id = 10112){
  if(is.null(hosp_id))
    DT <- getDataQTI()
  else
    DT <- getDataQTI(hosp_id = hosp_id)
  
  cDT <- DT[BMI > 0][MODALITY %in% c("Robotic", "Open")]
  
  if(nrow(cDT) == 0) 
    stop("No data for modality and/or hosp id", call. = FALSE)
  
  strat    <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")
  covars   <- c("BMI", "PATIENT_AGE", "CHARLSON_SCORE", "PATIENT_GENDER")
  outcomes <- c("LOS_HOURS", "OR_TIME_MINS")
  
  cohortDT <- psm_data(cDT, strat, covars, outcomes)
  return(get_stats(cohortDT, outcomes))
}