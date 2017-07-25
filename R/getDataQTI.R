#' [TITLE NEEDED]
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param hospID [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import data.table
#' 
#' @export
getDataQTI <- function(hospID = NULL){
  
  ## CRITICAL CLEANING FOR DATA ERRORS DONE HERE
  DT <- get_raw(hospID)
  
  cDT <- clean_data(DT[
    get("PRIMARY_PROCEDURE") != '_none'
    & get("EMERGENT_NONEMERGENT") != 'emergent'
    & get("BMI") > 0
    & get("PATIENT_TYPE") %in% c('I', 'O')
    & get("MODALITY") %in% c("Robotic", "Open")
    ])
  
  
  if(nrow(cDT) == 0) stop("No data available")
  
  
  # CHARLSON SCORE SHOULD BE FACTORS (E.G. GROUPING VARIABLE) AND NOT 
  # INTEGERS BECAUSE OF RANGE AND VARIATION IN THE VARIABLE.
  set(x = cDT, NULL, "CHARLSON_SCORE", cDT[, as.factor(get("CHARLSON_SCORE"))])
  
  # PSM MODEL REQUIRES BOOLEAN TREATMENT VAR
  cDT[, c("IS_ROBOTIC") := get("MODALITY") == "Robotic"]
  
  # MAKE MODALITY A FACTOR WITH DEFINED LEVELS FOR CONVENIENCE
  cDT[, c("MODALITY") := as.factor(get("MODALITY"))]
  
  return(cDT[!is.na(get("MODALITY"))])
}







