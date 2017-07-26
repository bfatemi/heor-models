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
  cDT <- clean_data(get_raw(hospID))
  
  if(nrow(cDT) == 0) 
    stop("No data available")
  
  return(cDT[!is.na(get("MODALITY"))])
  
  # cDT <- clean_data(DT[
  #   get("PRIMARY_PROCEDURE") != '_none'
  #   & get("EMERGENT_NONEMERGENT") != 'emergent'
  #   & get("BMI") > 0
  #   & get("PATIENT_TYPE") %in% c('I', 'O')
  #   & get("MODALITY") %in% c("Robotic", "Open")
  #   ])
  
}







