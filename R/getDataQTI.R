#' [TITLE NEEDED]
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param hospID [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import data.table
#' @name QTI_data
NULL

#' @describeIn QTI_data [DESCRIPTION OF FUNCTION NEEDED]
#' @export
getDataQTI <- function(hospID = NULL){
  cDT <- clean_data(get_raw(hospID))
  if(nrow(cDT) == 0) 
    stop("No data available")
  return(cDT[!is.na(get("MODALITY"))])
}

#' @describeIn QTI_data [DESCRIPTION OF FUNCTION NEEDED]
#' @export
getLevelsQTI <- function(){
  pkey  <- secret::local_key()
  vpath <- getOption("secret.vault")
  
  secret::get_secret(name = "levelList", 
                     key = pkey, 
                     vault = vpath)
}







