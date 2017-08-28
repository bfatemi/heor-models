#' [TITLE NEEDED]
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param hospID [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import data.table
#' @importFrom openssl read_key
#' @name QTI_data
NULL

#' #' @describeIn QTI_data [DESCRIPTION OF FUNCTION NEEDED]
#' #' @export
#' getDataQTI <- function(hospID = NULL){
#'   cDT <- clean_data(get_raw(hospID))
#'   if(nrow(cDT) == 0) 
#'     stop("No data available")
#'   return(cDT)
#' }
#' 
#' #' @describeIn QTI_data [DESCRIPTION OF FUNCTION NEEDED]
#' #' @export
#' getLevelsQTI <- function(){
#'   
#'   kpath <- getOption("secret.key")
#'   vpath <- getOption("secret.vault")
#'   
#'   secret::get_secret(name = "levelList", 
#'                      key = read_key(kpath), 
#'                      vault = vpath)
#' }







