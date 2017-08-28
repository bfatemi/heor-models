#' Internal Data Query Function
#'
#' @param cnList Internal argument
#' @param cnString Internal argument
#' 
#' @importFrom secret get_secret list_secrets
#' @importFrom RODBC odbcDriverConnect
#' @importFrom openssl read_key
#' 
#' @name conn_functions
NULL

#' @describeIn conn_functions Internal function that builds the connection string using 
#' @export
build_conn_string <- function(cnList){
  cnString <- str_c(names(cnList$cn_args), "=", cnList$cn_args, collapse = ";")
  return(cnString)
}

#' @describeIn conn_functions Internal function that decrypts and returns the connection params
#' @export
conn_params <- function(){
  path_key   <- getOption("secret.key")
  path_vault <- getOption("secret.vault")
  
  # These should either be null or valid paths. Set NULL if empty string
  path_key[path_key == ""]     <- NULL
  path_vault[path_vault == ""] <- NULL
  
  # Error handling if vault is not found
  tryCatch({
    list_secrets(path_vault)
  }, error = function(c){
    msg <- paste0("\nIn conn_string: ", c$message, "\n\nExpected vault path: ", path_vault)
    stop(msg, call. = FALSE)
  })
  
  # Error handling if key is not read
  key <- tryCatch({
    read_key(path_key)
  }, error = function(c){
    msg <- paste0("\nIn conn_string: ", c$message, "\n\nExpecting key path: ", path_key)
    stop(msg, call. = FALSE)
  })
  
  # Get encrypted connection string arguments
  cnList <- get_secret("is_connect", key = key, vault = path_vault)
  return(cnList)
}


#' @describeIn conn_functions Internal function that returns the connection object
#' @export
get_conn_object <- function(cnString){
  tryCatch(odbcDriverConnect( cnString ), error = function(c) stop("ODBC connection", call. = FALSE))
}




