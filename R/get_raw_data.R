#' Internal Data Query Function
#'
#' @param hospID [DESCRIPTION OF ARGUMENT NEEDED]
#'
#' @return [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @importFrom secret get_secret
#' @importFrom stringr str_c
#' @importFrom RODBC odbcDriverConnect odbcClose sqlColumns odbcQuery odbcFetchRows
#' @importFrom openssl read_key
#' @name get_raw
NULL


#' @describeIn get_raw [DESCRIPTION OF FUNCTION NEEDED]
#' @export
get_conn <- function(){
  
  # Check whether caller is on network
  chk <- system("ping corp.intusurg.com -n 1 -w 1000", show.output.on.console = FALSE)
  if(chk > 0) 
    stop("ISI network not detected", call. = FALSE)
  
  kpath <- getOption("secret.key")
  vpath <- getOption("secret.vault")
  
  # Get encrypted connection string arguments
  ciph <- get_secret("is_connect", key = read_key(kpath), vault = vpath)
  
  # Get connection object and start query text
  conn <- odbcDriverConnect( str_c(names(ciph$cn_args), "=", ciph$cn_args, collapse = ";") )
  return(conn)
}



#' @describeIn get_raw [DESCRIPTION OF FUNCTION NEEDED]
#' @export
get_raw <- function(hospID = NULL){
  
  # Default cleaning filters
  WHERE <- "PRIMARY_PROCEDURE != '_none'
  AND PRIMARY_PROCEDURE != 'Out of Range'
  AND MODALITY != 'Out of Scope'
  AND EMERGENT_NONEMERGENT != 'emergent'
  AND PATIENT_TYPE in ('I', 'O')"
  
  # If hospital id provided, add condition to WHERE
  if( !is.null(hospID) )
    WHERE <- paste0(WHERE, " AND HOSPITAL_ID = ", hospID)
  
  # build query that's dynamic to whether a where condtion was supplied
  q1 <- "SELECT * FROM [CUSTOM_QTI].[Hospital].[Fact_CASReport_Hospital]"
  query <- str_c(q1, c("\nWHERE ")[!is.null(WHERE)], WHERE)
  
  # Get connection object and send query for validity check
  cn <- get_conn()
  on.exit( odbcClose(cn) )
  
  if( odbcQuery(cn, query, 1000) != 1 ) stop("check query")
  RES <- odbcFetchRows(cn)
  setDT(RES$data) # set as data.table
  
  # get column names, set and return table
  cnames <- sqlColumns(cn, "Fact_CASReport_Hospital")[["COLUMN_NAME"]]
  setnames(RES$data, cnames)
  return(RES$data)
}