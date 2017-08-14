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

  ciph <- get_conn_string()
  
  # Get connection object and start query text
  txt <- str_c(names(ciph$cn_args), "=", ciph$cn_args, collapse = ";")
  cat(paste0("\n\nConnection String:\n", txt, "\n\n"))
  conn <- odbcDriverConnect( txt )
  return(conn)
}


#' @describeIn get_raw [DESCRIPTION OF FUNCTION NEEDED]
#' @export
get_conn_string <- function(){
  kpath <- getOption("secret.key")
  vpath <- getOption("secret.vault")
  
  # do this for continuous integration testing. Travis is configured on backend
  if(kpath == "")
    kpath <- NULL
  if(vpath == "")
    vpath <- NULL
  
  # Get encrypted connection string arguments
  ciph <- get_secret("is_connect", key = read_key(kpath), vault = vpath)
  return(ciph)
}

#' @describeIn get_raw [DESCRIPTION OF FUNCTION NEEDED]
#' @export
get_raw <- function(hospID = NULL){
  
  
  # Default cleaning filters
  WHERE <- "ProcedurePrimary != 'Out of Range' 
  AND Modality != 'Out of Scope' 
  AND EmergentNonemergent = 'nonemergent' 
  AND InpatientOutpatient in ('I', 'O')"
  
  # If hospital id provided, add condition to WHERE
  if( !is.null(hospID) )
    WHERE <- paste0(WHERE, " \nAND HospitalID = '", hospID, "'")
  
  ciph <- get_conn_string()
  # print(ciph)
  
  # build query that's dynamic to whether a where condtion was supplied
  q0 <- paste0("[", ciph$db_args$db, "].[", ciph$db_args$tbl_cat, "].[", ciph$db_args$tbl_nam, "]")
  q1 <- paste0("SELECT * FROM ", q0)
  query <- str_c(q1, "\nWHERE ", WHERE)
  
  cat(paste0("Running Query: \n\n", query))
  
  # Get connection object and send query for validity check
  cn <- get_conn()
  on.exit( odbcClose(cn) )
  
  if( odbcQuery(cn, query, 1000) != 1 ) stop("check query")
  RES <- odbcFetchRows(cn)
  setDT(RES$data) # set as data.table
  
  # get column names, set and return table
  cnames <- sqlColumns(cn, sqtable = "CHAHospitalReport")[["COLUMN_NAME"]]
  setnames(RES$data, cnames)
  return(RES$data)
}