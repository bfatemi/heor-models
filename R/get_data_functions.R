#' Internal Data Query Function
#'
#' @param hospID [DESCRIPTION OF ARGUMENT NEEDED]
#' @param cols columns to limit query to for minor efficiency gains
#' 
#' @return [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @importFrom stringr str_c
#' @importFrom RODBC odbcClose sqlColumns odbcQuery odbcFetchRows
#' 
#' @name get_data
NULL


#' @describeIn get_data Returns raw data with minor filtering on the server for efficiency
#' @export
getDataQTI <- function(hospID = NULL, cols = "*"){
  
  # ESTABLISH CONNECTION AND BUILD QUERY
  cnParams <- conn_params()
  cnString <- build_conn_string(cnParams)
  cnObject <- get_conn_object(cnString)
  
  # ON EXIT, CLOSE CONNECTION
  on.exit( odbcClose(cnObject) )
  
  # BUILD QUERY COMPONENTS
  db  <- cnParams$db_args$db
  cat <- cnParams$db_args$tbl_cat
  tbl <- cnParams$db_args$tbl_nam
  
  # DEFAULT WHERE CLAUSE FOR BASIC CLEANING ON SERVER
  WHERE <- "\nWHERE ProcedurePrimary != 'Out of Range' 
  AND Modality in ('Open', 'Robotic')
  AND EmergentNonemergent = 'nonemergent' 
  AND InpatientOutpatient in ('I', 'O')
  AND ORTimeMins > 0
  AND LOSDays is not NULL
  AND PatientCharlsonScore is not NULL
  AND PatientBMI is not NULL"
  
  # ADD TO WHERE CONDITION IF HOSPITAL ID IS PROVIDED
  if( !is.null(hospID) ){
    WHERE <- paste0(WHERE, " \nAND HospitalID = '", hospID, "'")
  }
  
  query <- paste0("SELECT ", 
                  paste0(cols, collapse = ",\n\t"), 
                  "\nFROM ", "[", db, "].[", cat, "].[", tbl, "]", 
                  WHERE)
  
  # PRINT UPDATE ON CONSOLE FOR VALIDATION
  cat(paste0("\n\nODBC String:\n\n", cnString, "\n\n"))
  cat(paste0("\nRunning Query: \n\n", query, "\n\n"))

  # RUN QUERY AND SET RESULTS AS DATATABLE
  resDT <- tryCatch({
    stopifnot(odbcQuery(cnObject, query, rows_at_time = 1000) == 1)
    
    as.data.table(sqlGetResults(channel = cnObject, 
                                as.is = TRUE,
                                errors = TRUE,
                                stringsAsFactors = FALSE,
                                max = 0,
                                buffsize = 500000,
                                believeNRows = FALSE))
    
  }, error = function(c){
    stop("query failed in get_raw", call. = FALSE)
  })
  return(resDT)
}




#' @describeIn get_data Returns raw data with no filtering or cleaning
#' @export
getRawData <- function(){
  
  # ESTABLISH CONNECTION AND BUILD QUERY
  cnParams <- conn_params()
  cnString <- build_conn_string(cnParams)
  cnObject <- get_conn_object(cnString)
  
  # ON EXIT, CLOSE CONNECTION
  on.exit( odbcClose(cnObject) )
  
  # BUILD QUERY COMPONENTS
  db  <- cnParams$db_args$db
  cat <- cnParams$db_args$tbl_cat
  tbl <- cnParams$db_args$tbl_nam
  
  query <- paste0("SELECT *\nFROM ", "[", db, "].[", cat, "].[", tbl, "]")
  
  # PRINT UPDATE ON CONSOLE FOR VALIDATION
  cat(paste0("\n\nODBC String:\n\n", cnString, "\n\n"))
  cat(paste0("\nRunning Query: \n\n", query, "\n\n"))
  
  # RUN QUERY AND SET RESULTS AS DATATABLE
  rawDT <- tryCatch({
    stopifnot(odbcQuery(cnObject, query, rows_at_time = 1000) == 1)
    
    as.data.table(sqlGetResults(channel = cnObject, 
                                                   as.is = TRUE,
                                                   errors = TRUE,
                                                   stringsAsFactors = FALSE,
                                                   max = 0,
                                                   buffsize = 500000,
                                                   believeNRows = FALSE))
    
  }, error = function(c){
    stop("query failed in get_raw", call. = FALSE)
  })
  return(rawDT)
}

