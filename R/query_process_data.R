#' Internal Data Query Function
#'
#' @param hospID Internal hospital ID to get data for a specific customer
#' @param verbose A boolean indicating whether to print information about the connection and query being executed
#' @param cols columns to limit query to for minor efficiency gains
#' 
#' @param DT Data for processing after query function is run
#' @param modal_A Modality 1 (defaults to Open)
#' @param modal_B Modality 2 (defaults to Robotic)
#' 
#' @import data.table
#' @importFrom stringr str_c
#' @importFrom RODBC odbcClose sqlColumns odbcQuery odbcFetchRows
#' 
#' @name query_data
NULL


#' @describeIn query_data Returns raw data with minor filtering on the server for efficiency
#' @export
getDataQTI <- function(hospID = NULL, cols = "*", verbose = TRUE){
  
  # GET CONNECTION PARAMETERS AND CREATE CONNECT OBJECT
  cnParams <- conn_params()
  cnString <- build_conn_string(cnParams)
  cnObject <- get_conn_object(cnString)
  
  # ON EXIT, CLOSE CONNECTION
  on.exit( odbcClose(cnObject) )
  
  # USE CONNECT OBJECT TO BUILD QUERY COMPONENTS
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
  
  # CONSTRUCT QUERY WITH COMPONENTS ABOVE
  query <- paste0("SELECT ", 
                  paste0(cols, collapse = ",\n\t"), 
                  "\nFROM ", "[", db, "].[", cat, "].[", tbl, "]", 
                  WHERE)
  
  if(verbose){
    # PRINT UPDATE ON CONSOLE FOR VALIDATION
    cat(paste0("\n\nODBC String:\n\n", cnString, "\n\n"))
    cat(paste0("\nRunning Query: \n\n", query, "\n\n"))    
  }

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
    stop("query failed in getDataQTI", call. = FALSE)
  })
  
  # CLEAN DATA AND SET CORRECT COLUMN CLASSES
  num_cols <- c("PatientBMI", "PatientAge", "LOSDays", "ORTimeMins")
  fac_cols <- c("Modality", "PatientCharlsonScore", "PatientGender", "InpatientOutpatient", "BenignMalignant")
  
  for(ncol in num_cols)
    set(resDT, i=NULL, j = ncol, value = resDT[, as.numeric(get(ncol))])
  for(fcol in fac_cols)
    set(resDT, i=NULL, j = fcol, value = resDT[, as.factor(get(fcol))])
  
  if(nrow(resDT) == 0) 
    stop("No data", call. = FALSE)
  
  # CHANGE COLUMN NAMES FOR READABILITY
  setnames(resDT, "PatientIDDeidentified", "PatientID")
  
  # RETURN QUERIED AND CLEANED PSM DATA
  return(resDT)
}




#' @describeIn query_data Returns raw data with no filtering or cleaning
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
    stop("query failed in getRawData", call. = FALSE)
  })
  return(rawDT)
}



#' @describeIn query_data Function that splits a data set by hospital, and returns a list of hospitals that have enough 
#'    patients to proceed with the PSM analysis
#' @export
split_check_hosp <- function(DT, modal_A = "Open", modal_B = "Robotic"){
  
  # SPLIT BY HOSPITAL - NOTE DATA NEEDS A COLUMN CALLED HospitalID
  hospList <- split(DT, DT[, get("HospitalID")])
  
  # CHECK EACH HOSPITAL FOR A MINIMUM OF 10 PATIENTS FOR BOTH MODALITIES
  validBools <- sapply(hospList, function(hdt){
    x <- table(hdt$Modality)
    bool <- x[[modal_A]] > 10 & x[[modal_B]] > 10
    return(bool)
  })
  
  # GET NAMES OF HOSPITALS AND DROP WITH WARNING
  dropHosp <- names(which(!validBools))
  
  if(length(dropHosp) > 0)
    warning("Not enough data for both modalities for hospitals: ", paste0(dropHosp, collapse = ", "), call. = FALSE)
  
  return(hospList[ which(validBools) ])
}

