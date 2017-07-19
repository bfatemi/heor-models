#' [TITLE NEEDED]
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param hosp_id [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import data.table
#' 
#' @export
getDataQTI <- function(hosp_id = NULL){
  
  ## CRITICAL CLEANING FOR DATA ERRORS DONE HERE
  DT <- get_raw(hosp_id)
  
  if(nrow(DT) == 0) stop("No data available")
  
  # MAKE FRIENDLY PAT ID
  DT[, c("PID") := .GRP, "PATIENT_ID_DEIDENTIFIED"]
  
  ## Fix column classes here for now
  numcols <- c("BMI", "PATIENT_AGE", "LOS_HOURS", "OR_TIME_MINS")
  for(col in numcols)
    set(DT, i = NULL, j = col, value = DT[, as.numeric(get(col))])
  
  # CHARLSON SCORE SHOULD BE FACTORS (E.G. GROUPING VARIABLE) AND NOT 
  # INTEGERS BECAUSE OF RANGE AND VARIATION IN THE VARIABLE.
  set(x = DT, NULL, "CHARLSON_SCORE", DT[, as.factor(get("CHARLSON_SCORE"))])
  
  # PSM MODEL REQUIRES BOOLEAN TREATMENT VAR
  DT[, c("IS_ROBOTIC") := get("MODALITY") == "Robotic"]
  
  # MAKE MODALITY A FACTOR WITH DEFINED LEVELS FOR CONVENIENCE
  DT[, c("MODALITY") := factor(get("MODALITY"), levels = c("Open", "Robotic"))]
  return( DT[!is.na(get("MODALITY"))] )
}

#' @describeIn getDataQTI helper function to perform query
#' @export
get_raw <- function(hosp_id = NULL){
  SELECT <- "*"
  
  filter_wh <- "PRIMARY_PROCEDURE != '_none'
  AND EMERGENT_NONEMERGENT != 'emergent' 
  AND BMI_CATEGORY_PSM != 'Not Present' 
  AND PATIENT_TYPE in ('I', 'O')"
  
  if(is.null(hosp_id)){
    WHERE <- filter_wh
  }else{
    WHERE <- paste0(filter_wh, " AND HOSPITAL_ID = ", hosp_id)
  }
  
  check <- system("ping corp.intusurg.com -n 1 -w 1000")
  if(check > 0) stop("ISI network not detected", call. = FALSE)
  
  icon <- secret::get_secret("is_connect")
  cn <- RODBC::odbcDriverConnect(stringr::str_c(names(icon$cn_args), "=", icon$cn_args, collapse = ";"))
  on.exit(RODBC::odbcClose(cn))
  
  tbl <- paste0(icon$db_args$db, ".", icon$db_args$tbl_cat, ".", icon$db_args$tbl_nam)
  query <- paste0("SELECT ", SELECT, "\nFROM ", tbl, c("\nWHERE ")[!is.null(WHERE)], WHERE)
  
  # get column names, send query, get rows
  cnames <- RODBC::sqlColumns(cn, icon$db_args$tbl_nam)[["COLUMN_NAME"]]
  stopifnot(RODBC::odbcQuery(cn, query, 1000) == 1)
  dat <- RODBC::odbcFetchRows(cn)
  
  # set as data.table, set column names and return
  DT <- as.data.table(dat$data)
  setnames(DT, cnames)
  return(DT)
}

