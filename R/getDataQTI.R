#' [TITLE NEEDED]
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param SELECT [DESCRIPTION OF ARGUMENT NEEDED]
#' @param WHERE [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import data.table
#' 
#' @export
getDataQTI <- function(SELECT = "*", WHERE = NULL){
  
  ## CRITICAL CLEANING FOR DATA ERRORS DONE HERE
  DT <- get_raw()[
    get("PRIMARY_PROCEDURE") != "_none"
    & get("EMERGENT_NONEMERGENT") != "emergent"
    & get("BMI_CATEGORY_PSM") != "Not Present"
    & get("PATIENT_TYPE") %in% c("I", "O")
    ]
  
  
  if(nrow(DT) == 0){
    stop("No data available")
  }
  
  # MAKE FRIENDLY PAT ID
  DT[, c("PID") := .GRP, "PATIENT_ID_DEIDENTIFIED"]
  
  # CHARLSON SCORE SHOULD BE FACTORS (E.G. GROUPING VARIABLE) AND NOT 
  # INTEGERS BECAUSE OF RANGE AND VARIATION IN THE VARIABLE.
  set(x = DT, 
      i = NULL, 
      j = "CHARLSON_SCORE", 
      value = DT[, as.factor(get("CHARLSON_SCORE"))])
  
  return( DT[] )
}

#' @describeIn getDataQTI helper function to perform query
get_raw <- function(){
  
  SELECT <- "*" #hardcode and add flexibility later
  WHERE <- NULL #hardcode and add flexibility later
  
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

