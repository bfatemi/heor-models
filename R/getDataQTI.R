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
  AND BMI > 0 
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

# clean_raw <- function(raw_data){
#   raw_data[, c("PATIENT_ID") := .GRP, "PATIENT_ID_DEIDENTIFIED"]
#   set(raw_data, NULL, "PATIENT_ID_DEIDENTIFIED", NULL)
#   
#   cnam <- colnames(raw_data)
#   
#   col_hosp <- unique(
#     c(which(str_detect(cnam, "HOSPITAL_")),
#       which(str_detect(cnam, "^YEAR")))
#   )
#   
#   col_pat <- unique(
#     c(which(str_detect(cnam, "PROCEDURE")),
#       which(str_detect(cnam, "ASA_SCORE_NUMERIC")),
#       which(str_detect(cnam, "^PATIENT")),
#       which(str_detect(cnam, "BMI$")),
#       which(str_detect(cnam, "AGE$")),
#       which(str_detect(cnam, "CHARLSON_SCORE")),
#       which(str_detect(cnam, "BENIGN_MALIGNANT")),
#       which(str_detect(cnam, "MODALITY")))
#   )
#   set(raw_data, NULL, "ASA_SCORE_NUMERIC", as.numeric(raw_data[, get("ASA_SCORE_NUMERIC")]))
#   set(raw_data, NULL, "PATIENT_AGE", as.numeric(raw_data[, get("PATIENT_AGE")]))
#   set(raw_data, NULL, "BMI", as.numeric(raw_data[, get("BMI")]))
#   set(raw_data, NULL, "YEAR", as.numeric(raw_data[, get("YEAR")]))
#   
#   col_dur <- unique(
#     c(which(str_detect(cnam, ".+_TIME_MINS")),
#       which(str_detect(cnam, ".+_DAYS")),
#       which(str_detect(cnam, ".+_HOURS")))
#   )
#   
#   
#   col_fin <- unique(
#     c(which(str_detect(cnam, "REVENUE")),
#       which(str_detect(cnam, "COST")),
#       which(str_detect(cnam, "MARGIN")),
#       which(str_detect(cnam, "_CHARGES")))
#   )
#   
#   
#   rDT <- raw_data[, cnam[c(col_hosp, col_pat, col_dur, col_fin)], with = FALSE]
#   
#   # get summary of data and clean out sparse cols
#   col_descr <- easydata::easy_describe(rDT)
#   
#   drop <- col_descr[count_unique == 1 | pct_NA > .75, col_name]
#   for(col in drop)
#     set(rDT, NULL, col, NULL)
#   
#   ## MAKE DUR COLUMNS NUMERIC AND REMAINING COST COLS
#   cnam <- colnames(rDT)
#   
#   col_dur <- unique(
#     c(which(str_detect(cnam, ".+_TIME_MINS")),
#       which(str_detect(cnam, ".+_DAYS")),
#       which(str_detect(cnam, ".+_HOURS")))
#   )
#   for(col in cnam[col_dur])
#     set(raw_data, NULL, col, as.numeric(raw_data[, get(col)]))
#   
#   cnam <- colnames(rDT)
#   col_fin <- unique(
#     c(which(str_detect(cnam, "REVENUE")),
#       which(str_detect(cnam, "COST")),
#       which(str_detect(cnam, "MARGIN")),
#       which(str_detect(cnam, "_CHARGES")))
#   )
#   for(col in cnam[col_fin])
#     set(raw_data, NULL, col, as.numeric(raw_data[, get(col)]))
#   
#   return(rDT)
# }

# 
# raw_data <- get_raw()
# rDT <- clean_raw(raw_data)









