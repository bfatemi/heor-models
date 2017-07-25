#' Internal Data Query Function
#'
#' @param hospID [DESCRIPTION OF ARGUMENT NEEDED]
#'
#' @return [DESCRIPTION OF ARGUMENT NEEDED]
#' 
#' @name get_raw
NULL

#' @describeIn get_raw [DESCRIPTION OF FUNCTION NEEDED]
#' @export
get_raw <- function(hospID = NULL){
  
  # filter_wh <- "PRIMARY_PROCEDURE != '_none'
  # AND EMERGENT_NONEMERGENT != 'emergent'
  # AND BMI > 0
  # AND PATIENT_TYPE in ('I', 'O')"
  
  if(is.null(hospID)){
    WHERE <- NULL
    # WHERE <- filter_wh
  }else{
    WHERE <- paste0("HOSPITAL_ID = ", hospID)
    # WHERE <- paste0(filter_wh, " AND HOSPITAL_ID = ", hosp_id)
  }
  
  check <- system("ping corp.intusurg.com -n 1 -w 1000")
  if(check > 0) stop("ISI network not detected", call. = FALSE)
  
  icon <- secret::get_secret("is_connect")
  cn <- RODBC::odbcDriverConnect(stringr::str_c(names(icon$cn_args), "=", icon$cn_args, collapse = ";"))
  on.exit(RODBC::odbcClose(cn))
  
  tbl <- paste0(icon$db_args$db, ".", icon$db_args$tbl_cat, ".", icon$db_args$tbl_nam)
  query <- paste0("SELECT * \nFROM ", tbl, c("\nWHERE ")[!is.null(WHERE)], WHERE)
  
  # get column names, send query, get rows
  cnames <- RODBC::sqlColumns(cn, icon$db_args$tbl_nam)[["COLUMN_NAME"]]
  stopifnot(RODBC::odbcQuery(cn, query, 1000) == 1)
  dat <- RODBC::odbcFetchRows(cn)
  
  # set as data.table, set column names and return
  DT <- as.data.table(dat$data)
  setnames(DT, cnames)
  return(DT)
}