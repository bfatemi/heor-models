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
  
  filter_wh <- "PRIMARY_PROCEDURE != '_none'
  AND PRIMARY_PROCEDURE != 'Out of Range'
  AND MODALITY != 'Out of Scope'
  AND EMERGENT_NONEMERGENT != 'emergent'
  AND PATIENT_TYPE in ('I', 'O')"
  
  if(is.null(hospID)){
    # WHERE <- NULL
    WHERE <- filter_wh
  }else{
    # WHERE <- paste0("HOSPITAL_ID = ", hospID)
    WHERE <- paste0(filter_wh, " AND HOSPITAL_ID = ", hospID)
  }
  
  # Check whether caller is on network
  check <- system("ping corp.intusurg.com -n 1 -w 1000", show.output.on.console = FALSE)
  if(check > 0) 
    stop("ISI network not detected", call. = FALSE)
  
  icon   <- secret::get_secret("is_connect")
  cn_txt <- stringr::str_c(names(icon$cn_args), "=", icon$cn_args, collapse = ";")
  cn     <- RODBC::odbcDriverConnect(cn_txt)
  on.exit( RODBC::odbcClose(cn) )
  
  tbl   <- paste0(icon$db_args$db, ".", icon$db_args$tbl_cat, ".", icon$db_args$tbl_nam)
  query <- paste0("SELECT * \nFROM ", tbl, c("\nWHERE ")[!is.null(WHERE)], WHERE)
  cat(query)
  # get column names, send query, get rows
  cnames <- RODBC::sqlColumns(cn, icon$db_args$tbl_nam)[["COLUMN_NAME"]]
  
  stopifnot(RODBC::odbcQuery(cn, query, 1000) == 1)
  dat <- RODBC::odbcFetchRows(cn)
  
  # set as data.table, set column names and return
  setDT(dat$data)
  setnames(dat$data, cnames)
  return(dat$data)
}