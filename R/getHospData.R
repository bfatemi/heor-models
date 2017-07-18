#' [TITLE NEEDED]
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param SELECT [DESCRIPTION OF ARGUMENT NEEDED]
#' @param WHERE [DESCRIPTION OF ARGUMENT NEEDED]
#'
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' @export
getHospData <- function(SELECT = "*", WHERE = NULL){
  
  ## DEFINE CONNECTION PARAMS
  srv     <- "AZCWDA0008"
  db      <- "CUSTOM_QTI"
  tbl_cat <- "Hospital"
  tbl_nam <- "Fact_CASReport_Hospital"
  tbl     <- paste0(db, ".", tbl_cat, ".", tbl_nam)
  
  
  ## DEFINE QUERY, EXECUTE AND RETRIEVE RESULTS OF QUERY
  query   <- paste0("SELECT ", SELECT, "\nFROM ", tbl, c("\nWHERE ")[!is.null(WHERE)], WHERE)
  rawDT   <- isdb::getData("QTI", query)
  
  ## IF QUERY ERROR, THEN CONVERT TO R ERROR AND STOP EXECUTION
  if(nrow(rawDT) == 1 & ncol(rawDT) == 1 & rawDT[1,1] == -1)
    stop("Query caused error: ", query, call. = FALSE)
  
  ## CRITICAL CLEANING FOR DATA ERRORS DONE HERE
  rDT <- rawDT[
    get("PRIMARY_PROCEDURE") != "_none"
    & get("EMERGENT_NONEMERGENT") != "emergent"
    & get("BMI_CATEGORY_PSM") != "Not Present"
    & get("PATIENT_TYPE") %in% c("I", "O")
    ]
  
  # MAKE FRIENDLY PAT ID
  rDT[, c("PID") := .GRP, "PATIENT_ID_DEIDENTIFIED"]
  
  # CHARLSON SCORE SHOULD BE FACTORS (E.G. GROUPING VARIABLE) AND NOT 
  # INTEGERS BECAUSE OF RANGE AND VARIATION IN THE VARIABLE.
  set(x = rDT, 
      i = NULL, 
      j = "CHARLSON_SCORE", 
      value = rDT[, as.factor(get("CHARLSON_SCORE"))])
  
  return( rDT[] )
}
