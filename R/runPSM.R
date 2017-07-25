#' Run Full Analysis with PSM Matching
#'
#' @param hospID A numeric variable representing the hospital id
#'
#' @return [DOCUMENT RETURN VALUE]
#' @export
#'
#' @import data.table
runPSM <- function(hospID = NULL){
  if(is.null(hospID)){
    qDT <- getDataQTI()
  }else{
    qDT <- getDataQTI(hospID = hospID)
  }
    
  
  if(nrow(qDT) == 0) 
    stop("No data for modality and/or hosp id", call. = FALSE)
  
  hospList <- split(qDT, qDT$HOSPITAL_ID)
  
  strat    <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")
  covars   <- c("BMI", "PATIENT_AGE", "CHARLSON_SCORE", "PATIENT_GENDER")
  outcomes <- c("LOS_HOURS", "OR_TIME_MINS")
  
  ##
  ## Run analysis for each hospital
  ##
  psmDataList <- lapply(hospList, function(hDT){
    DT <- psm_data(hDT, strat, covars, outcomes)
    
    # get result and merge back with descriptor columns needed
    res <- get_stats(DT, outcomes)
    dtable <- DT[, .(PSM_COUNT = .N), c("CID", 
                                        "MODALITY",
                                        "HOSPITAL_ID", 
                                        "PRIMARY_PROCEDURE", 
                                        "BENIGN_MALIGNANT", 
                                        "PATIENT_TYPE")]
    
    setkeyv(dtable, c("CID", "PSM_COUNT", "MODALITY"))
    setkeyv(res,    c("CID", "PSM_COUNT", "MODALITY"))
    return(res[dtable])
  })
  
  # Bind together all results for individual hospitals, then join to add hosp name
  DT <- rbindlist(psmDataList)
  htable <- qDT[, .N, c("HOSPITAL_ID", "HOSPITAL_NAME")][, !"N"]
  
  outDT <- htable[DT, on = "HOSPITAL_ID"]
  
  # change names for convenience
  setnames(outDT, c("C_INT_L", "C_INT_H"), c("CLOW", "CHIGH"))
  setnames(outDT, c("T_STAT", "P_VAL"), c("TSTAT", "PVAL"))
  setnames(outDT, "PSM_COUNT", "N")
  
  ## BEGIN TRANSFORMATION OF DATA
  stat_cols <- c("N", "MEAN", "MEDIAN", "STD", "VAR", "TSTAT", "PVAL", "CLOW", "CHIGH")
  
  
  env <- caller_env()
  y <- paste0(colnames(outDT)[ !colnames(outDT) %in% c(stat_cols, "outcome") ], collapse = " + ")
  x <- "outcome"
  f.expr <- paste0(y, " ~ ", x)
  
  
  woutDT <- dcast.data.table(data = outDT, 
                             sep = ".", 
                             formula = as.formula(f.expr, env), 
                             value.var = stat_cols, 
                             fill = NA)
  return(woutDT)
}