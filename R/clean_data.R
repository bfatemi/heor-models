#' Internal Data Cleaning Function
#'
#' @param rawDT [DESCRIPTION OF ARG NEEDED]
#' 
#' @importFrom easydata easy_describe
#' 
#' @return A trimmed data.table
#' @export
clean_data <- function(rawDT){

  cnam <- colnames(rawDT)
  expr <- substitute(which(stringr::str_detect(cnam, X)))
  pats <- c("Hospital",
            "^DischargeYear",
            "Procedure",
            "ASAScoreNumeric",
            "^Patient",
            "InpatientOutpatient",
            "PatientCharlsonScore",
            "BenignMalignant",
            "Modality",
            ".+TimeMins",
            ".+Days",
            ".+Hours")
  
  col_index <- unique(unlist(lapply(pats, function(i) eval(expr, list(X = i)))))
  
  rdata <- rawDT[, col_index, with=FALSE]
  
  cnam <- colnames(rdata)
  expr <- substitute(which(stringr::str_detect(cnam, X)))
  
  # get summary of data and clean out sparse cols
  col_descr <- easydata::easy_describe(rdata)
  
  # If column has only 1 unique value OR mostly NAs, then drop
  drop <- col_descr[get("count_unique") == 1 | get("pct_NA") > .85, "col_name"]
  for(col in drop)
    set(rdata, NULL, col, NULL)
  
  ## MAKE DUR COLUMNS NUMERIC AND REMAINING COST COLS
  cnam <- colnames(rdata)
  
  numPats <- c("PatientCharlsonScore",
               "PatientBMI$",
               "ASAScoreNumeric",
               "PatientAge$",
               ".+TimeMins",
               ".+Days",
               ".+Hours",
               "DischargeYear")
  num_cols <- cnam[unique(unlist(lapply(numPats, function(i) eval(expr, list(X = i)))))]
  
  for(col in num_cols){
    
    ## Clean up non-numeric garbage that would cause warning when converting class
    index <- which(rdata[, !stringr::str_detect(get(col), "[0-9]+")])
    if(length(index) > 1)
      set(rdata, index, col, NA)
    
    ## convert the column to numeric
    set(rdata, NULL, col, as.numeric(rdata[, get(col)]))
  }
  
  # MAKE PATIENT ID FRIENDLY
  rdata[, c("PatientID") := .GRP, "PatientIDDeidentified"]
  set(rdata, NULL, "PatientIDDeidentified", NULL)
  
  # CHARLSON SCORE SHOULD BE FACTORS (E.G. GROUPING VARIABLE) AND NOT 
  # INTEGERS BECAUSE OF RANGE AND VARIATION IN THE VARIABLE.
  set(x = rdata, NULL, "PatientCharlsonScore", rdata[, as.factor(get("PatientCharlsonScore"))])
  
  # PSM MODEL REQUIRES BOOLEAN TREATMENT VAR
  rdata[, c("IsRobotic") := get("Modality") == "Robotic"]
  
  # MAKE MODALITY A FACTOR WITH DEFINED LEVELS FOR CONVENIENCE
  rdata[, c("Modality") := as.factor(get("Modality"))]
  
  ## FILTER ON CRITICAL COLUMNS
  DT <- rdata[ !is.na(get("PatientCharlsonScore")) & 
                 !is.na(get("PatientBMI")) & 
                 get("ORTimeMins") > 0]
  return(DT)
}
