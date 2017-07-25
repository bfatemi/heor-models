#' Internal Data Cleaning Function
#'
#' @param rawDT [DESCRIPTION OF ARG NEEDED]
#'
#' @return A trimmed data.table
#' @export
clean_data <- function(rawDT){
  
  cnam <- colnames(rawDT)
  expr <- substitute(which(stringr::str_detect(cnam, X)))
  pats <- c("HOSPITAL_",
            "^YEAR",
            "PROCEDURE",
            "ASA_SCORE_NUMERIC",
            "^PATIENT",
            "BMI$",
            "AGE$",
            "CHARLSON_SCORE",
            "BENIGN_MALIGNANT",
            "MODALITY",
            ".+_TIME_MINS",
            ".+_DAYS",
            ".+_HOURS")
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
  
  numPats <- c("CHARLSON_SCORE",
               "^BMI$",
               "ASA_SCORE_NUMERIC",
               "PATIENT_AGE",
               ".+_TIME_MINS",
               ".+_DAYS",
               ".+_HOURS",
               "^YEAR$",
               "REVENUE",
               "COST",
               "MARGIN",
               "_CHARGES")
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
  rdata[, c("PATIENT_ID") := .GRP, "PATIENT_ID_DEIDENTIFIED"]
  set(rdata, NULL, "PATIENT_ID_DEIDENTIFIED", NULL)
  return(rdata[])
}
