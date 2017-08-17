#' Run Full Analysis with PSM Matching
#'
#' @param hospID A numeric variable representing the hospital id
#' @param modal [DOC NEEDED]
#' @param qDT optionally, for testing purposes you can supply the queried dataset
#' @return [DOCUMENT RETURN VALUE]
#' @export
#'
#' @import data.table
runPSM <- function(hospID = NULL, modal = "Open", qDT = NULL){
  if(is.null(qDT)){
    qDT <- getDataQTI(hospID = hospID)[Modality %in% c("Robotic", modal)]  
  }
  
  if(nrow(qDT) == 0) 
    stop("No data for modality and/or hosp id", call. = FALSE)
  
  hospList <- split(qDT, qDT[, get("HospitalID")])
  
  strat    <- c("ProcedurePrimary", 
                "BenignMalignant", 
                "InpatientOutpatient")
  covars   <- c("PatientBMI", 
                "PatientAge", 
                "PatientCharlsonScore", 
                "PatientGender")
  outcomes <- c("LOSHours",
                "LOSDays",
                "SurgeryTimeMins",
                "ORTimeMins")
  
  
  ##
  ## Run analysis for each hospital
  ##
  psmDataList <- lapply(hospList, function(hDT){
    
    # If each modality does not have at least 10 obs each, then return null for this hosp
    if(length(hDT[, .N, Modality][N > 10, Modality]) < 2){
      warning("Not enough data for comparison of modalities for HospitalID: ", hDT[, unique(HospitalID)], call. = FALSE)
      return(NULL)
    }
    
    # update oucomes as some may be all NAs
    outcomes <- names(which(apply(hDT[, outcomes, with=FALSE], 2, function(col) sum(!is.na(col)) > 0)))
    
    if(length(outcomes) == 0){
      warning("All selected outcome variables have only NA values", call. = FALSE)
      return(NULL)
    }
    
    DT <- psm_data(DT = hDT, 
                   strat_vars = strat, 
                   covariates = covars, 
                   outcome_vars = outcomes)
    if(is.null(DT))
      return(NULL)
    
    # get result and merge back with descriptor columns needed
    res <- get_stats(DT = DT, outcome_vars = outcomes)
    
    dtable <- DT[, .(PSMCount = .N), c("CID", 
                                       "Modality",
                                       "HospitalID", 
                                       "ProcedurePrimary", 
                                       "BenignMalignant", 
                                       "InpatientOutpatient")]
    
    setkeyv(dtable, c("CID", "PSMCount", "Modality"))
    setkeyv(res,    c("CID", "PSMCount", "Modality"))
    return(res[dtable])
  })
  
  
  # Bind together all results for individual hospitals, then join to add hosp name
  DT <- rbindlist(psmDataList)
  htable <- qDT[, .N, c("HospitalID", "HospitalName")][, !"N"]
  
  outDT <- htable[DT, on = "HospitalID"]
  
  # change names for convenience
  setnames(outDT, "PSMCount", "N")
  
  ## BEGIN TRANSFORMATION OF DATA
  stat_cols <- c("Mean", "Median", "STD", "Var", "TStat", "PValue", "CLow", "CHigh")
  
  
  env <- caller_env()
  y <- paste0(colnames(outDT)[ !colnames(outDT) %in% c(stat_cols, "outcome") ], collapse = " + ")
  x <- "outcome"
  f.expr <- paste0(y, " ~ ", x)
  
  
  woutDT <- dcast.data.table(data = outDT, 
                             sep = "_", 
                             formula = as.formula(f.expr, env), 
                             value.var = stat_cols, 
                             fill = NA)
  
  ## REQUEST TO CHANGE COLUMNS
  cnams <- names(woutDT)[stringr::str_detect(names(woutDT), "_")]
  new_nams <- apply(stringr::str_split_fixed(cnams, "_", 2)[, c(2,1)], 1, stringr::str_c, collapse = "")
  setnames(woutDT, cnams, new_nams)
  
  setnames(woutDT, "N", "PSMCount")
  return(woutDT[])
}