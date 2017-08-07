#' Run Full Analysis with PSM Matching
#'
#' @param hospID A numeric variable representing the hospital id
#' @param modal [DOC NEEDED]
#' @return [DOCUMENT RETURN VALUE]
#' @export
#'
#' @import data.table
runPSM <- function(hospID = NULL, modal = "Open"){
  qDT <- getDataQTI(hospID = hospID)[Modality %in% c("Robotic", modal)]
  
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
                "ORTimeMins")
  
  ##
  ## Run analysis for each hospital
  ##
  # count <- 2
  psmDataList <- lapply(hospList, function(hDT){
    # hDT <- hospList[[count]]
    
    # If each modality does not have at least 10 obs each, then return null for this hosp
    if(length(hDT[, .N, Modality][N > 10, Modality]) < 2){
      warning("Not enough data for comparison of modalities for HospitalID: ", hDT[, unique(HospitalID)], call. = FALSE)
      return(NULL)
    }
    
    DT <- psm_data(DT = hDT, 
                   strat_vars = strat, 
                   covariates = covars, 
                   outcome_vars = outcomes)
    
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
  # setnames(outDT, c("CLow", "CHigh"), c("CLOW", "CHIGH"))
  # setnames(outDT, c("T_STAT", "P_VAL"), c("TSTAT", "PVAL"))
  setnames(outDT, "PSMCount", "N")
  
  ## BEGIN TRANSFORMATION OF DATA
  stat_cols <- c("N", "Mean", "Median", "STD", "Var", "TStat", "PValue", "CLow", "CHigh")
  
  
  env <- caller_env()
  y <- paste0(colnames(outDT)[ !colnames(outDT) %in% c(stat_cols, "outcome") ], collapse = " + ")
  x <- "outcome"
  f.expr <- paste0(y, " ~ ", x)
  
  
  woutDT <- dcast.data.table(data = outDT, 
                             sep = "_", 
                             formula = as.formula(f.expr, env), 
                             value.var = stat_cols, 
                             fill = NA)
  return(woutDT)
}