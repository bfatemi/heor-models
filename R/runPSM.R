#' Run Full Analysis with PSM Matching
#'
#' @param hospID A numeric variable representing the hospital id
#' 
#' @return [DOCUMENT RETURN VALUE]
#' @export
#'
#' @import data.table
#' @import RODBC
#' @import stringr
#' 
runPSM <- function(hospID = NULL){
  
  # COLUMN GROUPINGS
  idcols   <- c("HospitalID", "HospitalName", "PatientIDDeidentified", "Modality")
  strat    <- c("ProcedurePrimary", "BenignMalignant", "InpatientOutpatient")
  covars   <- c("PatientBMI", "PatientAge", "PatientCharlsonScore", "PatientGender")
  outcomes <- c("LOSDays", "ORTimeMins")
  
  # GET DATA
  DT <- getDataQTI(hospID, c(idcols, strat, covars, outcomes))
  
  num_cols <- c("PatientBMI", "PatientAge", "LOSDays", "ORTimeMins")
  fac_cols <- c("Modality", "PatientCharlsonScore", "PatientGender", "InpatientOutpatient", "BenignMalignant")
  
  for(n in num_cols)
    set(DT, i=NULL, j = n, value = DT[, as.numeric(get(n))])
  for(f in fac_cols)
    set(DT, i=NULL, j = f, value = DT[, as.factor(get(f))])
  
  if(nrow(DT) == 0) 
    stop("No data", call. = FALSE)
  
  setnames(DT, "PatientIDDeidentified", "PatientID")
  
  ## SPLIT DATA INTO LIST BY HOSPITAL
  hospList <- split(DT, DT[, get("HospitalID")])
  
  
  ## DATA VALIDATION OF EACH HOSP
  validHosp <- sapply(hospList, function(hdt){
    x <- table(hdt$Modality)
    return(x[['Open']] > 10 & x[['Robotic']] > 10)
  })
  
  
  dropHosp <- names(which(!validHosp))
  if(length(dropHosp) > 0)
    warning("Not enough data for both modalities for hospitals: ", paste0(dropHosp, collapse = ", "))
  
  
  ##
  ## Run analysis for each hospital
  ##
  psmDataList <- lapply( hospList[which(validHosp)], function(hDT){
    
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
  nDT <- rbindlist(psmDataList)
  htable <- DT[, .N, c("HospitalID", "HospitalName")][, !"N"]
  
  outDT <- htable[nDT, on = "HospitalID"]
  
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