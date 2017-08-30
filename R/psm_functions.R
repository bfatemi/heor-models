#' Run Full Analysis with PSM Matching
#' 
#' Use runPSM() to execute entire workflow. Alternatively utilize modular functions for
#' an interactive execution of this PSM analysis
#'
#' @param hospID A numeric variable representing the hospital id
#' @param DT [ARGUMENT DEFINITION NEEDED]
#' @param strat_vars VARS TO STRATIFY THE DATA BY (PER REQUIREMENTS)
#' @param covariates [ARGUMENT DEFINITION NEEDED]
#' @param outcome_vars [ARGUMENT DEFINITION NEEDED]
#'
#' @import RODBC
#' @import stats
#' @import data.table
#' @importFrom stringr str_split_fixed str_detect str_c
#' @importFrom MatchIt get_matches matchit
#' 
#' @name PSM_Functions
NULL


#' @describeIn PSM_Functions Execute entire workflow
#' @export
runPSM <- function(hospID = NULL){
  
  ##
  ## Query and clean data
  ##
  idcols   <- c("HospitalID", "HospitalName", "PatientIDDeidentified", "Modality")
  strat    <- c("ProcedurePrimary", "BenignMalignant", "InpatientOutpatient")
  covars   <- c("PatientBMI", "PatientAge", "PatientCharlsonScore", "PatientGender")
  outcomes <- c("LOSDays", "ORTimeMins")
  
  DT <- getDataQTI(hospID, c(idcols, strat, covars, outcomes))
  
  # set appropriate col classes
  num_cols <- c("PatientBMI", "PatientAge", "LOSDays", "ORTimeMins")
  fac_cols <- c("Modality", "PatientCharlsonScore", "PatientGender", "InpatientOutpatient", "BenignMalignant")
  
  for(n in num_cols)
    set(DT, i=NULL, j = n, value = DT[, as.numeric(get(n))])
  for(f in fac_cols)
    set(DT, i=NULL, j = f, value = DT[, as.factor(get(f))])
  
  if(nrow(DT) == 0) 
    stop("No data", call. = FALSE)
  
  setnames(DT, "PatientIDDeidentified", "PatientID")
  
  
  ##
  ## DROP HOSPITALS WITH FEWER THAN 10 OPEN AND 10 ROBOTIC CASES
  ##
  
  hospList <- split(DT, DT[, get("HospitalID")])
  
  validHosp <- sapply(hospList, function(hdt){
    x <- table(hdt$Modality)
    return(x[['Open']] > 10 & x[['Robotic']] > 10)
  })
  
  dropHosp <- names(which(!validHosp))
  if(length(dropHosp) > 0)
    warning("Not enough data for both modalities for hospitals: ", paste0(dropHosp, collapse = ", "))
  
  psmInput <- hospList[which(validHosp)]
  
  
  ##
  ## Run analysis for each hospital
  ##
  psmDataList <- lapply(psmInput, function(hDT){
    
    # update oucomes as some may be all NAs
    outcomes <- names(which(apply(hDT[, outcomes, with=FALSE], 2, function(col) sum(!is.na(col)) > 0)))
    
    if(length(outcomes) == 0){
      warning("All selected outcome variables have only NA values", call. = FALSE)
      return(NULL)
    }
    
    DT <- getMatchedData(DT = hDT, 
                         strat_vars = strat, 
                         covariates = covars, 
                         outcome_vars = outcomes)
    
    if(is.null(DT))
      return(NULL)
    
    # get result and merge back with descriptor columns needed
    res <- getStats(DT = DT, outcome_vars = outcomes)
    
    dtable <- DT[, list(PSMCount = .N), c("CID", 
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
  nDT    <- rbindlist(psmDataList)
  htable <- DT[, .N, c("HospitalID", "HospitalName")][, !"N"]
  outDT  <- htable[nDT, on = "HospitalID"]
  
  ##
  ## TRANSFORM DATA PER REQUIREMENTS
  ##
  
  
  setnames(outDT, "PSMCount", "N") # change names for convenience
  
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
  cnams <- names(woutDT)[str_detect(names(woutDT), "_")]
  new_nams <- apply(str_split_fixed(cnams, "_", 2)[, c(2,1)], 1, str_c, collapse = "")
  
  setnames(woutDT, cnams, new_nams)
  setnames(woutDT, "N", "PSMCount") # quick fix to change back name per requirement
  return(woutDT[])
}


#' @describeIn PSM_Functions Takes in data from getDataQTI and returns a list of data.tables
#' with each list element being a PSM matched dataset containing
#' @export
getMatchedData <- function(DT = NULL, 
                           strat_vars = NULL, 
                           covariates = NULL, 
                           outcome_vars = NULL){
  
  ##
  ## TO-DO: TURN WARNINGS TO ERRORS AND IMPLEMENT TRYCATCH IN CALLER
  ##
  
  ##
  ## TRIM THE DATASET
  ##
  treat_var <- c("Modality")                  # hardcode for now
  inc_vars  <- c("HospitalID", "PatientID")   # include additional in output
  keepCols  <- c(inc_vars, outcome_vars, strat_vars, treat_var, covariates)
  
  trimDT <- DT[, keepCols, with=FALSE]
  
  ##
  ## PSM NEEDS COMPLETE DATA - REMOVE ANY ROWS WITH NA IN THESE COLUMNS
  ##
  psmDT <- trimDT[
    apply(X = trimDT[, lapply(.SD, FUN = function(col) is.na(col))], 
          MARGIN = 1, 
          FUN = function(j) !any(j))
    ]
  
  ##
  ## DATA CHECK AFTER NA ROW CLEANING
  ##
  if(nrow(psmDT) == 0){
    warning("No non-na observations in dataset (all rows contain NA. Check outcome measure)", call. = FALSE)
    return(NULL)
  }
  
  ##
  ## STRATIFY THE DATA BASED ON REQUIREMENTS
  ##
  psmDT[, c("CID") := .GRP, c(strat_vars)]
  setkeyv(psmDT, "CID")
  
  
  ##
  ## DATA CHECK: ENSURE BOTH MODALITIES, FOR EVERY STRATIFIED GROUP (COHORT), HAS
  ##             AT LEAST 10 PATIENTS
  ##
  cDT <- psmDT[get("CID") %in% psmDT[, .N >= 10, c("CID", treat_var)][, sum(get("V1"))>1, "CID"][, get("CID")[which(get("V1"))]]]
  
  if(nrow(cDT) == 0 | !is.data.table(cDT)){
    warning("NO STRATIFIED GROUPS HAVE 10 PATIENTS FOR BOTH MODALITIES", call. = FALSE)
    return(NULL)
  }
  
  
  cDT[, c("CID") := .GRP, strat_vars] # Regroup id since many were filtered
  setkeyv(cDT, c("CID"))
  cDT[, c("CID") := as.factor(get("CID"))] # set as factors
  
  
  ##
  ## SPLIT INTO LISTS CONTAINING DATA FOR EACH STRATIFIED GROUP
  ##  AND RUN PSM ANALYSIS ON EACH GROUP
  ##
  Sll <- split( cDT, cDT$CID )
  
  mList <- lapply(Sll, function(mDT){
    
    ##
    ## TEST VARIATION WITH COVARS. IF NONE THEN DROP. REPLACE WITH PCA SOON
    ##
    getValidCovars <- function(i) i[length(unique(mDT[!is.na(get(i)), get(i)])) > 1]
    new_covars     <- unlist(lapply(covariates, getValidCovars))
    
    ##
    ## SET MODALITY WITH MORE OBSERVATIONS AS THE CONTROL GROUP
    ##
    control  <- mDT[, .N, treat_var][which.max(get("N")), get(treat_var)]
    
    ##
    ## TRIM DATA BASED ON A SUBSET OF THE COVARS (THOSE THAT VARY)
    ##
    keepCols <- c(inc_vars, outcome_vars, strat_vars, treat_var, new_covars)
    psdata   <- mDT[, keepCols, with = FALSE]
    
    ##
    ## LABEL THE MODALITY THAT'S NOT THE CONTROL WITH A BOOLEAN FLAG
    ##
    psdata[, c("IsTreatment") := get(treat_var) != control]
    
    
    ##
    ## DATA CHECK: NOTE THIS IS POTENTIALLY REDUNDANT SINCE WE DID AN EARLIER CHECK
    ##
    cleanDT <- psdata[ !Reduce(`|`, lapply(psdata, function(i) is.na(i))) ]
    
    
    ##
    ## EXECUTE PROPENSITY SCORE MATCHING AND RETURN MATCHED DATASET
    ##
    env <- caller_env()
    matched_data <- tryCatch({
      
      ##
      ## WITHIN THIS STRATIFIED GROUP, MATCH BASED ON PROBABILITY THAT 
      ## COVARIATES PREDICT TREATMENT GROUP LABEL
      ##
      f.expr <- paste0("IsTreatment ~ ", paste0(new_covars, collapse = " + "))
      
      ##
      ## SPECIFY NEAREST NEIGHBOR MATCHING ALGORITHM 
      ##
      ml.psm <- MatchIt::matchit(data = cleanDT, 
                                 formula = as.formula(f.expr, env),
                                 method = "nearest", 
                                 ratio = 1, 
                                 distance = "logit")
      
      ##
      ## GET MATCHES AND APPEND THE COHORT ID AS AN ENTIRE COLUMN FOR WHEN WE 
      ## ROWBIND ALL THE RESULTS IN THE FINAL STEP, WE CAN PRESERVE GROUPING ID
      ##
      cbind(
        CID = mDT[, unique(get("CID"))],
        as.data.table(MatchIt::get_matches(ml.psm, cleanDT))
      )
      
    }, error = function(c){
      
      ##
      ## IF NO UNITS WHERE MATCHED WITHIN THIS STRATIFIED GROUP, THEN RETURN NULL
      ##
      if(stringr::str_detect(c$message, "No units were matched"))
        return(NULL)
    })
  })
  
  ##
  ## BIND THE RESULTS AND SET THE KEY FOR ORDERING (READABILITY)
  ##
  resDT <- rbindlist(mList, fill = TRUE)
  setkeyv(resDT, c("CID", treat_var))
  return(resDT)
}


#' @describeIn PSM_Functions Function that takes the outcome of \code{getMatchedData} and 
#' runs statistical summary functions and performs a paired t-test across the psm matched
#' groups
#' @export
getStats <- function(DT, outcome_vars){
  rbindlist(lapply(outcome_vars, function(o){
    FUNS <- list(
      PSMCount = length,
      Mean      = mean,
      Median    = median,
      STD       = sd,
      Var       = var
    )
    sList <- split(DT, by = c("CID", "Modality"), flatten = FALSE, drop = TRUE)
    rbindlist(lapply(sList, function(i){
      modals <- names(i)
      
      x <- i[[modals[1]]][, get(o)]
      y <- i[[modals[2]]][, get(o)]
      t.ml <- t.test(x, y)
      
      cbind(
        outcome = o,
        rbindlist(i)[, lapply(FUNS, function(fn) fn(get(o))), c("CID", "Modality")],
        data.table(TStat = t.ml$statistic, 
                   PValue = t.ml$p.value,
                   CLow = t.ml$conf.int[1],
                   CHigh = t.ml$conf.int[2])
      )
    }))
  }))
}


