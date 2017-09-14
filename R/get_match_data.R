#' Match Data Using PSM
#' 
#' @param DT [ARGUMENT DEFINITION NEEDED]
#' @param strat_vars VARS TO STRATIFY THE DATA BY (PER REQUIREMENTS)
#' @param covariates [ARGUMENT DEFINITION NEEDED]
#' @param outcome_vars [ARGUMENT DEFINITION NEEDED]
#' 
#' @import data.table
#' @importFrom MatchIt get_matches matchit
#' 
#' @name Match_Data
NULL

#' @describeIn Match_Data Takes in data from getDataQTI and returns a list of data.tables
#' with each list element being a PSM matched dataset containing
#' @export
getMatchedData <- function(DT = NULL, 
                           strat_vars = NULL, 
                           covariates = NULL, 
                           outcome_vars = NULL){
  
  ## TO-DO: TURN WARNINGS TO ERRORS AND IMPLEMENT TRYCATCH IN CALLER
  
  # TRIM THE DATASET
  treat_var <- c("Modality")                  # hardcode for now
  inc_vars  <- c("HospitalID", "PatientID")   # include additional in output
  keepCols  <- c(inc_vars, outcome_vars, strat_vars, treat_var, covariates)
  
  trimDT <- DT[, keepCols, with=FALSE]
  
  # PSM NEEDS COMPLETE DATA - REMOVE ANY ROWS WITH NA IN THESE COLUMNS
  psmDT <- trimDT[
    apply(X = trimDT[, lapply(.SD, FUN = function(col) is.na(col))], 
          MARGIN = 1, 
          FUN = function(j) !any(j))
    ]
  
  # DATA CHECK AFTER NA ROW CLEANING
  if(nrow(psmDT) == 0){
    warning("No non-na observations in dataset (all rows contain NA. Check outcome measure)", call. = FALSE)
    return(NULL)
  }
  
  # STRATIFY THE DATA BASED ON REQUIREMENTS
  psmDT[, c("CID") := .GRP, c(strat_vars)]
  setkeyv(psmDT, "CID")
  
  # DATA CHECK: ENSURE BOTH MODALITIES, FOR EVERY STRATIFIED GROUP (COHORT), HAS
  #             AT LEAST 10 PATIENTS
  cDT <- psmDT[get("CID") %in% psmDT[, .N >= 10, c("CID", treat_var)][, sum(get("V1"))>1, "CID"][, get("CID")[which(get("V1"))]]]
  
  if(nrow(cDT) == 0 | !is.data.table(cDT)){
    warning("NO STRATIFIED GROUPS HAVE 10 PATIENTS FOR BOTH MODALITIES", call. = FALSE)
    return(NULL)
  }
  
  
  cDT[, c("CID") := .GRP, strat_vars] # Regroup id since many were filtered
  setkeyv(cDT, c("CID"))
  cDT[, c("CID") := as.factor(get("CID"))] # set as factors
  
  # SPLIT INTO LISTS CONTAINING DATA FOR EACH STRATIFIED GROUP AND RUN PSM ANALYSIS ON EACH GROUP
  Sll <- split( cDT, cDT$CID )
  
  mList <- lapply(Sll, function(mDT){
    
    # TEST VARIATION WITH COVARS. IF NONE THEN DROP. REPLACE WITH PCA SOON
    getValidCovars <- function(i) i[length(unique(mDT[!is.na(get(i)), get(i)])) > 1]
    new_covars     <- unlist(lapply(covariates, getValidCovars))
    
    # SET MODALITY WITH MORE OBSERVATIONS AS THE CONTROL GROUP
    control  <- mDT[, .N, treat_var][which.max(get("N")), get(treat_var)]
    
    # TRIM DATA BASED ON A SUBSET OF THE COVARS (THOSE THAT VARY)
    keepCols <- c(inc_vars, outcome_vars, strat_vars, treat_var, new_covars)
    psdata   <- mDT[, keepCols, with = FALSE]
    
    # LABEL THE MODALITY THAT'S NOT THE CONTROL WITH A BOOLEAN FLAG
    psdata[, c("IsTreatment") := get(treat_var) != control]
    
    # DATA CHECK: NOTE THIS IS POTENTIALLY REDUNDANT SINCE WE DID AN EARLIER CHECK
    cleanDT <- psdata[ !Reduce(`|`, lapply(psdata, function(i) is.na(i))) ]
    
    # EXECUTE PROPENSITY SCORE MATCHING AND RETURN MATCHED DATASET
    env <- caller_env()
    matched_data <- tryCatch({
      
      # WITHIN THIS STRATIFIED GROUP, MATCH BASED ON PROBABILITY THAT 
      # COVARIATES PREDICT TREATMENT GROUP LABEL
      f.expr <- paste0("IsTreatment ~ ", paste0(new_covars, collapse = " + "))
      
      # SPECIFY NEAREST NEIGHBOR MATCHING ALGORITHM
      ml.psm <- MatchIt::matchit(data = cleanDT, 
                                 formula = as.formula(f.expr, env),
                                 method = "nearest", 
                                 ratio = 1, 
                                 distance = "logit")
      
      # GET MATCHES AND APPEND THE COHORT ID AS AN ENTIRE COLUMN FOR WHEN WE 
      # ROWBIND ALL THE RESULTS IN THE FINAL STEP, WE CAN PRESERVE GROUPING ID
      cbind(
        CID = mDT[, unique(get("CID"))],
        as.data.table(MatchIt::get_matches(ml.psm, cleanDT))
      )
      
    }, error = function(c){
      
      # IF NO UNITS WHERE MATCHED WITHIN THIS STRATIFIED GROUP, THEN RETURN NULL
      if(stringr::str_detect(c$message, "No units were matched"))
        return(NULL)
    })
  })
  
  # BIND THE RESULTS AND SET THE KEY FOR ORDERING (READABILITY)
  resDT <- rbindlist(mList, fill = TRUE)
  setkeyv(resDT, c("CID", treat_var))
  return(resDT)
}