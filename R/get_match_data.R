#' Match Data Using PSM
#' 
#' @param hospDT [ARGUMENT DEFINITION NEEDED]
#' @param stratby VARS TO STRATIFY THE DATA BY (PER REQUIREMENTS)
#' @param covariates [ARGUMENT DEFINITION NEEDED]
#' @param outcomes [ARGUMENT DEFINITION NEEDED]
#' 
#' @import data.table
#' @importFrom MatchIt get_matches matchit
#' 
#' @name Match_Data
NULL

#' @describeIn Match_Data Takes in data from getDataQTI and returns a list of data.tables
#' with each list element being a PSM matched dataset containing
#' @export
getMatchedData <- function(hospDT = NULL, 
                           stratby = NULL, 
                           covars = NULL, 
                           outcomes = NULL,
                           match_var = "Modality"){
  
  ## GET ALL ARGUMENTS FROM UNEVALUATED CALL
  arg_names <- pryr::fun_args(getMatchedData)
  call_list <- as.list(match.call())
  
  # NEEDED ID COLUMNS
  inc_vars  <- c("HospitalID", "PatientID")
  
  
  ##
  ## ROBUST ERROR CHECKING TO ENSURE DATA IS COMPLETE
  ##
  tryCatch({
    
    # not data.frame
    if( !is.data.frame(hospDT) )
      stop("hospDT is not of class data.frame or data.table")

    # get missing args (note they have not evaluated to NULL yet so they will be missing)
    args <- arg_names[ which( !arg_names %in% names(call_list) ) ]
    
    if(length(args) > 0){
      
      # set missing arguments as null in this environment
      args_list <- lapply(args, function(i){
        ll <- list(NULL)
        names(ll) <- i
        return(ll)
      })
      
      # complete the list of named arguments and eval them before sending to error check fun
      argList <- c(call_list, unlist(args_list, recursive = FALSE))[-1]
      argll <- lapply(argList, eval)
      
      # no NULL arguments allowed
      nullIndex <- sapply(argll, is.null, simplify = TRUE)
      if( any(nullIndex) )
        stop( paste0("missing arguments: ", paste0( names(nullIndex)[which(nullIndex)], collapse = ", " )) )
    }
    
    # ensure all columns needed are in the data and arguments are character class objects
    cols <- c(stratby, covars, outcomes, match_var)
    if( any( sapply(cols, function(i) class(i) != "character", simplify = TRUE) ))
      stop("invalid argument provided")
    
    missing_cols <- cols[!cols %in% colnames(hospDT)]
    if(length(missing_cols) > 0)
      stop( paste0("missing columns in data: ", paste0(missing_cols, collapse = ", ")))
    
    # ensure there are only two groups in the match_var column (expand later)
    if(hospDT[, .N, get(match_var)][, .N != 2])
      stop("Invalid number of modalities in the column named by 'match_var'")
    
    
    # Check if additional crucial columns are not present
    missing_ids <- inc_vars[ which(!inc_vars %in% colnames(hospDT)) ]
    if( length(missing_ids) > 0 )
      stop( paste0("dataset is missing the following id columns: ", paste0(missing_ids, collapse = ", ")) )

    
  }, error = function(c){
    
    stop(paste0(
      "Invalid data or arguments. Recieved the following error: ", "\n", 
      c$message
    ), call. = FALSE)
    
  })
  
  ##
  ## DATA VALIDATED ABOVE. CONTINUE SCRIPT
  ##
  keepCols  <- c(inc_vars, outcomes, stratby, match_var, covars)
  
  trimDT <- hospDT[, keepCols, with=FALSE]
  
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
  psmDT[, c("CID") := .GRP, c(stratby)]
  setkeyv(psmDT, "CID")
  
  # DATA CHECK: ENSURE BOTH MODALITIES, FOR EVERY STRATIFIED GROUP (COHORT), HAS
  #             AT LEAST 10 PATIENTS
  cDT <- psmDT[get("CID") %in% psmDT[, .N >= 10, c("CID", match_var)][, sum(get("V1"))>1, "CID"][, get("CID")[which(get("V1"))]]]
  
  if(nrow(cDT) == 0 | !is.data.table(cDT)){
    warning("NO STRATIFIED GROUPS HAVE 10 PATIENTS FOR BOTH MODALITIES", call. = FALSE)
    return(NULL)
  }

  cDT[, c("CID") := .GRP, stratby] # Regroup id since many were filtered
  setkeyv(cDT, c("CID"))
  cDT[, c("CID") := as.factor(get("CID"))] # set as factors
  
  # SPLIT INTO LISTS CONTAINING DATA FOR EACH STRATIFIED GROUP AND RUN PSM ANALYSIS ON EACH GROUP
  Sll <- split( cDT, cDT$CID )
  
  mList <- lapply(Sll, function(mDT){
    
    # TEST VARIATION WITH COVARS. IF NONE THEN DROP. REPLACE WITH PCA SOON
    getValidCovars <- function(i) i[length(unique(mDT[!is.na(get(i)), get(i)])) > 1]
    new_covars     <- unlist(lapply(covars, getValidCovars))
    
    # SET MODALITY WITH MORE OBSERVATIONS AS THE CONTROL GROUP
    control  <- mDT[, .N, match_var][which.max(get("N")), get(match_var)]
    
    # TRIM DATA BASED ON A SUBSET OF THE COVARS (THOSE THAT VARY)
    keepCols <- c(inc_vars, outcomes, stratby, match_var, new_covars)
    psdata   <- mDT[, keepCols, with = FALSE]
    
    # LABEL THE MODALITY THAT'S NOT THE CONTROL WITH A BOOLEAN FLAG
    psdata[, c("IsTreatment") := get(match_var) != control]
    
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
  setkeyv(resDT, c("CID", match_var))
  return(resDT)
}