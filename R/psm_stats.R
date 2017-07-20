#' Test Difference of Means with Balancing
#' 
#' [DESCRIPTION OF FUNCTION NEEDED]
#'
#' @param DT [ARGUMENT DEFINITION NEEDED]
#' @param strat_vars [ARGUMENT DEFINITION NEEDED]
#' @param covariates [ARGUMENT DEFINITION NEEDED]
#' @param outcome_vars [ARGUMENT DEFINITION NEEDED]
#'
#' @return [DESCRIPTION OF OUTPUT NEEDED]
#' 
#' @import stats
#' @import data.table
#' 
#' @name PSM_Analysis
NULL

#' @describeIn PSM_Analysis Function that takes the outcome of \code{getDataPSM} and 
#' runs statistical summary functions and performs a paired t-test across the psm matched
#' groups
#' @export
get_stats <- function(DT, outcome_vars){
  rbindlist(lapply(outcome_vars, function(o){
    FUNS <- list(
      PSM_COUNT = length,
      MEAN      = mean,
      MEDIAN    = median,
      STD       = sd,
      VAR       = var
    )
    sList <- split(DT, by = c("CID", "MODALITY"), flatten = FALSE, drop = TRUE)
    rbindlist(lapply(sList, function(i){
      modals <- names(i)
      
      x <- i[[modals[1]]][, get(o)]
      y <- i[[modals[2]]][, get(o)]
      t.ml <- t.test(x, y)
      
      cbind(
        outcome = o,
        rbindlist(i)[, lapply(FUNS, function(fn) fn(get(o))), c("CID", "MODALITY")],
        data.table(T_STAT = t.ml$statistic, 
                   P_VAL = t.ml$p.value,
                   C_INT_L = t.ml$conf.int[1],
                   C_INT_H = t.ml$conf.int[2])
      )
    }))
  }))
}

#' @describeIn PSM_Analysis Takes in data from getDataQTI and returns a list of data.tables
#' with each list element being a PSM matched dataset containing
#' @export
psm_data <- function(DT = NULL, 
                     strat_vars = NULL, 
                     covariates = NULL, 
                     outcome_vars = NULL){
  
  
  treat_var <- "MODALITY" # hardcode for now
  
  if(!"Robotic" %in% DT[, unique(get(treat_var))])
    stop("No Robotic modality observations detected in the treatment column", call. = FALSE)
  
  inc_vars <- c("HOSPITAL_ID", "PID") # include additional in output
  keepCols <- c(inc_vars, outcome_vars, strat_vars, treat_var, covariates) # Group output cols
  
  ## TRIM THE DATASET AND REMOVE ANY ROWS WITH NA IN THESE COLUMNS. PSM NEEDS COMPLETE DATA
  trimDT <- DT[, keepCols, with=FALSE]
  psmDT <- trimDT[
    apply(X = trimDT[, lapply(.SD, FUN = function(col) is.na(col))], 
          MARGIN = 1, 
          FUN = function(j) !any(j))
    ]
  
  psmDT[, c("CID") := .GRP, c(strat_vars)] # stratify and label cohorts
  setkeyv(psmDT, "CID")
  
  # count patients for each modality and get all modality pairs that have at least 10
  cDT <- psmDT[CID %in% psmDT[, .N >= 10, c("CID", treat_var)][, sum(V1)>1, "CID"][, CID[which(V1)]]]
  
  if(nrow(cDT) == 0)
    stop("no modality pairs with at least 10 patients each", call. = FALSE)
  
  cDT[, CID := .GRP, strat_vars] # Regroup id since many were filtered
  setkey(cDT, CID)
  cDT[, CID := as.factor(CID)] # set as factors
  
  
  # Split each cohort and run psm analysis
  mList <- lapply(split( cDT, cDT$CID ), function(mDT){
    
    ## TEST VARIATION WITH COVARS. IF NONE THEN DROP. REPLACE WITH PCA SOON
    new_covars <- unlist(lapply(covariates, function(i) i[length(unique(mDT[!is.na(get(i)), get(i)])) > 1]))
    control    <- mDT[, .N, treat_var][which.max(N), get(treat_var)]
    
    keepCols <- c(inc_vars, outcome_vars, strat_vars, treat_var, new_covars)
    psdata <- mDT[, keepCols, with = FALSE]
    
    psdata[, IS_TREATMENT := get(treat_var) != control]
    
    indNA <- which(Reduce(`|`, lapply(psdata, function(i) is.na(i))))
    if(length(indNA) == 0){
      cleanDT <- psdata
    }else{
      cleanDT <- psdata[ -indNA ]
    }
    
    env <- rlang::caller_env()
    tryCatch({
      f.expr <- rlang::parse_expr(paste0("IS_TREATMENT ~ ", paste0(new_covars, collapse = " + ")))
      
      ml.psm <- MatchIt::matchit(data = cleanDT, 
                                 formula = as.formula(f.expr, env),
                                 method = "nearest", 
                                 ratio = 1, 
                                 distance = "logit")
      matched_data <- cbind(
        CID = mDT[, unique(CID)],
        as.data.table(MatchIt::get_matches(ml.psm, cleanDT))
      )
      return(matched_data)
      
    }, error = function(c){
      if(stringr::str_detect(c$message, "No units were matched"))
        return(NULL)
    })
  })
  
  resDT <- rbindlist(mList, fill = TRUE)
  setkeyv(resDT, c("CID", treat_var))
  return(resDT)
  
}
