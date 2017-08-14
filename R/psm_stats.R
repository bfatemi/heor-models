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

#' @describeIn PSM_Analysis Takes in data from getDataQTI and returns a list of data.tables
#' with each list element being a PSM matched dataset containing
#' @export
psm_data <- function(DT = NULL, 
                     strat_vars = NULL, 
                     covariates = NULL, 
                     outcome_vars = NULL){
  
  
  treat_var <- "Modality" # hardcode for now
  
  if(!"Robotic" %in% DT[, unique(get(treat_var))])
    stop("No Robotic modality observations detected in the treatment column", call. = FALSE)
  
  # include additional in output
  inc_vars <- c("HospitalID", "PatientID")
  
  # Group output cols
  keepCols <- c(inc_vars, 
                outcome_vars, 
                strat_vars, 
                treat_var, 
                covariates)
  
  ## PSM NEEDS COMPLETE DATA
  #   - TRIM THE DATASET AND REMOVE ANY ROWS WITH NA IN THESE COLUMNS
  #
  trimDT <- DT[, keepCols, with=FALSE]
  psmDT <- trimDT[
    apply(X = trimDT[, lapply(.SD, FUN = function(col) is.na(col))], 
          MARGIN = 1, 
          FUN = function(j) !any(j))
    ]
  
  if(nrow(psmDT) == 0){
    warning("No non-na observations in dataset (all rows contain NA. Check outcome measure)", call. = FALSE)
    return(NULL)
  }
  
  psmDT[, c("CID") := .GRP, c(strat_vars)] # stratify and label cohorts
  setkeyv(psmDT, "CID")
  
  # count patients for each modality and get all modality pairs that have at least 10
  cDT <- psmDT[CID %in% psmDT[, .N >= 10, c("CID", treat_var)][, sum(V1)>1, "CID"][, CID[which(V1)]]]
  
  if(nrow(cDT) == 0){
    warning("no modality pairs with at least 10 patients each", call. = FALSE)
    return(NULL)
  }
  
  
  cDT[, CID := .GRP, strat_vars] # Regroup id since many were filtered
  setkey(cDT, CID)
  cDT[, CID := as.factor(CID)] # set as factors
  
  
  # Split each cohort and run psm analysis
  ll <- split( cDT, cDT$CID )
  
  mList <- lapply(ll, function(mDT){
    ## TEST VARIATION WITH COVARS. IF NONE THEN DROP. REPLACE WITH PCA SOON
    new_covars <- unlist(lapply(covariates, function(i) i[length(unique(mDT[!is.na(get(i)), get(i)])) > 1]))
    control <- mDT[, .N, treat_var][which.max(N), get(treat_var)]
    
    keepCols <- c(inc_vars, outcome_vars, strat_vars, treat_var, new_covars)
    psdata <- mDT[, keepCols, with = FALSE]
    
    psdata[, c("IsTreatment") := get(treat_var) != control]
    
    # remove any rows that has an NA observation
    cleanDT <- psdata[ !Reduce(`|`, lapply(psdata, function(i) is.na(i))) ]
    
    
    env <- caller_env()
    
    matched_data <- tryCatch({
      f.expr <- paste0("IsTreatment ~ ", paste0(new_covars, collapse = " + "))
      
      ml.psm <- MatchIt::matchit(data = cleanDT, 
                                 formula = as.formula(f.expr, env),
                                 method = "nearest", 
                                 ratio = 1, 
                                 distance = "logit")
      cbind(
        CID = mDT[, unique(CID)],
        as.data.table(MatchIt::get_matches(ml.psm, cleanDT))
      )
      
      
    }, error = function(c){
      if(stringr::str_detect(c$message, "No units were matched"))
        return(NULL)
    })
  })
  
  resDT <- rbindlist(mList, fill = TRUE)
  setkeyv(resDT, c("CID", treat_var))
  return(resDT)
}
