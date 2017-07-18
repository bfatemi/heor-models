#' Test Difference of Means with Balancing
#' 
#' Utilizes PSM to balance the groups. Model specified with the \code{treatment}, \code{covars},
#' and \code{outcome} arguments. The \code{split_var} argument is used for stratifying the data
#' prior to PSM matching.
#'
#' @param DT Data set that meets IS specifications
#' @param treatment Name of the column that represents assignment of treatment or control group
#' @param split_var Variables use to stratify observations into groups
#' @param covars A character vector naming the columns that represent the covariates
#' @param outcome The name of the column that represents the outcome variable
#'
#' @return A data.table of statistics
#' 
#' @import stats
#' @import data.table
#' @export
getStatPSM <- function(DT, treatment, split_var, covars, outcome){
  
  # CONSTRUCT MODEL DATA AND FORMULA, THEN GET MODEL AND CALC PROPENSITY SCORES
  keepCols <- c("HOSPITAL_ID", "PID", outcome, split_var, treatment, covars)
  psmDT <- DT[, keepCols, with=FALSE]
  psmDT[, CID := .GRP, c(split_var)]
  psmDT[, CID := as.factor(CID)]
  
  setkeyv(psmDT, "CID")
  tmp <- dcast(psmDT[, .N < 10, c("CID", treatment)][V1 == FALSE, !"V1"], 
               CID ~ MODALITY, 
               value.var = "MODALITY",
               fun.aggregate = length)
  cDT <- psmDT[CID %in% as.numeric(tmp[which(rowSums(tmp[, !"CID"]) == 2), CID])]
  
  
  ll <- split(cDT, cDT$CID)
  mList <- ll[sapply(ll, nrow) > 0]
  
  
  RESULT <- rbindlist(lapply(mList, function(mDT){
    
    mDT[, IS_ROBOTIC := MODALITY == "Robotic"]
    
    # if covariates don't vary, need to remove them. If not left, PSM shouldn't be applied
    valid_covars <- covars[sapply(covars, function(i) length(unique(mDT[, get(i)]))) > 1]
    if(length(valid_covars) == 0)
      return(NULL)
    
    ## GET MODEL EQUATION FOR EACH RUN
    env <- rlang::caller_env()
    
    f <- rlang::new_formula(
      rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(valid_covars, collapse = " + "))),
      lhs = as.name("IS_ROBOTIC"),
      env = env
    )
    
    #
    # matchit spits out an error if ANY columns have missing values, including the columns
    # that are not even relevant. To work around, reduce the dataset to only the relevant
    # columns for each run
    #
    outList <- lapply(outcome, function(o){
      # o <- outcome[[1]]
      
      psm_data <- mDT[, c("IS_ROBOTIC", valid_covars, o), with=FALSE]
      
      psmll <- tryCatch({
        psm.ml    <- MatchIt::matchit(formula = f, mDT, method = "nearest", ratio = 1)
        
        index_rob <- as.numeric(row.names(psm.ml$match.matrix))
        index_ctr <- as.numeric(psm.ml$match.matrix[, 1])
        
        list(
          r = mDT[ index_rob ],
          c = mDT[ index_ctr ]
        )
        
      }, warning = function(c){
        
        if(stringr::str_detect(c$message, "Fewer control than treated units")){
          
          
          mDT[, IS_CONTROL := !IS_ROBOTIC]
          
          f <- rlang::new_formula(
            rhs = lazyeval::as_call(stringr::str_c(stringr::str_c(valid_covars, collapse = " + "))),
            lhs = as.name("IS_CONTROL"),
            env = env
          )
          psm.ml    <- MatchIt::matchit(formula = f, mDT, method = "nearest", ratio = 1)
          
          index_rob <- as.numeric(psm.ml$match.matrix[, 1])
          index_ctr <- as.numeric(row.names(psm.ml$match.matrix))
          
          list(
            r = mDT[ index_rob ],
            c = mDT[ index_ctr ]
          )
          
        } 
      }, error = function(c){
        if(stringr::str_detect(c$message, "No units were matched"))
          return(NULL)
      })
      
      if( is.null(psmll) )
        return(NULL)
      
      
      # define list of functions to apply to stratified groups
      FUNS   <- list(
        PSM_COUNT = length,
        MEAN      = mean,
        MEDIAN    = median,
        STD       = sd,
        VAR       = var
      )
      
      keyby <- c(treatment, "CID", "HOSPITAL_ID", split_var)
      
      names(FUNS)[-1] <- paste0(o, "_", names(FUNS)[-1])
      sdata <- rbindlist(psmll)[ , fapply(funs = FUNS, 
                                          vector = get(o), 
                                          min.obs = 10), 
                                 by = keyby]
      
      p_val <- data.table(
        round(t.test(psmll$r[, get(o)], psmll$c[, get(o)])$p.value, 8)
      )
      setnames(p_val, paste0(o, "_PVALUE"))
      resDT <- cbind(sdata, p_val)
      setkeyv(resDT, keyby)
      return(resDT)
      
    })
    
    return( Reduce(function(x, y) x[y], outList) )
  }))
  return(RESULT)
}