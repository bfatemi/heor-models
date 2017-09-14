#' Run Stats Across Groups
#' 
#' @param DT Data that contains a column representing the outcome variables and contains columns CID and Modality for grouping
#' @param outcome_vars [ARGUMENT DEFINITION NEEDED]
#' 
#' @import stats
#' 
#' @name Stat_Data
NULL

#' @describeIn Stat_Data Function that takes the outcome of \code{getMatchedData} and 
#' runs statistical summary functions and performs a paired t-test across the psm matched
#' groups
#' @export
getStatData <- function(DT, outcome_vars){
  
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

