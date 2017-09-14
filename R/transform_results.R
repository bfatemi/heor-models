#' Transform Final Output
#' 
#' Function to transform final result of runPSM per requirements. Note: current data layout is very sub-optimal.
#'
#' @param outDT Final step required due to requirements of how results should be structured
#' 
#' @import data.table
#' @importFrom stats as.formula
#' @importFrom stringr str_c str_detect str_split_fixed
#' 
#' @export
transformResult <- function(outDT){
  
  # change names for convenience
  setnames(outDT, "PSMCount", "N") 
  

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