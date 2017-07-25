#' #' Statistical Helper Functions
#' #'
#' #' @param DT [ARGUMENT DEFINITION NEEDED]
#' #' @param x A vector of numerical data
#' #' @param method one of "prob" or "quantile" specifying the detection method
#' #' @param p if \code{method = "prob"} then this specifies the probability threshold to use (defaults to .05)
#' #'
#' #' @return A boolean vector corresponding to the observations in argument \code{x}
#' #' 
#' #' @import stats
#' #' 
#' #' @name stat_helper_funs
#' NULL

#' #' @describeIn stat_helper_funs function to id outliers
#' #' @export
#' id_outliers <- function(x = NULL, method = c("quantile", "prob"), p = .05){
#'   method <- match.arg(method, c("quantile", "prob"))
#'   
#'   if(is.null(x))
#'     stop("Input x is null", call. = FALSE)
#'   if(!is.numeric(x)){
#'     warning("coercing x to class numeric", call. = FALSE)
#'     x <- suppressWarnings(as.numeric(x))
#'   }
#'   
#'   if(length(x)==0)
#'     stop("length of x is 0", call. = FALSE)
#'   
#'   
#'   if(method == "quantile"){
#'     # interquantile range
#'     q3 <- stats::quantile(x, .75)
#'     q1 <- stats::quantile(x, .25)
#'     
#'     lower <- q1 - 1.5*(q3 - q1)
#'     upper <- q3 + 1.5*(q3 - q1)
#'     
#'     return(x < lower | x > upper)
#'   }
#'   
#'   if(method == "prob"){
#'     res <- round(pnorm(x, mean(x), sd(x), FALSE), digits = 5)
#'     return(res < p | res > 1-p)
#'   }
#'   
#'   stop("Incorrect method specified. Should be one of: prob, quantile")
#' }

#' #' @describeIn stat_helper_funs which columns are all NA
#' #' @export
#' wcolNA <- function(DT){
#'   which(apply(DT[, lapply(.SD, is.na)], 2, sum)==nrow(DT))
#' }
#' 
#' #' @describeIn stat_helper_funs which rows are all NA
#' #' @export
#' wrowNA <- function(DT){
#'   which(apply(DT[, lapply(.SD, is.na)], 1, sum)==ncol(DT))
#' }

