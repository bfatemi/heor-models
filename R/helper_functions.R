

id_outliers <- function(x = NULL, method = c("quantile", "prob"), p = .05){
  method <- match.arg(method, c("quantile", "prob"))
  
  if(is.null(x))
    stop("Input x is null", call. = FALSE)
  if(!is.numeric(x)){
    warning("coercing x to class numeric", call. = FALSE)
    x <- suppressWarnings(as.numeric(x))
  }
  
  if(length(x)==0)
    stop("length of x is 0", call. = FALSE)
  
  
  if(method == "quantile"){
    # interquantile range
    q3 <- stats::quantile(x, .75)
    q1 <- stats::quantile(x, .25)
    
    lower <- q1 - 1.5*(q3 - q1)
    upper <- q3 + 1.5*(q3 - q1)
    
    return(x < lower | x > upper)
  }
  
  if(method == "prob"){
    res <- round(pnorm(x, mean(x), sd(x), FALSE), digits = 5)
    return(res < p | res > 1-p)
  }
  
  stop("Incorrect method specified. Should be one of: prob, quantile")
}

