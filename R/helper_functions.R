data_check <- function(cols, DT){
   data_env <- rlang::as_env(DT)
   cols_missing <- cols[which(!rlang::env_has(data_env, cols))]
   if( length(cols_missing) > 0 ){
      return( cols_missing )
   }
   return( character(0) )
}

fapply <- function(funs, vector, min.obs = 25, na.rm = TRUE, verbose = FALSE){

   na_index <- which( is.na(vector) )
   countNA  <- length(na_index)

   # remove if nas exist and flag is true
   if( countNA > 0 & na.rm)
      vector <- vector[ -na_index ]

   # if num obs below threshold, return null
   if( length(vector) < min.obs ){
      if(verbose)
         warning( "Group had fewer observations than min threshold ", call. = FALSE)
      return(NULL)
   }
   return(lapply(funs, do.call, list(vector)))
}

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


# xFormula <- function(y = NULL, x = NULL){
# 
#    env <- rlang::caller_env()
# 
#    y[is.null(y)] <- "."
#    x[is.null(x)] <- "."
# 
#    f <- rlang::new_formula(
#       rhs = as.symbol(stringr::str_c(x, collapse = " + ")),
#       lhs = as.name(stringr::str_c(y, collapse = " + ")),
#       env = env
#    )
#    f_env(f) <- pryr::parenv()
#    return(f)
# }










# PatCount <- function(sDT, y, x){
# 
#    countDT <- sDT[, .N, c(y, x)][N >= 10]
# 
#    f.cast <- new_formula(
#       lhs = as_lang(str_c(x, collapse = " + ")),
#       rhs = as_symbol(y)
#    )
# 
#    wideDT <- dcast(data = countDT, formula = f.cast, value.var = "N")
# 
#    mcols <- wideDT[, countDT[, unique(get(y))], with=FALSE]
#    resDT <- wideDT[ !Reduce(`|`, lapply(mcols, is.na)) ]
# 
#    setnames(resDT, names(mcols), paste0("pat_count_", str_to_lower(names(mcols))))
#    return(resDT)
# }

