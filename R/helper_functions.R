getPatientData <- function(SELECT = "*", WHERE = NULL){
   srv     <- "AZCWDA0008"
   db      <- "CUSTOM_QTI"
   tbl_cat <- "Hospital"
   tbl_nam <- "Fact_CASReport_Hospital"
   tbl   <- paste0(paste0("[", db, "]"), ".", paste0("[", tbl_cat, "]"), ".", paste0("[", tbl_nam, "]"))
   query <- paste0("SELECT ", SELECT, "\nFROM ", tbl, c("\nWHERE ")[!is.null(WHERE)], WHERE)
   DT    <- getData("QTI", query)
   if(nrow(DT) == 1 & ncol(DT) == 1 & DT[1,1] == -1)
      stop("Query caused error: ", query, call. = FALSE)

   date_today <- Sys.Date()
   dir_path <- "C:/Users/bobbyf/Dropbox/WORK/ISI/Projects/PSM_Algorithm/data/"
   dest_path <- paste0(dir_path, "data_", date_today, ".csv")
   fwrite(DT, dest_path)
   DT
}

readPatientData <- function(){
   fpath <- "C:/Users/bobbyf/Dropbox/WORK/ISI/Projects/PSM_Algorithm/data/data_2017-07-07.csv"
   fread(fpath)
}



data_check <- function(cols, DT){
   data_env <- as_env(DT)
   cols_missing <- cols[which(!env_has(data_env, cols))]
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


xFormula <- function(y = NULL, x = NULL){

   env <- rlang::caller_env()

   y[is.null(y)] <- "."
   x[is.null(x)] <- "."

   f <- rlang::new_formula(
      rhs = as.symbol(stringr::str_c(x, collapse = " + ")),
      lhs = as.name(stringr::str_c(y, collapse = " + ")),
      env = env
   )
   f_env(f) <- pryr::parenv()
   return(f)
}



drawBeanPlot <- function(data, outcome, rm_out = TRUE){

   cohort_group <- c("modality", "proc_primary")

   tdt <- data[!is.na(get(outcome)), .(proc_primary, modality, outcome = get(outcome))]
   tdt[, joined_groups := str_c(str_replace_all(proc_primary, " ", "_"), " ", modality)]

   # if true, remove outliers from the outcome variable at the cohort level
   if(rm_out){
      data[!is.na(get(outcome)),
           is_outlier := id_outliers(get(outcome), method = "prob", p = .025),
           cohort_group]

      data <- data[is_outlier == FALSE]
   }

   plot_formula <- new_formula(
      lhs = as_symbol("outcome"),
      rhs = as_symbol("joined_groups")
   )

   modals <- str_to_title(tdt[, .N, modality][, modality])

   plot.new()
   par(lend = 1, mai = c(0.8, 0.8, 0.5, 0.5))
   names(modals) <- c("x", "y")
   sub_txt <- eval(substitute(paste0("(", y, " vs. ", x, ")")), as.list(modals))
   beanplot(plot_formula,
            data = tdt,
            ll = 0,
            main = bquote(substitute(paste("Outcomes By Procedure Type ", italic(.(sub_txt))))),
            side = "both",
            border = NA,
            col = list("black", "grey"),
            log = "y")
   legend("top",
          x.intersp = 0.5,
          inset = 0,
          xjust = .5,
          horiz = TRUE,
          text.width = .5,
          fill = c("black", "grey"),
          legend = rev(modals))
}


id_outliers <- function(x = NULL, method = c("quantile", "prob"), p = .05){
   method <- match.arg(method, c("quantile", "prob"))

   if(is.null(x))
      stop("Input x is null", call. = FALSE)
   if(!is.numeric(x)){
      warning("coercing x to class numeric", call. = FALSE)
      x <- suppressWarnings(as.numeric(x))
   }

   # x <- x[!is.na(x)]

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





PatCount <- function(sDT, y, x){

   countDT <- sDT[, .N, c(y, x)][N >= 10]

   f.cast <- new_formula(
      lhs = as_lang(str_c(x, collapse = " + ")),
      rhs = as_symbol(y)
   )

   wideDT <- dcast(data = countDT, formula = f.cast, value.var = "N")

   mcols <- wideDT[, countDT[, unique(get(y))], with=FALSE]
   resDT <- wideDT[ !Reduce(`|`, lapply(mcols, is.na)) ]

   setnames(resDT, names(mcols), paste0("pat_count_", str_to_lower(names(mcols))))
   return(resDT)
}

