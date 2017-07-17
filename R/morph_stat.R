
#' Patient Cohort Analysis
#'
#' Compute basic statistics as well as a paired T-Test on an outcome measure (\code{outcome}) after
#' stratification of the data using the provided covariates (\code{covars}) and
#' balancing cohorts of patients based on treatment modality (\code{treatment}).
#'
#' Note: data provided in the \code{DT} argument should have carefully considered outliers and potential to influence.
#' Additionally, this function expects two groups. Therefore the treatment column should be reduced to the desired
#' two group values.
#'
#' @param DAT ISI Data
#' @param GRP_VARS Variables use to stratify observations into groups
#' @param TREATMENT Name of the column that represents assignment of treatment or control group
#' @param OUTCOME The name of the column that represents the outcome to analysis across groups
#' @param min_n The minimum number of patients required to have a valid group (default = 25)
#' @return A data.table with the desired statistics
#' @export
#'
#' @examples
#' # EXAMPLES PENDING
morph_stat <- function(DAT, GRP_VARS, TREATMENT, OUTCOME, min_n = 25){
   # define list of functions to apply to stratified groups
   FUNS   <- list(
      numObs = length,
      mean   = mean,
      median = median,
      sd     = sd,
      var    = var,
      stderr = function(x) sd(x) / sqrt(length(x))
   )

   # validate that variables provided exist in the dataset
   chk <- data_check( unique(c(GRP_VARS, TREATMENT, OUTCOME)), DAT)
   if( length(chk) )
      stop("Columns missing in data: ", stringr::str_c(chk, collapse = ", "))

   # id each stratified group of patients (not by modality)
   DAT[, COHORT_ID := .GRP, by = GRP_VARS]

   # split each cohort by modality (ensuring min num of patients) and run stats
   sdata <- DAT[,
                c( fapply(FUNS, get(OUTCOME), min.obs = 10) ),
                by = c(TREATMENT, "COHORT_ID", GRP_VARS)
                ]

   setkeyv(statDT, c("COHORT_ID", TREATMENT))

   # Keep cohorts where both treatments have valid number of patients
   patDT  <- dcast(data = statDT[, numObs, .(COHORT_ID, MODALITY)],
                   formula = xFormula("COHORT_ID", TREATMENT),
                   value.var = "numObs",
                   fill = NA)
   setDT(patDT)

   keepInd <- Reduce(function(x, y) !(is.na(x) | is.na(y)), patDT[, !"COHORT_ID"])

   resDT <- statDT[COHORT_ID %in% patDT[keepInd, COHORT_ID]]

   ##
   ## Calculate test for difference of means across modalities
   ##
   tmp <- resDT[, c(TREATMENT, "COHORT_ID", "numObs", "mean", "var"), with=FALSE]
   mlist <- split(tmp, resDT$MODALITY)

   oN <- mlist$Open$numObs
   rN <- mlist$Robotic$numObs

   oVar <- mlist$Open$var
   rVar <- mlist$Robotic$var

   oAve <- mlist$Open$mean
   rAve <- mlist$Robotic$mean

   pooled_est <- ( (oN - 1)*oVar + (rN - 1)*rVar ) / ( (oN - 1) + (rN - 1) )

   t_value <- (oAve - rAve) / ( sqrt(pooled_est) * sqrt( (1/oN) + (1/rN) ))

   pvals <- round(2*pt(t_value, (oN - 1) + (rN - 1), lower=FALSE), 4)

   ##
   ## add the pvals to each table, combine, then return
   ##
   bindres <- rbind(
      mlist$Open[,    .(MODALITY, COHORT_ID, pvals)],
      mlist$Robotic[, .(MODALITY, COHORT_ID, pvals)]
   )

   setkeyv(bindres, c("COHORT_ID", TREATMENT))
   setkeyv(resDT,   c("COHORT_ID", TREATMENT))
   output <- resDT[bindres]
   return(output[order(PRIMARY_PROCEDURE, COHORT_ID)])
}
