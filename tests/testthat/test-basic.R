

test_that("Running a diff of means between psm matched groups", {
  skip_on_travis()
  skip_on_cran()
  
  op <- getOption("secret.vault")
  options(secret.vault = "C:/Users/bobbyf/Documents/isds/inst/vault")
  on.exit(options(secret.vault = op))

  DT <- getDataQTI()
  expect_true(is.data.table(DT))  
  expect_true(nrow(DT) > 1)
  
  strat_vars   <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")
  covariates   <- c("BMI", "PATIENT_AGE", "CHARLSON_SCORE", "PATIENT_GENDER")
  outcome_vars <- c("LOS_HOURS", "OR_TIME_MINS")

  psm_dt <- psm_data(DT = DT,
                       strat_vars = strat_vars,
                       covariates = covariates,
                       outcome_vars = outcome_vars)
  expect_true(is.data.table(psm_dt))
  expect_true(nrow(psm_dt) > 1)

  sdt <- get_stats(psm_dt, outcome_vars)
  expect_true(is.data.table(sdt))
  expect_true(nrow(sdt) > 1)

  runPSM(hosp_id = 10112)
})


