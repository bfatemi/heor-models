Sys.setenv(USER_KEY = "~/.ssh/private.pk")

test_that("Running a diff of means between psm matched groups", {

  skip_on_travis()
  
  DT <- getDataQTI()
  expect_true(is.data.table(DT))  
  
  hDT <- DT[
    MODALITY %in% c("Robotic", "Open")
    & HOSPITAL_ID == 10112
    & BMI > 0
    ]
  
  expect_true(nrow(hDT) > 1)
  
  
  treat    <- c("MODALITY")
  strat    <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")
  covar    <- c("BMI", "PATIENT_AGE", "CHARLSON_SCORE", "PATIENT_GENDER")
  outcomes <- c("LOS_HOURS", "OR_TIME_MINS")
  
  statDT <- getStatPSM(DT = hDT, 
                       treat_var = treat, 
                       split_vars = strat, 
                       covariates = covar, 
                       outcome_var = outcomes)
  
})


