
context("Test of all major functions")

test_that("Testing data query and cleaning", {

  skip_on_cran()
  skip_on_travis()
  # expect_false(Sys.getenv("USER_KEY") == "")
  
  # Set path to package vault
  op  <- getOption("secret.vault")
  nop <- paste0(devtools::inst("isds"), "\\vault")
  
  options(secret.vault = nop)
  on.exit(options(secret.vault = op))
  
  # if this fails, that means this local machine is not authorized
  # If the user of this machine is authorized, then read and decrypted stored data
  # secret::get_secret("is_connect",vault = nop)
  check <- tryCatch(get_secret("is_connect", key = local_key(), vault = nop), 
                    error = function(c) NULL)
  
  if(is.null(check)){
    raw_data <- tryCatch(get_secret("raw_data", local_key()), 
                         error = function(c) stop("Error on travis raw data decrypt data", 
                                                  call. = FALSE))
  }else{
    raw_data <- tryCatch(get_raw(), 
                         error = function(c) stop("Error on data query to network", 
                                                  call. = FALSE))
  }
  
  
  
  DT <- tryCatch(clean_data(raw_data), 
                 error = function(c) stop("Error in data cleaning", call. = FALSE))
  
  expect_is(DT, "data.table")
  expect_gt(nrow(DT), 1)
  expect_gt(ncol(DT), 5)
})
  

test_that("Local Test of user-facing getDataQTI (wrapper around get_raw and clean_data)", {
  skip_on_travis()
  skip_on_cran()
  
  # Set path to package vault
  op  <- getOption("secret.vault")
  nop <- paste0(devtools::inst("isds"), "\\vault")
  
  options(secret.vault = nop)
  on.exit(options(secret.vault = op))
  
  DT <- getDataQTI()
  expect_is(DT, "data.table")
  expect_gt(nrow(DT), 1)
  expect_gt(ncol(DT), 5)
  
  DT <- getDataQTI(10112)
  expect_is(DT, "data.table")
  expect_gt(nrow(DT), 1)
  expect_gt(ncol(DT), 5)
})
# 
#   # # Skip if travis does not have access to secret
#   # testthat::(
#   #   "travis_ci" %in% secret::list_owners("is_connect", getOption("secret.vault")), 
#   #   "Travis does not have access to secret"
#   # )
#   
#   # 
#   # # getDataQTI()
#   # # get_stats()
#   # # psm_data()
#   # # runPSM()
#   # 
#   # 
#   # skip_on_travis()
#   # skip_on_cran()
#   # 
#   # 
#   # 
#   # secret::local_key()
#   # secret::list_users(getOption("secret.vault"))
#   
#   
#   # Check if private key exists and user is authorized
#   
# 
#   DT <- getDataQTI()
#   expect_true(is.data.table(DT))  
#   expect_true(nrow(DT) > 1)
  
  # strat_vars   <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")
  # covariates   <- c("BMI", "PATIENT_AGE", "CHARLSON_SCORE", "PATIENT_GENDER")
  # outcome_vars <- c("LOS_HOURS", "OR_TIME_MINS")
  # 
  # psm_dt <- psm_data(DT = DT,
  #                      strat_vars = strat_vars,
  #                      covariates = covariates,
  #                      outcome_vars = outcome_vars)
  # expect_true(is.data.table(psm_dt))
  # expect_true(nrow(psm_dt) > 1)
  # 
  # sdt <- get_stats(psm_dt, outcome_vars)
  # expect_true(is.data.table(sdt))
  # expect_true(nrow(sdt) > 1)
  # 
  # runPSM(hosp_id = 10112)



