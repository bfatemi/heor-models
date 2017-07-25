
context("Test of all major functions")

test_that("Testing data query and cleaning", {

  skip_on_cran()
  # skip_on_travis()
  # expect_false(Sys.getenv("USER_KEY") == "")
  
  # Set path to package vault
  op  <- getOption("secret.vault")
  nop <- normalizePath(paste0(devtools::inst("isds"), "/vault"))
  
  options(secret.vault = nop)
  on.exit(options(secret.vault = op))
  
  # if this fails, that means this local machine is not authorized
  # If the user of this machine is authorized, then read and decrypted stored data
  check <- tryCatch(secret::get_secret("is_connect", key = secret::local_key(), vault = nop), error = function(c) NULL)
  
  if(is.null(check)){
    raw_data <- tryCatch(secret::get_secret("raw_data", secret::local_key()), 
                         error = function(c) stop("Error on travis raw data decrypt data", call. = FALSE))
  }else{
    raw_data <- tryCatch(get_raw(), 
                         error = function(c) stop("Error on data query to network", 
                                                  call. = FALSE))
  }
  
  DT <- tryCatch(clean_data(raw_data), 
                 error = function(c) stop("Error in data cleaning: \n", c$message, call. = FALSE),
                 warning = function(c){
                   cat(paste0("Warning in data cleaning: \n", c$message))
                   suppressWarnings(clean_data(raw_data))
                 })
  
  expect_is(DT, "data.table")
  expect_gt(nrow(DT), 1)
  expect_gt(ncol(DT), 5)
})
  

test_that("Local Test of user-facing getDataQTI (wrapper around get_raw and clean_data)", {
  # skip_on_travis()
  skip_on_cran()
  
  # Set path to package vault
  op  <- getOption("secret.vault")
  nop <- paste0(devtools::inst("isds"), "\\vault")
  
  options(secret.vault = nop)
  on.exit(options(secret.vault = op))
  
  # if this fails, that means this local machine is not authorized
  # If the user of this machine is authorized, then read and decrypted stored data
  check <- tryCatch(secret::get_secret("is_connect", key = secret::local_key(), vault = nop), error = function(c) NULL)
  
  if(!is.null(check)){
    DT <- getDataQTI()
    expect_is(DT, "data.table")
    expect_gt(nrow(DT), 1)
    expect_gt(ncol(DT), 5)
    
    DT <- getDataQTI(10112)
    expect_is(DT, "data.table")
    expect_gt(nrow(DT), 1)
    expect_gt(ncol(DT), 5)  
  }
  
})



