
context("Test of all major functions")

test_that("Testing data query and cleaning", {

  skip_on_cran()
  skip_on_travis()
  
  ## Write out local key to package folder
  openssl::write_pem(local_key(), paste0(system.file(package = "isds"), "/access.pk"))
  
  # Set path to package vault and secret key
  kpath <- normalizePath(paste0(devtools::inst("isds"), "/access.pk"), mustWork = FALSE)
  vpath <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  
  # options(secret.vault = vpath)
  # options(secret.key   = kpath)
  
  key <- openssl::read_key(kpath)
  # if this fails, that means this local machine is not authorized
  # If the user of this machine is authorized, then read and decrypted stored data
  check <- tryCatch(secret::get_secret("is_connect", key = key, vault = vpath), error = function(c) NULL)
  
  if(is.null(check)){
    raw_data <- tryCatch(secret::get_secret("raw_data", key = key, vault = vpath), 
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
  skip_on_travis()
  skip_on_cran()
  
  # Set path to package vault and secret key
  kpath <- normalizePath(paste0(devtools::inst("isds"), "/access.pk"), mustWork = FALSE)
  vpath <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  
  # options(secret.vault = vpath)
  # options(secret.key   = kpath)
  
  key <- openssl::read_key(kpath)
  
  # if this fails, that means this local machine is not authorized
  # If the user of this machine is authorized, then read and decrypted stored data
  check <- tryCatch(secret::get_secret("is_connect", 
                                       key = key, 
                                       vault = vpath), 
                    error = function(c) NULL)
  
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



