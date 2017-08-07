.onLoad <- function(libname, pkgname){
  
  tmp <- paste0(gsub("base$", "", system.file()), "/access.pk")
  kpath <- normalizePath(tmp, mustWork = FALSE)
  
  ## Write out local key to package folder if needed
  if(!file.exists(kpath)){
    packageStartupMessage("No access key found.
                          Please use make_is_key() to initiate access.")
    return(invisible(FALSE))
  }
  
  vpath <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  options(secret.vault = vpath)
  options(secret.key = kpath)
  return(invisible(TRUE))
}
