.onAttach <- function(libname, pkgname){
  
  tmp <- paste0(gsub("base$", "", system.file()), "/access.pk")
  kpath <- normalizePath(tmp, mustWork = FALSE)
  
  ## Write out local key to package folder if needed
  if(!file.exists(kpath)){
    packageStartupMessage("No access key found")
    return(invisible(FALSE))
  }
  
  vpath <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  vpath <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  options(secret.vault = vpath)
  options(secret.key = kpath)
  return(invisible(TRUE))
}
