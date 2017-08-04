



.onLoad <- function(libname, pkgname){

  # if( !nzchar(Sys.getenv("USER_KEY")) ){
  # 
  #   tmp <- tryCatch({
  #     # Set random RSA key as default
  #     tmp <- normalizePath(paste0(Sys.getenv("R_USER"), "/.ssh"), mustWork = FALSE)
  #     suppressWarnings(dir.create(tmp, recursive = TRUE))
  #     tmp <- normalizePath(paste0(tmp, "/is_access.pem"), mustWork = FALSE)
  # 
  #     if( !file.exists(tmp) ){
  #       key <- openssl::rsa_keygen()
  #       openssl::write_pem(key, tmp, password = "")
  # 
  #       # Now save public key to package files
  #       path.pubkey <- paste0(Sys.getenv("R_USER"), "/pubkey_isds.txt")
  #       openssl::write_ssh(key$pubkey, path.pubkey)
  #     }
  #     return(tmp)
  #   }, error = function(c){
  #     warning("Access not configured")
  #     return("")
  #   })
  # 
  # 
  #   Sys.setenv("USER_KEY" = tmp)
  # }
  
  path.key <- normalizePath(paste0(devtools::inst("isds"), "/access.pk"), mustWork = FALSE)
  
  ## Write out local key to package folder if needed
  if(!file.exists(path.key))
    openssl::write_pem(secret::local_key(), path.key)
  
  path.vault <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  options(secret.vault = path.vault)
  options(secret.key = path.key)
}

# .onAttach <- function(libname, pkgname){
#   path.pubkey <- paste0(Sys.getenv("R_USER"), "/pubkey_isds.txt")
# 
#   tryCatch({
#     secret::get_secret("is_connect", Sys.getenv("USER_KEY"), getOption("secret.vault"))
#   },error = function(c){
#     packageStartupMessage(paste0("\nAccess not configured.\n\nEmail public key to: \n\tBobby.Fatemi@intusurg.com\nLocated here: \n\t", path.pubkey))
#   })
# }
