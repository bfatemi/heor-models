

.onAttach <- function(libname, pkgname){
  
  if( !nzchar(Sys.getenv("USER_KEY")) ){
    
    # Set random RSA key as default
    tmp <- normalizePath(paste0(Sys.getenv("R_USER"), "/.ssh"), mustWork = FALSE)
    dir.create(tmp, recursive = TRUE)
    tmp <- normalizePath(paste0(tmp, "/is_access.pem"), mustWork = FALSE)
    
    if( !file.exists(tmp) ){
      key <- openssl::rsa_keygen()
      openssl::write_pem(key, tmp, password = "")  
      
      # Now save public key to package files
      path.pubkey <- paste0(Sys.getenv("R_USER"), "/pubkey_isds.txt")
      openssl::write_ssh(key$pubkey, path.pubkey)
      
      packageStartupMessage(paste0("\nAccess configured.\n\nEmail public key to: \n\tBobby.Fatemi@intusurg.com\nLocated here: \n\t", path.pubkey))
    }
    
    Sys.setenv("USER_KEY" = tmp)
  }
  
  
  
  path.vault <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  options(secret.vault = path.vault)

}
  # #   
  # # 
  # # Sys.getenv("USERPROFILE")
  # # .Library <- normalizePath("C:/Users/AshokB/Documents/R/win-library/3.3")
  # # 
  # # install.packages("devtools", lib = .Library, dependencies = TRUE)
  # # install.packages("backports", lib = .Library, dependencies = TRUE)
  # 
  # install_github("hadley/rlang", lib = .Library)
  # install_github("gaborcsardi/secret", lib = .Library)
  # install_github("bfatemi/isds", lib = .Library)
  # 
  # Sys.setenv(USER_KEY = "C:/Users/AshokB/.ssh/id_rsa")
  # 
  # 
  # library(isds)
  # runPSM()
# }
