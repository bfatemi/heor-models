

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



.onLoad <- function(libname, pkgname){
  # path.key <- normalizePath(paste0(devtools::inst("isds"), "/access.pk"), mustWork = FALSE)
  # 
  # ## Write out local key to package folder if needed
  # if(!file.exists(path.key))
  #   openssl::write_pem(secret::local_key(), path.key)
  # 
  # path.vault <- normalizePath(paste0(devtools::inst("isds"), "/vault"), mustWork = FALSE)
  # options(secret.vault = path.vault)
  # options(secret.key = path.key)
  
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

  ghp <- Sys.getenv("GITHUB_PAT")
  Sys.setenv(GITHUB_PAT = "")
  on.exit(Sys.setenv(GITHUB_PAT = ghp))

  lines <- capture.output(devtools::source_gist(id = "d9fbf427eb67282ca3549ea0c420945b",
                                                filename = "pkg.start.isds.R",
                                                quiet = TRUE),
                          type = "message")

  packageStartupMessage(str_c(lines[-1], collapse = "\n"))
  return(invisible(TRUE))
}

# request_access <- function(){
#   tmp <- paste0(gsub("base$", "", system.file()), "/access.pk")
#   kpath <- normalizePath(tmp, mustWork = FALSE)
#   
#   if(file.exists(kpath))
#     stop("Existing file found. Delete prior key at path: \n", kpath, call. = FALSE)
#   
#   pkg <- devtools::as.package(system.file(package = "isds"))
#   
#   
#   field.from <- paste0(Sys.getenv("RSTUDIO_USER_IDENTITY"), "@intusurg.com")
#   field.to <- str_extract(eval(parse(text = pkg$`authors@r`)), "(?<=\\<).+(?=\\>)")
#   
#   mailR::send.mail(from = field.from,
#                    to = field.to,
#                    subject = "[Access Request] isds",
#                    body = paste0("From user: ", Sys.getenv("RSTUDIO_USER_IDENTITY")), 
#                    authenticate = TRUE,
#                    smtp = list(host.name = "smtp.office365.com", 
#                                port = 587,
#                                user.name = "bobby.fatemi@intusurg.com", 
#                                passwd = "Newyork7", 
#                                tls = TRUE))
#   
#   
#   openssl::write_pem(secret::local_key(), kpath)
# }

# .onAttach <- function(libname, pkgname){
#   path.pubkey <- paste0(Sys.getenv("R_USER"), "/pubkey_isds.txt")
# 
#   tryCatch({
#     secret::get_secret("is_connect", Sys.getenv("USER_KEY"), getOption("secret.vault"))
#   },error = function(c){
#     packageStartupMessage(paste0("\nAccess not configured.\n\nEmail public key to: \n\tBobby.Fatemi@intusurg.com\nLocated here: \n\t", path.pubkey))
#   })
# }
