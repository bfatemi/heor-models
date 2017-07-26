library(data.table)
library(RODBC)
library(secret)
library(stringr)

get_level <- function(){
  
  # Get encrypted connection string arguments
  ciph <- get_secret("is_connect")
  carg <- ciph$cn_args
  darg <- ciph$db_args
  
  # Get connection object and start query text
  conn  <- odbcDriverConnect( str_c(names(carg), "=", carg, collapse = ";") )
  query <- "SELECT * FROM [CUSTOM_QTI].[Hospital].[Fact_CASReport_Hospital]"
  
  # Get column names and information about column classes
  colDat <- sqlColumns(conn, darg$tbl_nam)
  setDT(colDat)
  
  # Drop all columns that match with the pattern defined
  dropPat <- "FILE|COST|REVENUE|PAYOR|MALE|DATE$|MARGIN|PACU|FIN_|CHARGES|_FLA?G|ROW_"
  kCols   <- colDat[ , COLUMN_NAME[ !str_detect(COLUMN_NAME, dropPat) ]]
  
  # Set key on column name and reduce column info data to just varchars (some will be factors in R)
  setkeyv(colDat, "COLUMN_NAME")
  col_chars <- colDat[kCols][get("TYPE_NAME") == "varchar", get("COLUMN_NAME")]
  
  # Now query for the data associated with just the selected columns and get a list
  # of unique values present in the data for each
  checkDB <- odbcQuery(conn, str_replace(query, "\\*", str_c(col_chars, collapse = ",")), 1000)
  if(checkDB == 1){
    cDT <- as.data.table(odbcFetchRows(conn)$data)
    odbcClose(conn)
    setnames(cDT, col_chars)
    resList <- lapply(cDT, function(i) sort(unique(i)))
  }else{
    stop("Check query or connection", call. = FALSE)
  }
  return(resList) # if no error, then results are returned
}

levelList <- getLevelsQTI()

secret::add_secret(name = "levelList", 
                   value = levelList, 
                   users = "bobby.fatemi@intusurg.com", 
                   vault = getOption("secret.vault"))


