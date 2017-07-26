library(data.table)
library(RODBC)
library(secret)
library(stringr)




cnArgs <- get_secret("is_connect")$cn_args
dbArgs <- get_secret("is_connect")$db_args
cntxt <- str_c(names(cnArgs), "=", cnArgs, collapse = ";")
cnObj <- odbcDriverConnect(cntxt)
query <- "SELECT * FROM [CUSTOM_QTI].[Hospital].[Fact_CASReport_Hospital]"

# get column names, send query, get rows
colInfo <- sqlColumns(cnObj, dbArgs$tbl_nam)
setDT(colInfo)

cols <- colInfo$COLUMN_NAME
kcols <- cols[!str_detect(cols, "FILE|COST|REVENUE|PAYOR|MALE|DATE$|MARGIN|PACU|FIN_|CHARGES|_FLA?G|ROW_")]

setkeyv(colInfo, "COLUMN_NAME")
colDT <- colInfo[kcols]
cols.char <- colDT[get("TYPE_NAME") == "varchar", get("COLUMN_NAME")]

query_char <- str_replace(query, "\\*", str_c(cols.char, collapse = ","))
stopifnot(odbcQuery(cnObj, query_char, 1000) == 1)
charDT <- as.data.table(odbcFetchRows(cnObj)$data)
setnames(charDT, cols.char)

lapply(charDT, function(i) sort(unique(i[is.na(i) | !str_detect(i, "^Out of|Not Present|null|\\*")])))
cnames <- sqlColumns(cnObj, dbArgs$tbl_nam)[["COLUMN_NAME"]]


odbcClose(cnObj)
# set as data.table, set column names and return
setDT(dat$data)
setnames(dat$data, cnames)

