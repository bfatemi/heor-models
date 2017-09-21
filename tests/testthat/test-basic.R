library(data.table)
library(secret)

context("Test of all major functions")

test_that("Testing data query and cleaning", {

  skip_on_cran()
  skip_on_travis()
  
})
  

test_that("Local Test of user-facing getDataQTI (wrapper around get_raw and clean_data)", {
  skip_on_travis()
  skip_on_cran()
  

})




test_that("CI check of clean_data and runPSM functions",{
  skip_on_cran()

  
})






# CLEAN UP RAW DATA -------------------------------------------------------

# library(data.table)
# library(isds)
# library(easydata)
# library(stringr)
# 
# rawDT  <- getRawData()
# descDT <- easy_describe(rawDT)
# 
# num_cols <- descDT[, col_name[str_detect(col_name, "Time|Cost|Hours|Days|Mins|Revenue|Age$|Charges|Margin|Numeric")]]
# 
# 
# ## For each numeric column, first turn each non-digit to an NA value then set num class
# 
# for(nCol in num_cols){
#   
#   colVal <- rawDT[, get(nCol)]
#   index  <- rawDT[!is.na(colVal), which(!str_detect(colVal, pattern = "^\\-?[0-9]*\\.?[0-9]+$"))]
#   
#   if(length(index) > 0)
#     set(rawDT, i = index, j = nCol, value = NA)
#   set(rawDT, i=NULL, j = nCol, value = as.numeric(rawDT[, get(nCol)]))
# }
# 
# 
# descDT2 <- easy_describe(rawDT)
# descDT2
# 
# 
# bool_cols <- unique(c(descDT[str_detect(col_name, "Flag"), col_name], descDT[count_unique == 2, col_name]))
# bool_cols



