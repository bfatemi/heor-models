

# SUBSET DATA FOR TWO MODALITIES AND DESIRED HOSP -------------------------

hosp_data <- getHospData()
DT <- hosp_data[MODALITY %in% c("Robotic", "Open") & HOSPITAL_ID == 10112 & BMI > 0]



# DEFINE INPUTS THAT WILL STRATIFY DATA FOR PSM THEN RUN ------------------

treat    <- c("MODALITY")
strat    <- c("PRIMARY_PROCEDURE", "BENIGN_MALIGNANT", "PATIENT_TYPE")
covar    <- c("BMI", "PATIENT_AGE", "CHARLSON_SCORE", "PATIENT_GENDER")
outcomes <- c("LOS_HOURS", "OR_TIME_MINS")

statDT <- getStatPSM(DT, treat, strat, covar, outcomes)


# fwrite(statDT, "../../Dropbox/WORK/ISI/Projects/PSM_ALGO/data/h10112.csv")
# RUN PSM ANALYSIS --------------------------------------------------------



  
getStats(DT, "MODALITY", c("PRIMARY_PROCEDURE", "PATIENT_TYPE", "BENIGN_MALIGNANT"), )
  
  # morph_stat(DAT = sDT, 
  #            GRP_VARS = "PRIMARY_PROCEDURE", 
  #            OUTCOME = ovar, 
  #            TREATMENT = "modality")
  # 
  # split(mDT, mDT[, get(treatment)])
  # 
#   # 
#   # 
#   # lapply(outcome, function(i, data) )
#   # procList <- split(psmDT, psmDT$PRIMARY_PROCEDURE)
#   # bmList   <- split(procList$Hyst, hdt$BENIGN_MALIGNANT)
#   # 
#   # 
#   # # mtch each
#   # i <- bmList[[1]]
#   # 
#   # iList <- split(i, i$IS_ROBOTIC)
#   # 
#   # rDT <- iList$`TRUE`
#   # oDT <- iList$`FALSE`
# 
# 
# match_data <- data.table::rbindlist(apply(rDT, 1, function(i){
# 
#   i <- rDT[1]
# 
#   p_score <- as.numeric(i[, "Propensity_Score", with=FALSE])
# 
#   indexMatch <- oDT[, which.min(p_score - Propensity_Score)][1]
#   oDT[62]
# 
#   library(ggplot)
#   (oDT) + geom_hist(aes(BMI_CATEGORY_PSM, Propensity_Score))
#   res <- cbind(row, data.table(match_pid = oDT[indexMatch, PID]))
#   oDT <<- oDT[-indexMatch]
#   return(res)
# }))
# 
# oDT <- iList$`FALSE`
# statDT <- oDT[PID %in% match_data$match_pid]
# 
# morph_stat()
# 
# 
# 
# 
# 
# getPropScore(sDT, covars, treatment, outcome)
