## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN TSD WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: TSD Refactor - Original Script by Imran Mujawar
## CREATION DATE: 6/8/2020
## UPDATE: 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)

source("C:/Users/pcx5/Documents/Randy Codebase/R/TSD/MSD_TSD_FXNS.R")

## ==================== MAIN ====================

setwd("C:/Users/pcx5/Desktop/MSD_FY20Q3_Site/MSD")
period <- "CleanQ3"
ou_list <- list.files(pattern = ".*.txt")
ou_list1 <- ou_list[-c(12,19,21,22)]
ou_list2 <- ou_list[c(12,19,21,22)]

for (ou in ou_list) {
  df <- tsd_generate(ou, #msd_txt
                     "2020_qtr2", #prevR
                     "2020_qtr3", #currR
                     "2020_targets") #currT
  ou_name <- unique(df$OperatingUnit)
  openxlsx::write.xlsx(df, file=paste("C:/Users/pcx5/OneDrive - CDC/TSD/TSD_20Q3_", ou_name, "_q2q3", period,".xlsx", sep = ""), 
             keepNA = FALSE, asTable = TRUE)
  rm(df)
  gc()
}


setwd("C:/Users/pcx5/OneDrive - CDC/TSD")
append_list <- list.files(pattern = ".*.xlsx")
big_ou <- data.frame()

for (ou1 in append_list){
  df1 <- read.xlsx(ou1)
  big_ou <- bind_rows(big_ou,df1)
}

for (ou2 in unique(big_ou$OperatingUnit)){
  big_ou2 <- filter(big_ou, OperatingUnit == ou2)
  openxlsx::write.xlsx(big_ou2, file=paste("C:/Users/pcx5/OneDrive - CDC/TSD/TSD_20Q3_", ou2, "_cumulatives", period,".xlsx", sep = ""), 
                       keepNA = FALSE, asTable = TRUE)
}

## ==================== TESTS ==================== 
#
# test <- msd_import(ou_list[19]) # CHECKED
# 
# test1 <- msd_convert_long(test) # CHECKED
# rm(test)
# gc()
# test2 <- recode_period_txdisagg(test1, "2019_cumulative", "2020_cumulative", "2020_targets")
# rm(test1)
# gc()
# test3 <- recode_prioritizations(test2)
# rm(test2)
# gc()
# test4 <- collapse_age(test3)
# rm(test3)
# gc()
# test5 <- reformat_age_sex(test4)
# rm(test4)
# gc()
# test6 <- redo_indicator_name(test5)
# rm(test5)
# gc()
# test7 <- tsd_clean(test6)
# rm(test6)
# gc()
# test8 <- tsd_convert_wide(test7)
# rm(test7)
# gc()
# test9 <- tsd_dummies(test8)
# rm(test8)
# gc()