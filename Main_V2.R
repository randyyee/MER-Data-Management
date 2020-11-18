## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_NET_NEW Adjustments by Aaron Chafetz (USAID)
##      TX_NET_NEW Adjustment FXN by Andrea Stewart (CDC)
## CREATION DATE: 6/8/2020
## UPDATE: 11/17/2020
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)

source("C:/Users/pcx5/Documents/Randy Codebase/R/TSD/MSD_TSD_FXNS_V2.R")

## ==================== MAIN ====================
setwd("C:/Users/pcx5/Desktop/MSD_FY20Q3_Site/MSD") # Folder 
period <- "InitialQ4"
ou_list <- list.files(pattern = ".*.txt")

big_ou <- data.frame()

for (ou in ou_list) {
  
  df <- txs_adj_generate(ou, # msd_txt
                     "2020_qtr1", # prevR
                     "2020_qtr2", # currR
                     "2020_targets") # currT
  df1 <- txs_adj_generate(ou,
                     "2020_qtr2",
                     "2020_qtr3",
                     "2020_targets")
  df2 <- txs_adj_generate(ou,
                     "2020_qtr2",
                     "2020_qtr3",
                     "2020_targets")
  
  big_ou <- bind_rows(list(big_ou, 
                           df, 
                           df1 %>% select(-TX_CURR_Now_T), 
                           df2 %>% select(-TX_CURR_Now_T)))
  rm(df)
  rm(df1)
  rm(df2)
  gc()
}

openxlsx::write.xlsx(big_ou, file=paste("C:/Users/pcx5/OneDrive - CDC/TSD/Waterfall_ADJ", period,".xlsx", sep = ""), 
                     keepNA = FALSE, asTable = TRUE)



## ==================== TESTS ==================== 
# test <- msd_import(ou_list[2])
# 
# test1 <- msd_convert_long(test)
# 
# test2 <- recode_period_txdisagg(test1, "2020_qtr2", "2020_qtr3", "2020_targets")
# 
# test3 <- recode_prioritizations(test2)
# 
# test4 <- collapse_age(test3)
# 
# test5 <- redo_indicator_name(test4)
# 
# test6 <- txs_clean(test5)
# 
# test7 <- txs_convert_wide(test6)
# 
# test8 <- tx_net_new_adj(test1)
# 
# test9 <- txs_w_netnewadj(test7, test8)
# 
# compose_test <- txs_generate(ou_list[2], "2020_qtr2", "2020_qtr3", "2020_targets")
# 
# compose_test2 <- txs_adj_generate(ou_list[2], "2020_qtr2", "2020_qtr3", "2020_targets")
