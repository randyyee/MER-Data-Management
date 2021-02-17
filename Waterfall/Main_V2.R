## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_NET_NEW Adjustments by Aaron Chafetz (USAID)
##      TX_NET_NEW Adjustment FXN by Andrea Stewart (CDC)
## CREATION DATE: 6/8/2020
## UPDATE: 12/15/2020
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)

source("~/Randy Codebase/R/TSD/MSD_TSD_FXNS_V5.R")

## ==================== MAIN ====================
setwd("C:/Users/pcx5/Desktop/MSD_FY20Q3_Site/MSD") # Folder 
period <- "InitialQ1Test"
ou_list <- list.files(pattern = ".*.txt")

big_ou <- data.frame()

ou_list <- "C:/Users/pcx5/Downloads/test_MER_Structured_Datasets_SITE_IM_FY19-21_20210207_Haiti/test_MER_Structured_Datasets_SITE_IM_FY19-21_20210207_Haiti.txt"
for (ou in ou_list) {
  
  #ou_ou <- data.frame()
  
  df0 <- txs_adj_generate(ou, # msd_txt
                          "2019_qtr4", # prevR
                          "2020_qtr1", # currR
                          "2020_targets") # currT
  
  df <- txs_adj_generate(ou, 
                         "2020_qtr1", 
                         "2020_qtr2", 
                         "2020_targets") 
  df1 <- txs_adj_generate(ou,
                          "2020_qtr2",
                          "2020_qtr3",
                          "2020_targets")
  df2 <- txs_adj_generate(ou,
                          "2020_qtr3",
                          "2020_qtr4",
                          "2020_targets")
  
  df3 <- txs_adj_generate(ou,
                          "2020_qtr4",
                          "2021_qtr1",
                          "2021_targets")
  
  #ou_ou2 <- data.frame()
  
  df0a <- txs_generate(ou, # msd_txt
                          "2019_qtr4", # prevR
                          "2020_qtr1", # currR
                          "2020_targets") # currT
  
  dfa <- txs_generate(ou, 
                     "2020_qtr1", 
                     "2020_qtr2", 
                     "2020_targets") 
  df1a <- txs_generate(ou,
                     "2020_qtr2",
                     "2020_qtr3",
                     "2020_targets")
  df2a <- txs_generate(ou,
                      "2020_qtr3",
                      "2020_qtr4",
                      "2020_targets")
  
  df3a <- txs_generate(ou,
                       "2020_qtr4",
                       "2021_qtr1",
                       "2021_targets")
  
  ou_ou <- bind_rows(list(
    df0,
    df, 
    df1, 
    df2,
    df3
  ))
  
  ou_ou2 <- bind_rows(list(
    df0a,
    dfa, 
    df1a, 
    df2a,
    df3a
  ))
  
  ou_name <- unique(ou_ou$operatingunit)
  
  ou_ou2 <- ou_ou2 %>% select("operatingunit",                                                         
                              "countryname",                                                           
                              "snu1",                                                                  
                              "snuprioritization",                                                     
                              "psnu",                                                                  
                              "psnuuid",                                                               
                              "sitetype",                                                              
                              "sitename",                                                              
                              "orgunituid",                                                            
                              "fundingagency",                                                         
                              "primepartner",                                                          
                              "mech_name",                                                             
                              "mech_code",
                              "facility",
                              "facilityprioritization",                                                
                              "age_type",                                                                   
                              "age",                                                                   
                              "sex", 
                              "period",
                              #"period_range",
                              
                              "TX_CURR_Prev_R",
                              "TX_NEW_Prev_R",
                              "TX_CURR_Now_R",
                              "TX_CURR_Now_T",
                              "TX_ML_No Contact Outcome - Died_Now_R",
                              "TX_ML_No Contact Outcome - Refused Stopped Treatment_Now_R",
                              "TX_ML_No Contact Outcome - Transferred Out_Now_R", 
                              "TX_NEW_Now_R",
                              "TX_RTT_Now_R",
                              "TX_ML_No Contact Outcome - Interruption in Treatment <3 Months Treatment_Now_R",
                              "TX_ML_No Contact Outcome - Interruption in Treatment 3+ Months Treatment_Now_R"
  )
  
  openxlsx::write.xlsx(ou_ou, file=paste("C:/Users/pcx5/OneDrive - CDC/TSD/Waterfall_ADJ", ou_name, period,"_V1.xlsx", sep = ""), 
                       keepNA = FALSE, asTable = TRUE)
  
  openxlsx::write.xlsx(ou_ou2, file=paste("C:/Users/pcx5/OneDrive - CDC/TSD/Waterfall", ou_name, period,"_V1.xlsx", sep = ""), 
                       keepNA = FALSE, asTable = TRUE)
  
  # rm(df0)
  # rm(df)
  # rm(df1)
  # rm(df2)
  # rm(df0a)
  # rm(dfa)
  # rm(df1a)
  # rm(df2a)
  #gc()
}



## ==================== TESTS ==================== 
# test <- msd_import(ou_list[3])
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
# compose_test <- txs_generate(ou_list[3], "2020_qtr2", "2020_qtr3", "2020_targets")
# 
# compose_test2 <- txs_adj_generate(ou_list[3], "2020_qtr2", "2020_qtr3", "2020_targets")
