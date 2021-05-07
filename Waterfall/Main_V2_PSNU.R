## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN WATERFALL WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
## CREATION DATE: 4/30/2021
## UPDATE: 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(openxlsx)

source("~/Randy Codebase/R/TSD/MSD_TSD_FXNS_V5_PSNU.R")

## ==================== MAIN ====================
setwd("C:/Users/pcx5/Downloads/MER_Structured_Datasets_PSNU_IM_FY19-21_20210319_v2_1") # Folder 
period <- "CleanQ1"
ou_list <- list.files(pattern = ".*.txt")


ptm <- proc.time()
for (ou in ou_list) {
  
  ## Import
  ou_df <- msd_df(ou)
 
  ## Waterfall Generate
  df0a <- txs_generate(ou_df, # msd_txt
                       "2019_qtr4", # prevR
                       "2020_qtr1", # currR
                       "2020_targets") # currT
  
  dfa <- txs_generate(ou_df, 
                      "2020_qtr1", 
                      "2020_qtr2", 
                      "2020_targets") 
  df1a <- txs_generate(ou_df,
                       "2020_qtr2",
                       "2020_qtr3",
                       "2020_targets")
  df2a <- txs_generate(ou_df,
                       "2020_qtr3",
                       "2020_qtr4",
                       "2020_targets")
  
  df3a <- txs_generate(ou_df,
                       "2020_qtr4",
                       "2021_qtr1",
                       "2021_targets")
  
  ## Waterfall Column Order
  ou_ou2 <- bind_rows(list(df0a, dfa, df1a, df2a, df3a)) %>% 
    mutate(period = paste0("FY",substr(period,3,4),"Q",substr(period,9,9)))
  
  shell_df1 <- c("operatingunit",                                                         
                 "countryname",                                                           
                 "snu1",                                                                  
                 "snuprioritization",                                                     
                 "psnu",                                                                  
                 "psnuuid",                                                               
                 "fundingagency",                                                         
                 "primepartner",                                                          
                 "mech_name",                                                             
                 "mech_code",
                 "age_type",                                                                   
                 "age",                                                                   
                 "sex", 
                 "indicatortype",
                 "period",
                 
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
                 "TX_ML_No Contact Outcome - Interruption in Treatment 3+ Months Treatment_Now_R",
                 "TX_NEW_Now_T")
  
  missing1 <- setdiff(shell_df1, names(ou_ou2))
  ou_ou2[missing1] <- NA
  ou_ou2 <- ou_ou2[shell_df1] # Column Order
  
  ## Export
  ou_name <- "PSNUxIM"
  
  openxlsx::write.xlsx(ou_ou2, file=paste("C:/Users/pcx5/OneDrive - CDC/TSD/Waterfall", ou_name, period,"_V1.xlsx", sep = ""), 
                       keepNA = FALSE, asTable = TRUE)
}
proc.time() - ptm
