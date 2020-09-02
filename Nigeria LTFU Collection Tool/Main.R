#### Nigeria LTFU Partner Collection Tool Generation
#### AUTHOR: Randy Yee (PCX5@cdc.gov)
#### CREATION DATE: 4/29/2019

library(readxl)
library(openxlsx)
library(tidyverse)
library(lubridate)
library(compareDF)

source("FileRead.R")
source("Archive_Continue_LTFU.R")
source("PartnerToolGenerator.R")

date <- format(Sys.Date(), format =  "%Y_%m_%d")
data_pull_date <- "2020-10-08" # Current Data_Pull Date

######################## 1) Archive/Continue Determination ######################## 
## a) Import New LTFU's & Updated LTFU's and Append
df_final <- ndr_wrangle("./UMB/NEWLTFU_08092020_UMB.xlsx", # New LTFU File
                        "./UMB/Continue_LTFU_2020_08_10_UMB.xlsx", # Updated LTFU File
                        data_pull_date
                        )

# Any dupes
dupe_df <- df_final %>% 
  group_by(SITE_PID,FACILITY_UID) %>% 
  filter(n()>1) %>%
  ungroup()

# Same Inactive Dates
dupe_dfb <- df_final %>% 
  group_by(SITE_PID,FACILITY_UID,INACTIVE_DATE) %>% 
  filter(n()>1)


## b) Archive Updated LTFU's that have RETURN_VALIDATE "Yes" or Died Date or > 6months
# (into Folder Historical_LTFU > Stage2)
## c) Archive Final Dataset for Next Update (Most recent inactive date taken)
# (into Folder Continue_LTFU)
df_final2 <- continue_determine(df_final)


## d) Import Partner Current Submissions
# (from Folder Submissions)
df_partner <- import_partnersubmissions()

  
## e) Merge Partner Entry to NDR
df_final3 <- merge_ndrdf_partnersub(df_final2, df_partner)

dupe_df3 <- df_final3 %>% 
  group_by(SITE_PID,FACILITY_UID) %>% 
  filter(n()>1)

## In case, partner submission tools are duplicating linelist (df_final3 > df_final2)
test <- df_final3 %>%
  group_by(SITE_PID, FACILITY_UID) %>%
  arrange(desc(INACTIVE_DATE), .by_group = TRUE ) %>%
  slice(1L) %>%
  ungroup()

test99 <- df_final3 %>%
  group_by(SITE_PID, FACILITY_UID) %>%
  arrange(desc(INACTIVE_DATE)) %>%
  slice(1L) %>%
  ungroup()

compare_df(test,test99)

## Other Duplication Tests
test2 <- df_final3 %>% group_by(SITE_PID, IMPLEMENTING_PARTNER, STATE, FACILITY_UID) %>% summarise(n()>1) %>% filter(`n() > 1` == T)
test3 <- df_final3 %>% group_by(SITE_PID) %>% summarise(n()>1) %>% filter(`n() > 1` == T)


testa <- df_final2 %>%
  group_by(SITE_PID, FACILITY_UID) %>%
  arrange(desc(INACTIVE_DATE), .by_group = TRUE) %>%
  slice(1L) %>%
  ungroup()
test2a <- df_final2 %>% group_by(SITE_PID, FACILITY_UID) %>% summarise(n()>1) %>% filter(`n() > 1` == T)
test3a<- df_final2 %>% group_by(SITE_PID) %>% summarise(n()>1) %>% filter(`n() > 1` == T)


######################## 2) Generate Partner Tools ########################
## a) Create Partner Tools
# (into Folder New Tools)
generatetools(test)


test %>% group_by(IMPLEMENTING_PARTNER,STATE)%>% summarise(n=n())



df_final %>% 
  filter(RETURN_VALIDATE == "Yes" & INACTIVE_TIME == "> 6 months") %>% 
  group_by(SITE_PID, FACILITY_UID) %>%
  arrange(desc(INACTIVE_DATE), .by_group = TRUE ) %>%
  slice(1L) %>%
  ungroup() %>% 
  group_by(IMPLEMENTING_PARTNER,STATE) %>% 
  summarise(n=n()) 




















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
######################## 1) Stage 1 Determination ######################## 
## a) Import Prev_LTFU Dataset 
# (from Folder Continue_LTFU > Stage2)
df <- ndr_read("./Continue_LTFU/Stage2/Continue_LTFU_stage2_2020_07_10.xlsx")

## b) Archive LTFU's > 6months 
# (into Folder Historical_LTFU > Stage1)
stage1archive(df)

## c) Export LTFU's for UMB to Update 
# (into Folder Continue_LTFU > Stage1_ForUMBUpdate)
stage1continue(df)

## aa) Deduplication for UMB Update 
# (into Folder Continue_LTFU > Stage1_ForUMBUpdate)
df2 <- df %>%
  group_by_at(vars(-NDR_PID)) %>%
  arrange(INACTIVE_DATE) %>%
  slice(1L) %>%
  ungroup()

dupe_df <- df %>% 
  group_by_at(vars(-NDR_PID)) %>% 
  filter(n()>1)

write.xlsx(df2, paste("./Continue_LTFU/Stage1_ForUMBUpdate/Continue_LTFU_stage1_", date, ".xlsx", sep = ""), asTable = TRUE)

## TODO: Retire Stage 1, roll >6 months criteria to Stage 2



