#### HITS DATASET TRANSFORMATION (SITE-IM) 
#### FORMAT: LONG
#### AUTHOR: Randy Yee
#### REV DATE: 03/04/2020

library(tidyverse)
library(openxlsx)
library(ICPIHelpers)

setwd("C:/Users/pcx5/Desktop/MSD_FY20Q1_Site")
period <- "CleanQ1"


ou_list <- list.files() # Remove Cambodia,
#ou_list2 <- ou_list[c(-2, -7, -35,-36)]

for (ou in ou_list) {
  new_df <- read_new_msd(ou, save_rds = FALSE) %>%
    filter(indicator %in% c("HTS_TST",
                            "HTS_TST_POS")
    ) %>%
    select(-c("region", 
              "regionuid",
              "operatingunituid", 
              "countryname", 
              "coarsedisaggregate")
    ) %>%
    filter(standardizeddisaggregate %in% c("Modality/MostCompleteAgeDisagg",
                                           "ServiceDeliveryPoint",
                                           "ServiceDeliveryPoint/Result",
                                           "Total Numerator",
                                           "Modality/Age/Sex/Result",
                                           "Modality/Age Aggregated/Sex/Result")
    ) %>%
    na_if(0)%>%
    
    # Grouping to recreate Wide Format 
    #(i.e. each observation-row will have all their 
    # time periods grouped together)
    group_by(operatingunit, 
             indicator, 
             standardizeddisaggregate, 
             categoryoptioncomboname, 
             modality, 
             fundingagency,
             primepartner)%>%
    
    # Keep observations that have values for either TARGETS or Cumulative
    filter(!is.na(cumulative) | 
             !is.na(targets))%>%
    
    ungroup()
  
  new_df <- convert_new_msd_HITS(new_df)
  new_df <- new_df %>%
    
    rename(
      # "FY2017Q1" = "2017 qtr1",
      # "FY2017Q2" = "2017 qtr2",
      # "FY2017Q3" = "2017 qtr3",
      # "FY2017Q4" = "2017 qtr4",
      # "FY2017APR" = "2017 cumulative",
      # "FY2017_TARGETS" = "2017 targets",
      
      "FY2018Q1" = "2018 qtr1",
      "FY2018Q2" = "2018 qtr2",
      "FY2018Q3" = "2018 qtr3",
      "FY2018Q4" = "2018 qtr4",
      "FY2018APR" = "2018 cumulative",
      "FY2018_TARGETS" = "2018 targets",
      
      "FY2019Q1" = "2019 qtr1",
      "FY2019Q2" = "2019 qtr2",
      "FY2019Q3" = "2019 qtr3",
      "FY2019Q4" = "2019 qtr4",
      "FY2019_TARGETS" = "2019 targets",
      "FY2019APR" = "2019 cumulative",
      
      "FY2020Q1" = "2020 qtr1",
      "FY2020Q2" = "2020 qtr2",
      "FY2020Q3" = "2020 qtr3",
      "FY2020Q4" = "2020 qtr4",
      "FY2020_TARGETS" = "2020 targets",
      "FY2020APR" = "2020 cumulative",
      
      "AgeFine" = "trendsfine",
      "AgeSemiFine" = "trendssemifine",
      "AgeCoarse" = "trendscoarse",
      "resultstatus" = "statushiv")
  
  
  new_df <- new_df %>%
    mutate( 
      siteprioritization = case_when(
        orgunituid == facilityuid ~ facilityprioritization,
        orgunituid == communityuid ~ communityprioritization,
        orgunituid == psnuuid ~ snuprioritization)
    ) %>% 
    na_if(0) %>%
    rename(siteid = orgunituid) %>%
    filter(
      # !is.na(FY2017_TARGETS)
      #      | !is.na(FY2017Q1)
      #      | !is.na(FY2017Q2)
      #      | !is.na(FY2017Q3)
      #      | !is.na(FY2017Q4)
      #      | !is.na(FY2017APR)
           # | 
             !is.na(FY2018_TARGETS)
           | !is.na(FY2018Q1)
           | !is.na(FY2018Q2)
           | !is.na(FY2018Q3)
           | !is.na(FY2018Q4)
           | !is.na(FY2018APR)
           | !is.na(FY2019Q1)
           | !is.na(FY2019Q2)
           | !is.na(FY2019Q3)
           | !is.na(FY2019Q4)
           | !is.na(FY2019APR)
           | !is.na(FY2019_TARGETS)
           | !is.na(FY2020Q1)
           | !is.na(FY2020APR)
           | !is.na(FY2020_TARGETS)
    ) %>%
    select("operatingunit",
           "snu1",
           "psnu",
           "snuprioritization",
           "primepartner", 
           "fundingagency",             
           "mech_code",             
           "mech_name",             
           "indicator",         
           "numeratordenom",        
           "indicatortype",               
           "disaggregate",        
           "resultstatus",    
           # "FY2017_TARGETS",        
           # "FY2017Q1",       
           # "FY2017Q2",    
           # "FY2017Q3",       
           # "FY2017Q4",       
           # "FY2017APR",
           "FY2018_TARGETS",               
           "FY2018Q1",       
           "FY2018Q2",       
           "FY2018Q3",       
           "FY2018Q4",       
           "FY2018APR",  
           "FY2019_TARGETS",        
           "FY2019Q1",
           "FY2019Q2",
           "FY2019Q3",
           "FY2019Q4",
           "FY2019APR",
           "FY2020Q1",
           "FY2020APR",
           "FY2020_TARGETS",
           "siteid",
           "sitetype",
           "siteprioritization",
           "sitename",
           "standardizeddisaggregate",
           "modality") 
  
  new_df <- new_df[!
                     (
                       (new_df$standardizeddisaggregate == "Modality/Age/Sex/Result" & 
                          is.na(new_df$FY2018Q1)& 
                          is.na(new_df$FY2018Q2)& 
                          is.na(new_df$FY2018Q3)& 
                          is.na(new_df$FY2018Q4)& 
                          is.na(new_df$FY2019Q1)&
                          is.na(new_df$FY2019Q2)&
                          is.na(new_df$FY2019Q3)&
                          is.na(new_df$FY2019Q4)&
                          is.na(new_df$FY2020Q1))|
                         (new_df$standardizeddisaggregate == "Modality/Age Aggregated/Sex/Result" & 
                            is.na(new_df$FY2018Q1)& 
                            is.na(new_df$FY2018Q2)& 
                            is.na(new_df$FY2018Q3)& 
                            is.na(new_df$FY2018Q4)&
                            is.na(new_df$FY2019Q1)&
                            is.na(new_df$FY2019Q2)&
                            is.na(new_df$FY2019Q3)&
                            is.na(new_df$FY2019Q4)&
                            is.na(new_df$FY2020Q1))
                     ),
                   ]
  ou_name <- unique(new_df$operatingunit)
  write.xlsx(new_df, file=paste("C:/Users/pcx5/Documents/HTS/HITS/2020/", period,"/Datasets/HTS_", ou_name, "_", period,".xlsx", sep = ""), keepNA = FALSE)
}
