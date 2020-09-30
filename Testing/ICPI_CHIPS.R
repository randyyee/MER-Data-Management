#### CHIPS DATASET TRANSFORMATION (PSNU-IM) 
#### FORMAT: LONG
#### AUTHOR: Randy Yee
#### DATE: 11/7/2019
rm(list = ls())

library(tidyverse)
library(openxlsx)
library(ICPIHelpers)


period <- "CleanQ1"

#######################################################################################################################################
ptm <- proc.time()
# Import manually into R
new_df <- read_new_msd(file.choose(),
                      save_rds = FALSE,
                      remove_txt = FALSE)


ou_list <- unique(new_df$operatingunit)

#remain_ou_list <- ou_list[c(2,7,35,36)]
#ou_list <- ou_list[c(-2,-7,-35,-36)]


for (ou in ou_list){
  
  newer_df <- new_df %>% filter(operatingunit == ou) %>%
    
    # Subsetting indicators 
    filter(indicator %in% c("HTS_TST",
                            "HTS_TST_POS", 
                            #"HTS_TST_NEG", 
                            "TX_NEW",
                            "HTS_INDEX")) %>%
    
    # Drop columns
    select(-c("region", 
              "regionuid",
              "snu1uid",
              "operatingunituid", 
              "countryname",
              "psnuuid",
              #"mechanismuid", 
              #"mechanismid", 
              "disaggregate",
              "coarsedisaggregate"))%>%
    
    # Change all 0 values to NA
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
  
  # Filter if statements
  final <- filter(newer_df, (indicator == "TX_NEW" & standardizeddisaggregate == "Total Numerator") |
                    (indicator == "TX_NEW" & standardizeddisaggregate == "Age/Sex/HIVStatus") |
                    
                    #HTS_TST Requirements
                    (indicator == "HTS_TST" & standardizeddisaggregate == "Total Numerator") | 
                    (indicator == "HTS_TST" & standardizeddisaggregate == "MostCompleteAgeDisagg") | 
                    (indicator == "HTS_TST" & standardizeddisaggregate == "Modality/MostCompleteAgeDisagg") | 
                    (indicator == "HTS_TST" & standardizeddisaggregate == "Modality/Age/Sex/Result") | 
                    
                    
                    
                    #Requirements for HTS_TST_POS
                    (indicator == "HTS_TST_POS" & standardizeddisaggregate == "Total Numerator") | 
                    (indicator == "HTS_TST_POS" & standardizeddisaggregate == "MostCompleteAgeDisagg") | 
                    (indicator == "HTS_TST_POS" & standardizeddisaggregate == "Modality/MostCompleteAgeDisagg") | 
                    (indicator == "HTS_TST_POS" & standardizeddisaggregate == "Modality/Age/Sex/Result") | 
                    
                    
                    #Requirements for HTS_TST_NEG
                    (indicator == "HTS_TST_NEG" & standardizeddisaggregate == "Total Numerator") | 
                    (indicator == "HTS_TST_NEG" & standardizeddisaggregate == "MostCompleteAgeDisagg") | 
                    (indicator == "HTS_TST_NEG" & standardizeddisaggregate == "Modality/MostCompleteAgeDisagg") | 
                    (indicator == "HTS_TST_NEG" & standardizeddisaggregate == "Modality/Age/Sex/Result") |
                    
                    #All HTS_INDEX
                    (indicator == "HTS_INDEX")
  )
  
  
  
  ###########################################
  # Wide Transformation #####################
  ###########################################
  final <- convert_new_msd_CHIPS(final)
  
  final <- final %>% rename(
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
    "FY2020APR" = "2020 cumulative",
    "FY2020_TARGETS" = "2020 targets"
    ) %>%
    
    
    select("operatingunit", 
           "snu1", 
           "psnu", 
           "snuprioritization", 
           "fundingagency", 
           "primepartner", 
           "mech_name", 
           "typemilitary", 
           "indicator",
           "numeratordenom",
           "indicatortype",
           "ageasentered",
           "trendsfine",
           "trendssemifine",
           "trendscoarse",
           "sex",
           "statushiv",
           "standardizeddisaggregate",
           "otherdisaggregate",
           "modality",
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
           #"FY2020Q2",
           #"FY2020Q3",
           #"FY2020Q4",
           "FY2020APR",
           "FY2020_TARGETS"
    ) %>%
    na_if(0) %>%
    filter(!is.na(FY2018_TARGETS)
           | !is.na(FY2018Q1)
           | !is.na(FY2018Q2)
           | !is.na(FY2018Q3)
           | !is.na(FY2018Q4)
           | !is.na(FY2018APR)
           | !is.na(FY2019_TARGETS)
           | !is.na(FY2019Q1)
           | !is.na(FY2019Q2)
           | !is.na(FY2019Q3)
           | !is.na(FY2019Q4)
           | !is.na(FY2019APR)
           | !is.na(FY2020Q1)
           | !is.na(FY2020APR)
           | !is.na(FY2020_TARGETS))
  
  
  write.xlsx(final, file=paste("C:/Users/pcx5/Documents/HTS/CHIPS/2020/", period,"/Datasets/HTS_", ou, "_", period,".xlsx", sep = ""), keepNA = FALSE)
  
}

proc.time() - ptm





