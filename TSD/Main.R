## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MAIN TSD WRANGLE
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: TSD Refactor - Original Script by Imran Mujawar
## CREATION DATE: 6/8/2020
## UPDATE: 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)

source("MSD_TSD_FXNS.R")
#source("NATSUBNAT_TSD_FXNS.R")


## ==================== TESTS ==================== 
test <- msd_import("preview_site.txt") # CHECKED

test1 <- msd_convert_long(test) # CHECKED

test2 <- recode_period_txdisagg(test1, "2019_cumulative", "2020_cumulative", "2020_targets")

test3 <- recode_prioritizations(test2)

test4 <- collapse_age(test3)

test5 <- reformat_age_sex(test4)

test6 <- redo_indicator_name(test5)

test7 <- tsd_clean(test6)

test8 <- tsd_convert_wide(test7)


## ==================== MAIN ====================
df <- tsd_generate("preview_site.txt", #msd_txt
                   "2019_cumulative", #prevR
                   "2020_cumulative", #currR
                   "2020_targets") #currT

