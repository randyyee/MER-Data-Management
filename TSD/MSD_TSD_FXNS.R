## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MSD Functions for TSD
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: TSD Refactor - Original Script by Imran Mujawar 
## CREATION DATE: 6/8/2020
## UPDATE: 
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`%ni%` <- Negate(`%in%`) 

## ==================== MSD IMPORT ====================
msd_import <- function(msd_txt){
  
  df <- read_delim(msd_txt, 
                   "\t", 
                   escape_double = FALSE,
                   trim_ws = TRUE,
                   col_types = cols(.default = col_character(), 
                                    targets = col_double(),
                                    qtr1 = col_double(),
                                    qtr2 = col_double(),
                                    qtr3 = col_double(),
                                    qtr4 = col_double(),
                                    cumulative = col_double()
                                    ) 
                   ) %>%
    
    filter(indicator %in% c("HTS_TST_POS", 
                            "HTS_TST",
                            "TX_CURR",
                            "TX_ML",
                            "TX_RTT",
                            "TX_NEW", 
                            "TX_PVLS"))
}


## ==================== MSD CONVERT LONG ====================
msd_convert_long <- function(msd){
  
  df <- pivot_longer(msd,
                     targets:cumulative,
                     names_to = "period",
                     values_to = "value")
  
  df <- unite(df, 
              "period", 
              c("fiscal_year", "period"),
              sep = "_", 
              remove = T)
}


## ==================== TSD RECODE PERIOD & NEW TX INDICATOR LABELS ==================== 
recode_period_txdisagg <- function(msd_converted_long, prev_r, curr_r, curr_t){
  df <- msd_converted_long %>%
    mutate(period = case_when(period == prev_r ~ "prev_result",
                              period == curr_r ~ "curr_result",
                              period == curr_t ~ "curr_target",
                              TRUE ~"remove")) %>%
    mutate(indicator = ifelse(indicator == "TX_ML",
                              paste(indicator, standardizeddisaggregate,sep="_"),
                              indicator)) %>%
    filter(period != "remove") %>%
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
}  


## ==================== TSD RECODE MOST CURRENT PRIORITIZATIONS ==================== 
recode_prioritizations <- function(recoded_period_txdisagg){
  df <- recoded_period_txdisagg %>% 
    filter(period == "curr_result") %>%
    select(c(orgunituid,snuprioritization,facilityprioritization)) %>%
    distinct()
  
  df2<- select(recoded_period_txdisagg, -c(snuprioritization,facilityprioritization))
  
  df3 <- left_join(df2, df)
}


## ==================== TSD ADD SINGLE AGE COLUMN ====================
collapse_age <- function(recoded_prioritizations){
  
  ## TRENDSCOARSE RECODE
  df <- recoded_prioritizations %>%
    mutate(F_C = case_when(
      standardizeddisaggregate %in% c("Total Numerator",
                                      "Total Denominator") ~ "No disaggregation",
      standardizeddisaggregate %in% c("Modality/Age Aggregated/Sex/Result",
                                      "Modality/Age/Sex/Result",
                                      "Age Aggregated/Sex/HIVStatus",
                                      "Age/Sex/HIVStatus",
                                      "Age Aggregated/Sex/Indication/HIVStatus",
                                      "Age/Sex/Indication/HIVStatus",
                                      "Age/Sex/ARTNoContactReason/HIVStatus",
                                      "Age/Sex/ARTCauseofDeath",
                                      "Age/Sex/ARTNoContactReason") ~ "MCAD/Coarse")) %>% 
    rename(age = trendscoarse) %>% 
    filter(!is.na(F_C))
  
  ## TRENDSSEMIFINE RECODE
  df1 <- recoded_prioritizations %>% 
    mutate(F_C = case_when(
      standardizeddisaggregate %in% c("Modality/Age/Sex/Result",
                                      "Age/Sex/HIVStatus",
                                      "Age/Sex/Indication/HIVStatus",
                                      "Age/Sex/ARTNoContactReason/HIVStatus",
                                      "Age/Sex/ARTCauseofDeath",
                                      "Age/Sex/ARTNoContactReason") ~ "Semi-Fine")) %>% 
    rename(age = trendssemifine) %>% 
    filter(!is.na(F_C))
  
  ## TRENDSFINE RECODE
  df2 <- recoded_prioritizations %>%
    mutate(F_C = case_when(
      standardizeddisaggregate %in% c("Modality/Age/Sex/Result",
                                      "Age/Sex/HIVStatus",
                                      "Age/Sex/Indication/HIVStatus",
                                      "Age/Sex/ARTNoContactReason/HIVStatus",
                                      "Age/Sex/ARTCauseofDeath",
                                      "Age/Sex/ARTNoContactReason") ~ "Fine")) %>% 
    rename(age = trendsfine) %>% 
    filter(!is.na(F_C))
  
  ## COMBINE ALL TRENDS
  df_combine <- bind_rows(df,df1,df2)
}


## ==================== TSD RECODE AGE & SEX BASED ON F_C ====================
reformat_age_sex <- function(collapsed_tsd_age){
  df <- collapsed_tsd_age %>% 
    mutate(age = case_when(
      F_C == "No disaggregation" ~ "",
      age == "" & F_C %in% c("MCAD/Coarse","Fine","Semi-Fine") ~ "Unknown Age",
      TRUE ~ age)
    ) %>% 
    mutate(sex = case_when(
      F_C == "No disaggregation" ~ "",
      F_C %in% c("MCAD/Coarse","Fine", "Semi-Fine") & age %in% c("01-04","01-09","05-09","05-14", 
                                             "10-14","<01","<05","<15") ~ "Unknown Sex",
      age == "" & F_C %in% c("MCAD/Coarse","Fine", "Semi-Fine") ~ "Unknown Sex",
      TRUE ~ sex)
    ) 
  
  ## AGGREGATE AGE-SEX TOTALS
  df <- df %>% 
    select(-c(trendscoarse,trendssemifine,trendsfine)) %>%
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TSD REDO INDICATOR NAMES FOR WIDE PIVOT ====================
redo_indicator_name <- function(reformatted_age_sex){
  df <- reformatted_age_sex %>% 
    mutate(var_suffix = 
             case_when(
               indicator %ni% c("TX_PVLS") & period=="curr_result"  ~ "Now_R",
               indicator %ni% c("TX_PVLS") & period=="curr_target"  ~ "Now_T",
               indicator %ni% c("TX_PVLS") & period=="prev_result"  ~ "Prev_R",
               indicator %in% c("TX_PVLS") & numeratordenom=="N" & period=="curr_result" ~ "Now_N",
               indicator %in% c("TX_PVLS") & numeratordenom=="D" & period=="curr_result" ~ "Now_D")
    ) %>% 
    filter(!is.na(var_suffix)) %>%
    unite("varname", 
          c(indicator, var_suffix),
          sep = "_", 
          remove = T) %>%
    group_by_if(is.character) %>%
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TSD REMOVE EXTRANEOUS COLUMNS & CLEAN ====================
tsd_clean <- function(redone_indicator_name){
  df <- redone_indicator_name %>%
    select(operatingunit,
           countryname,
           snu1,
           snuprioritization,
           psnu,
           psnuuid,
           sitetype,
           sitename,
           orgunituid,
           fundingagency,
           primepartner,
           mech_name,
           mech_code,
           facilityprioritization,
           F_C,
           age,
           sex,
           varname,
           value) %>%
    group_by_if(is_character) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TSD CONVERT WIDE ====================
tsd_convert_wide <- function(tsd_cleaned){
  df <- pivot_wider(tsd_cleaned,
                    names_from = "varname",
                    values_from = "value") %>%
    na_if(0)
  
  df$SUMCOL <- rowSums(df[sapply(df, is.numeric)], na.rm = TRUE)
  
  df <- df %>%
    filter(SUMCOL != 0) %>%
    select(-SUMCOL)
}


## ==================== COMPOSED FUNCTION ====================
tsd_generate <- function(msd_txt, prevR, currR, currT){
  df <- msd_import(msd_txt)
  
  df1 <- msd_convert_long(df)
  
  df2 <- recode_period_txdisagg(df1, prevR, currR, currT)
  
  df3 <- recode_prioritizations(df2)
  
  df4 <- collapse_age(df3)
  
  df5 <- reformat_age_sex(df4)
  
  df6 <- redo_indicator_name(df5)
  
  df7 <- tsd_clean(df6)
  
  df8 <- tsd_convert_wide(df7)
}
