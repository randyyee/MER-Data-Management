## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MSD Functions for TXS
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_NET_NEW Adjustments by Aaron Chafetz (USAID) * Updated 12/3/20
##      TX_NET_NEW Adjustment FXN by Andrea Stewart (CDC)
## CREATION DATE: 6/8/2020
## UPDATE: 12/4/2020
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
    
    filter(indicator %in% c(
      "TX_CURR", 
      "TX_ML", 
      "TX_RTT",
      "TX_NEW", 
      "TX_PVLS",
      "SC_ARVDISP"
    ))
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


## ==================== TXS RECODE PERIOD & NEW TX INDICATOR LABELS ==================== 
recode_period_txdisagg <- function(msd_converted_long, prev_r, curr_r, curr_t){
  df <- msd_converted_long %>%
    filter(standardizeddisaggregate %in% c("Age Aggregated/Sex/HIVStatus",
                                           "Age/Sex/HIVStatus",
                                           "Age/Sex/ARTNoContactReason/HIVStatus",
                                           "Age/Sex/ARTCauseofDeath",
                                           "Age/Sex/ARTNoContactReason",
                                           "DispensedARVBottles",
                                           "Age/Sex/ARVDispense/HIVStatus",
                                           "Age/Sex/Indication/HIVStatus"
    )) %>%
    mutate(period = case_when(period == prev_r ~ "prev_result",
                              period == curr_r ~ "curr_result",
                              period == curr_t ~ "curr_target",
                              TRUE ~"remove")) %>%
    mutate(indicator =        paste(indicator, otherdisaggregate, sep="_")) %>%
    filter(period != "remove") %>%
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup() %>%
    mutate(period_range = curr_r)
}  


## ==================== TXS RECODE MOST CURRENT PRIORITIZATIONS ==================== 
recode_prioritizations <- function(recoded_period_txdisagg){
  df <- recoded_period_txdisagg %>% 
    filter(period == "curr_result") %>%
    select(c(orgunituid, snuprioritization, facilityprioritization)) %>%
    distinct()
  
  df2 <- select(recoded_period_txdisagg, -c(snuprioritization, facilityprioritization))
  
  df3 <- left_join(df2, df)
}


## ==================== TXS ADD SINGLE AGE COLUMN ====================
collapse_age <- function(recoded_prioritizations){
  
  df <- recoded_prioritizations %>%
    select(-ageasentered) %>%
    pivot_longer(cols=c("trendsfine", "trendssemifine", "trendscoarse"), 
                 names_to = "age_type", 
                 values_to = "age") %>%
    filter(!is.na(value))
}


## ==================== TXS REDO INDICATOR NAMES FOR WIDE PIVOT ====================
redo_indicator_name <- function(reformatted_age_sex){
  df <- reformatted_age_sex %>% 
    mutate(var_suffix = 
             case_when(
               period == "curr_result"  ~ "Now_R",
               period == "curr_target"  ~ "Now_T",
               period == "prev_result"  ~ "Prev_R")
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


## ==================== TXS REMOVE EXTRANEOUS COLUMNS & CLEAN ====================
txs_clean <- function(redone_indicator_name){
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
           facility,
           facilityprioritization,
           indicatortype,
           age_type,
           age,
           sex,
           period_range,
           varname,
           value) %>%
    group_by_if(is_character) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== TXS CONVERT WIDE ====================
txs_convert_wide <- function(txs_cleaned){
  df <- pivot_wider(txs_cleaned,
                    names_from = "varname",
                    values_from = "value") %>%
    na_if(0)
  
  df$SUMCOL <- rowSums(df[sapply(df, is.numeric)], na.rm = TRUE)
  
  df <- df %>%
    rename(period = period_range) %>%
    filter(SUMCOL != 0) %>%
    select(-SUMCOL)

}

## ==================== COMPOSED FUNCTION ====================
msd_df <- function(msd_txt){
  
  df <- msd_import(msd_txt)
  
  df <- msd_convert_long(df)
  
}


txs_generate <- function(msd_df, prevR, currR, currT){
  
  df <- recode_period_txdisagg(msd_df, prevR, currR, currT)
  
  df <- recode_prioritizations(df)
  
  df <- collapse_age(df)
  
  df <- redo_indicator_name(df)
  
  df <- txs_clean(df)
  
  df <- txs_convert_wide(df)
  
}