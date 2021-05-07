## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MSD Functions for Clinical Cascade Completeness
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: Long Dataset 
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
    
    filter(indicator %in% c( 
                            "HTS_TST",
                            "TX_CURR",
                            "TX_ML",
                            "TX_RTT",
                            "TX_NEW", 
                            "TX_PVLS")) %>%
    select(-targets,-cumulative)
}


## ==================== MSD CONVERT LONG ====================
msd_convert_long <- function(msd){
  
  df <- pivot_longer(msd,
                     qtr1:qtr4,
                     names_to = "period",
                     values_to = "value")
  
  df <- df %>% 
    unite("period", 
          c("fiscal_year", "period"),
          sep = " ", 
          remove = T)
    
}


## ==================== QTR RECODE ==================== 
recode_qtrs <- function(msd_converted_long){
  df <- msd_converted_long %>%
    mutate(Qtr = case_when(period == "2019 qtr1" ~0,
                            period == "2019 qtr2" ~1,
                            period == "2019 qtr3" ~2,
                            period == "2019 qtr4" ~3,
                            period == "2020 qtr1" ~4,
                            period == "2020 qtr2" ~5,
                           period == "2020 qtr3" ~6
                            )) %>%
    filter(!is.na(Qtr))
}


## ==================== INDICATOR & DISAGG FILTER ==================== 
filter_ind_disagg <- function(recoded_qtrs, ind, disagg){
  df <- recoded_qtrs %>%
    filter(indicator == ind & standardizeddisaggregate == disagg)%>%
    group_by_at(vars(-value)) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== SITE REPORTING MATRIX ==================== 
site_reporting <- function(filtered_ind_disagg){
  df <- filtered_ind_disagg %>%
    select(c(sitename,
             orgunituid,
             psnu,
             period,
             indicator,
             standardizeddisaggregate,
             value))%>%
    group_by_at(vars(-value)) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== MECH MATRIX ==================== 
mech_matrix <- function(filtered_ind_disagg){
  df <- filtered_ind_disagg%>%
    select(c(sitename,
             orgunituid,
             psnu,
             mech_name,
             period,
             indicator,
             standardizeddisaggregate,
             value))%>%
    filter(value != 0) %>%
    filter(mech_name != "Dedup") %>%
    group_by_at(vars(-value)) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== PARTNER MATRIX ==================== 
primep_matrix <- function(filtered_ind_disagg){
  df <- filtered_ind_disagg%>%
    select(c(sitename,
             orgunituid,
             psnu,
             primepartner,
             period,
             indicator,
             standardizeddisaggregate,
             value))%>%
    group_by_at(vars(-value)) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}


## ==================== AGENCY MATRIX ==================== 
agency_matrix <- function(filtered_ind_disagg){
  df <- filtered_ind_disagg%>%
    select(c(sitename,
             orgunituid,
             psnu,
             fundingagency,
             period,
             indicator,
             standardizeddisaggregate,
             value))%>%
    group_by_at(vars(-value)) %>%
    summarise(value = sum(value, na.rm=T)) %>% 
    ungroup()
}
