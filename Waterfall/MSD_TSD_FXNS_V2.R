## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## TITLE: MSD Functions for TXS
## AUTHOR: Randy Yee (pcx5@cdc.gov)
## DESCRIPTION: 
##      TSD Refactor from Imran Mujawar (CDC)'s original script
##      TX_NET_NEW Adjustments by Aaron Chafetz (USAID)
##      TX_NET_NEW Adjustment FXN by Andrea Stewart (CDC)
## CREATION DATE: 6/8/2020
## UPDATE: 11/17/2020
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
    
    filter(indicator %in% c("TX_CURR",
                            "TX_ML",
                            "TX_RTT",
                            "TX_NEW"
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
    mutate(period = case_when(period == prev_r ~ "prev_result",
                              period == curr_r ~ "curr_result",
                              period == curr_t ~ "curr_target",
                              TRUE ~"remove")) %>%
    mutate(indicator = ifelse(indicator == "TX_ML",
                              paste(indicator, otherdisaggregate, sep="_"),
                              indicator)) %>%
    filter(period != "remove") %>%
    group_by_if(is.character) %>% 
    summarize(value = sum(value, na.rm=T)) %>% 
    ungroup() %>%
    mutate(period_range = paste0(prev_r, "-", curr_r))
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
  
  ## TRENDSCOARSE RECODE
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
    filter(standardizeddisaggregate %in% c("Age Aggregated/Sex/HIVStatus",
                                           "Age/Sex/HIVStatus",
                                           "Age/Sex/ARTNoContactReason/HIVStatus",
                                           "Age/Sex/ARTCauseofDeath",
                                           "Age/Sex/ARTNoContactReason")
    ) %>%
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
    filter(SUMCOL != 0) %>%
    select(-SUMCOL) %>%
    select_if(names(.) %in% c("operatingunit",                                                         
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
                              "period_range",
                              
                              "TX_CURR_Prev_R",
                              "TX_CURR_Now_R",
                              "TX_CURR_Now_T",
                              
                              "TX_NEW_Now_R",                                                          
                              
                              "TX_RTT_Now_R",
                              "TX_ML_No Contact Outcome - Transferred Out_Now_R",                      
                              "TX_ML_No Contact Outcome - Lost to Follow-Up <3 Months Treatment_Now_R",
                              "TX_ML_No Contact Outcome - Lost to Follow-Up 3+ Months Treatment_Now_R",
                              "TX_ML_No Contact Outcome - Refused Stopped Treatment_Now_R",
                              "TX_ML_No Contact Outcome - Died_Now_R"
    ))
}

## ==================== COMPOSED FUNCTION ====================
txs_generate <- function(msd_txt, prevR, currR, currT){
  df <- msd_import(msd_txt)
  
  df <- msd_convert_long(df)
  
  df <- recode_period_txdisagg(df, prevR, currR, currT)
  
  df <- recode_prioritizations(df)
  
  df <- collapse_age(df)
  
  df <- redo_indicator_name(df)
  
  df <- txs_clean(df)
  
  df <- txs_convert_wide(df)
  
}




## ==================== NET_NEW_ADJ FUNCTION ====================
tx_net_new_adj <- function(msd_converted_long){
  
  msd_converted_long <- msd_converted_long %>%
    filter(indicator == "TX_CURR" & standardizeddisaggregate == "Total Numerator") %>%
    filter(str_detect(period,"qtr"))
    
  #store hierarchy for later
  df_mechnames <- msd_converted_long %>%
    distinct(mech_code, mech_name, primepartner, period) 
  
  df_org <- msd_converted_long %>% 
    distinct(operatingunit, countryname, snu1, psnu, facility, orgunituid)
  
  #limit df for analysis
  df <- msd_converted_long %>% 
    arrange(operatingunit, orgunituid, period) %>% 
    select(operatingunit, orgunituid, period, mech_code, fundingagency, value)

  #flag for lone obs
  df <- df %>% 
    complete(period, nesting(operatingunit, orgunituid, mech_code, fundingagency)) %>% 
    arrange(orgunituid, mech_code, period) %>% 
    group_by(orgunituid, mech_code) %>% 
    mutate(flag_loneobs = !is.na(value) & is.na(lead(value, order_by = period)) & is.na(lag(value, order_by = period))) %>% 
    ungroup() %>% 
    mutate(flag_loneobs = ifelse(period %in% c(min(df$period), max(df$period)), FALSE, flag_loneobs)) %>% 
    filter(!is.na(value))
  
  #flag multi mechanism site
  df <- df  %>% 
    group_by(orgunituid, period) %>% 
    mutate(flag_multimech_site = n() > 1) %>% 
    ungroup()
  
  #identify last site obs
  df <- df %>% 
    group_by(orgunituid) %>% 
    mutate(last_obs_site = max(period)) %>% 
    ungroup() 
  
  #flag sitexmech end
  df <- df %>% 
    group_by(orgunituid, mech_code) %>% 
    mutate(last_obs_sitexmech = max(period)) %>% 
    ungroup() %>% 
    mutate(flag_end_sitexmech = period == last_obs_sitexmech & period != max(df$period))
  
  #end type
  df <- df %>% 
    group_by(orgunituid) %>% 
    mutate(end_type = case_when(flag_end_sitexmech == TRUE & last_obs_sitexmech == last_obs_site ~ "Transition out of PEPFAR",
                                flag_end_sitexmech == TRUE & flag_multimech_site == TRUE ~ "Consolidate mutli-mechanism site",
                                flag_end_sitexmech == TRUE & fundingagency != lead(fundingagency, order_by = period) ~ "Transition to other agency",
                                flag_end_sitexmech == TRUE & mech_code != lead(mech_code, order_by = period) ~ "Transition to other mechanism")) %>% 
    ungroup()
  
  #identify agency transitions
  df <- df %>% 
    group_by(orgunituid) %>%
    mutate(agency_exiting = case_when(end_type == "Transition to other agency" ~ fundingagency),
           agency_inheriting = case_when(end_type == "Transition to other agency" ~ lead(fundingagency, order_by = period))) %>% 
    ungroup()
  
  #method (adjusted NN or traditional for multi-mech sites)
  df <- df %>% 
    group_by(orgunituid, mech_code) %>% 
    mutate(method = case_when(flag_multimech_site == TRUE | lag(flag_multimech_site) == TRUE ~ "standard",
                              TRUE ~ "adjusted")) %>% 
    ungroup()
  
  # MERGE META --------------------------------------------------------------
  
  #adapting AC's code that binds hierarchy info 
  #mechanism info
  
  df <- df %>%
    left_join(df_mechnames)
  
  #reorder
  df <- df %>% 
    select(operatingunit, orgunituid, fundingagency, 
           mech_code, mech_name, primepartner, everything())
  
  #bring org hierarchy data in
  df <- df %>% 
    select(-operatingunit) %>% 
    right_join(df_org, ., by = "orgunituid")
  
  #this is where AC's code writes df as a csv
  #excluding CSV export
  
  #rename value
  df <- rename(df, tx_curr = value)
  
  #remove flags
  df_noflag <- select(df, orgunituid:tx_curr, method)
  
  #store var order for export
  lst_order <- names(df_noflag)
  lst_order <- lst_order[lst_order != "method"]
  
  # CALCULATE NORMAL NET NEW ------------------------------------------------
  
  #create a full set of periods for calculating NET NEW
  df_complete_mechxsite <- df_noflag %>%
    select(-method) %>% 
    complete(period, nesting(orgunituid, mech_code), fill = list(tx_curr = 0)) %>% 
    group_by(mech_code, orgunituid) %>% 
    fill(operatingunit, countryname, snu1, psnu, facility, 
         fundingagency, mech_name, primepartner,
         .direction = "downup") %>% 
    ungroup() %>% 
    arrange(operatingunit, orgunituid, mech_code, period)
  
  #calc normal NET_NEW
  df_complete_nn_orig <- df_complete_mechxsite %>%
    group_by(orgunituid, mech_code) %>% 
    mutate(tx_net_new = tx_curr - lag(tx_curr, order_by = period)) %>% 
    ungroup()
  
  # CALCULATE ADJUSTED NET NEW ----------------------------------------------
  
  #create a full set of periods for calculating NET NEW
  df_complete_site <- df_noflag %>% 
    filter(method == "adjusted") %>% 
    select(-method) %>% 
    complete(period, nesting(orgunituid), fill = list(tx_curr = 0)) %>% 
    group_by(orgunituid) %>% 
    fill(operatingunit, countryname, snu1, psnu, facility, 
         fundingagency, mech_code, mech_name, primepartner,
         .direction = "downup") %>% 
    ungroup() %>% 
    arrange(operatingunit, orgunituid, mech_code, period)
  
  #calc adjusted NET_NEW
  df_complete_nn_adj <- df_complete_site %>% 
    select(-c(operatingunit:primepartner, -mech_code)) %>% 
    arrange(orgunituid, period) %>% 
    group_by(orgunituid) %>%
    mutate(tx_curr_lag_site = lag(tx_curr, order_by = period),
           tx_net_new_adj = tx_curr -  lag(tx_curr, order_by = period)) %>%
    ungroup() 
  
  rm(df_noflag, df_complete_site)
  
  # JOIN BOTH NET_NEW TYPES -------------------------------------------------
  
  #cleanup for merging 
  df_complete_nn_adj <- df_complete_nn_adj %>% 
    select(-tx_curr) %>% 
    filter_at(vars(tx_net_new_adj, tx_curr_lag_site), any_vars(!is.na(.) & . != 0))
  
  #join
  df_complete_nn_both <- df_complete_nn_orig %>% 
    left_join(df_complete_nn_adj, by = c("period", "orgunituid", "mech_code"))
  
  #remove any with all 0/NAs
  df_nn <- df_complete_nn_both %>% 
    filter_at(vars(tx_curr, tx_net_new, tx_net_new_adj), 
              any_vars(!is.na(.) & . != 0))
  
  #replace artifically created zeros for TX_CURR
  df_nn <- mutate_at(df_nn, vars(tx_curr, tx_curr_lag_site), ~ na_if(., 0))
  
  #reorder
  df_nn <- select(df_nn, all_of(lst_order), starts_with("tx"))
  
  
  # ENUMERATE TRANFERS ------------------------------------------------------
  
  #calc transfers in and out by partner
  df_nn <- df_nn %>% 
    mutate(tx_xfer = case_when(is.na(tx_curr) & tx_net_new < 0 ~ tx_net_new)) %>% 
    group_by(orgunituid, period) %>%
    mutate(tx_xfer = case_when(n() == 2 ~ tx_xfer)) %>% 
    fill(tx_xfer) %>% 
    ungroup() %>% 
    mutate(tx_xfer = ifelse(tx_net_new > 0, -tx_xfer, tx_xfer)) 
  
  
  # ADD FLAGS BACK IN -------------------------------------------------------
  
  #select flags to merge on from orig df
  df_flags <- select(df, orgunituid, mech_code, period, flag_loneobs:method)
  
  #merge onto nn
  df_nn_flags <- left_join(df_nn, df_flags, by = c("orgunituid", "mech_code", "period"))
  
  #fill missing (ie where mech has neg net_new after it transitions)
  df_nn_flags <- df_nn_flags %>% 
    group_by(mech_code, orgunituid) %>% 
    fill(flag_loneobs:last_obs_sitexmech, method, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(flag_end_sitexmech = ifelse(is.na(flag_end_sitexmech), FALSE, flag_end_sitexmech))
  
  # ADD COMBINED NET NEW ----------------------------------------------------
  
  #var that includes both adj and traditional (for multi)
  df_nn_flags <- df_nn_flags %>% 
    mutate(tx_net_new_adj_plus = ifelse(method == "adjusted", tx_net_new_adj, tx_net_new)) %>% 
    relocate(tx_net_new_adj_plus, .after = tx_net_new_adj)
  
  return(df_nn_flags)
  
}

## ==================== TOTAL NUMERATOR WATERFALL WITH NET_NEW_ADJ ====================
txs_w_netnewadj <- function(txs_df, netnew_df){

  df <- left_join(txs_df %>%
                    filter(age_type == "trendscoarse") %>%
                    select(-age_type, -age, -sex) %>%
                    group_by_if(is.character) %>%
                    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%
                    ungroup() %>%
                    separate(period_range, into = c("previous", "period"), sep = "-", remove = F), 
                  netnew_df, 
                  by = c("operatingunit", 
                         "countryname", 
                         "snu1", 
                         "psnu", 
                         "orgunituid", 
                         "fundingagency", 
                         "primepartner", 
                         "mech_name", 
                         "mech_code",
                         "facility",
                         "period")) %>%
    select(-previous, -period)
}

## ==================== COMPOSED FUNCTION 2 ====================
txs_adj_generate <- function(msd_txt, prevR, currR, currT){
  df_txs <- txs_generate(msd_txt, prevR, currR, currT)
  df_tnna <- tx_net_new_adj(msd_convert_long(msd_import(msd_txt)))
  df <- txs_w_netnewadj(df_txs, df_tnna)
  
  df$SUMCOL <- rowSums(df[sapply(df, is.numeric)], na.rm = TRUE)
  
  df <- df %>%
    filter(SUMCOL != 0) %>%
    select(-SUMCOL)
}
