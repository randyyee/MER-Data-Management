convert_new_msd_HITS <-
  function(msd)
    {
      msd <- tibble::rowid_to_column(msd)

      msd <- msd %>%
        gather("period","value",targets:cumulative) %>%
        mutate(fiscalperiod = paste(fiscal_year,period)) %>%
        select(-fiscal_year, -period)


      msd <- msd %>%
        group_by_at(vars(orgunituid:modality, fiscalperiod)) %>% #for site level start at orgunituid
        summarise(Value = sum(value, na.rm = TRUE)) %>%
        ungroup() %>%
        spread(fiscalperiod,Value)

      return(msd)
    }
