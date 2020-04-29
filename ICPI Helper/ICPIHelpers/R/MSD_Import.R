read_new_msd <-
  function(file,
           to_lower = TRUE,
           save_rds = TRUE,
           remove_txt = FALSE) {
    #ensure file ends in .txt
    if (stringr::str_detect(file, ".txt") == FALSE)
      file <- paste0(file, ".txt")

    #import
    df <- data.table::fread(file, sep = "\t", colClasses = "character", showProgress = FALSE)
    df <- tibble::as_tibble(df)

    #covert any QTR to double
    df <- df %>%
      dplyr::mutate_at(dplyr::vars(dplyr::matches("qtr", ignore.case = TRUE)), ~ as.double(.))%>%
      dplyr::mutate_at(dplyr::vars(dplyr::matches("targets", ignore.case = TRUE)), ~ as.double(.))%>%
      dplyr::mutate_at(dplyr::vars(dplyr::matches("cumulative", ignore.case = TRUE)), ~ as.double(.))

    #remove N/As now present in the file as of FY18Q2
    df <- df %>%
      dplyr::mutate_if(is.logical, ~ as.character(.)) %>% #converts any logicals created in mutate_all back to character
      dplyr::mutate_if(is.character, ~ ifelse(. == "", NA, .))

    #convert year to integer
    df <- dplyr::mutate(df, fiscal_year = as.integer(fiscal_year))

    #rename to lower for ease of use
    if (to_lower == TRUE)
      df <- dplyr::rename_all(df, ~ tolower(.))

    #save as rds
    newfile <- stringr::str_replace(file, "txt", "rds")
    if (save_rds == TRUE)
      saveRDS(df, newfile)

    #remove txt file
    if (remove_txt == TRUE)
      file.remove(file)

    return(df)
  }
