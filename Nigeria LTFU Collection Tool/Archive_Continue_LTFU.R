#### Nigeria LTFU Partner Collection Tool Generation
#### DETERMINE LTFUs TO ARCHIVE (RETURN_VALIDATE == Yes | DIED_NDR | > 6 months) OR UPDATE
#### AUTHOR: Randy Yee (PCX5@cdc.gov)
#### CREATION DATE: 7/9/2019

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ndr_wrangle <- function(newLTFU_df, prevLTFU_df, date_data_pull){
  df1 <-  ndr_read(newLTFU_df)
  
  print("New LTFU's by implementing partner and state:")
  print(df1 %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  print("Distinct new LTFU's by implementing partner and state:")
  print(df1 %>% distinct(SITE_PID,FACILITY_UID,IMPLEMENTING_PARTNER,STATE) %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  
  df2 <- ndr_read(prevLTFU_df) %>%
    mutate(DATA_PULL = as.POSIXct(date_data_pull, tz = "UTC")) %>% # "2020-07-05"
    group_by(SITE_PID, FACILITY_UID) %>%
    arrange(desc(INACTIVE_DATE), .by_group = TRUE ) %>%
    slice(1L) %>%
    ungroup()
    
  print("Continue LTFU's by implementing partner and state:")
  print(df2 %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  print("Distinct continue LTFU's by implementing partner and state:")
  print(df2 %>% distinct(SITE_PID,FACILITY_UID,IMPLEMENTING_PARTNER,STATE) %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  
  
  df3 <- bind_rows(df1, df2)
  
  
  print(colnames(df3))
  print("Counts by implementing partner and state:")
  print(df3 %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
        )
  print("Distinct counts by implementing partner and state:")
  print(df3 %>% distinct(SITE_PID,FACILITY_UID,IMPLEMENTING_PARTNER,STATE) %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
        )
  
  return(df3)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

archive_determine <- function(df){
  
  df1 <- df %>% 
    filter(RETURN_VALIDATE == "Yes" | !is.na(DIED_NDR) | INACTIVE_TIME == "> 6 months") 
  
  print("Counts of RETURN_VALIDATE == Yes")
  print(df %>% 
          filter(RETURN_VALIDATE == "Yes") %>% 
          group_by(IMPLEMENTING_PARTNER,STATE) %>% 
          summarise(n = n())
  )
  print("Counts of DIED_NDR")
  print(df %>% 
          filter(!is.na(DIED_NDR)) %>% 
          group_by(IMPLEMENTING_PARTNER,STATE) %>% 
          summarise(n = n())
  )
  print("Counts of INACTIVE_TIME == > 6 months")
  print(df %>% 
          filter(INACTIVE_TIME == "> 6 months") %>% 
          group_by(IMPLEMENTING_PARTNER,STATE) %>% 
          summarise(n = n())
  )
  print("Total # of Removed (allowing for PID overlap)")
  print(df %>% 
          group_by(SITE_PID, FACILITY_UID) %>%
          arrange(desc(INACTIVE_DATE), .by_group = TRUE ) %>%
          slice(1L) %>%
          ungroup() %>%
          filter(RETURN_VALIDATE == "Yes" | !is.na(DIED_NDR) | INACTIVE_TIME == "> 6 months") %>% 
          group_by(IMPLEMENTING_PARTNER,STATE) %>% 
          summarise(n = n())
  )
  
  
  
  write.xlsx(df1, 
             paste("./Historical_LTFU/Historical_LTFU_Archive_", date, ".xlsx", sep = ""), 
             asTable = TRUE)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

continue_determine <- function(df){
  
  archive_determine(df)

  df1 <- df %>%
    group_by(SITE_PID, FACILITY_UID) %>%
    arrange(desc(INACTIVE_DATE), .by_group = TRUE ) %>%
    slice(1L) %>%
    ungroup()
  
  df2 <- df1 %>% 
    filter(RETURN_VALIDATE == "No" & is.na(DIED_NDR) & INACTIVE_TIME != "> 6 months") 
  
  
  print("Counts by implementing partner and state:")
  print(df2 %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  print("Distinct counts by implementing partner and state:")
  print(df2 %>% distinct(SITE_PID,FACILITY_UID,IMPLEMENTING_PARTNER,STATE) %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  
  write.xlsx(df2, 
             paste("./Continue_LTFU/Continue_LTFU_", date, ".xlsx", sep = ""), 
             asTable = TRUE)
  return(df2)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #"Plateau", "Osun", "Ondo", "Benue", "Ekiti", "Ogun", "Oyo", ---- APIN
  #"Enugu", "Imo", "Ebonyi", "Delta", ---- CCFN
  #"Kaduna", "Gombe", "Lagos", "Kogi", ---- CIHP
  #"Nasarawa", "Katsina", "FCT", "Rivers" ---- IHVN
  
  
  #setwd("/Submissions") # Folder of partner submissions
import_partnersubmissions <- function(){
  filelist <- list.files(path = "./Submissions/", pattern = "*.xlsx")
  print(filelist)
  master <- data.frame()
  for(x in filelist){
    if(str_detect(x,"APIN")){
      print("Reading APIN...")
      master <- rbind(master,
                      read_partner(x, "Plateau"),
                      read_partner(x, "Osun"),
                      read_partner(x, "Ondo"),
                      read_partner(x, "Benue"),
                      read_partner(x, "Ekiti"),
                      read_partner(x, "Ogun"),
                      read_partner(x, "Oyo")
      )
    }else if(str_detect(x,"CCFN")){
      print("Reading CCFN...")
      master <- rbind(master,
                      read_partner(x, "Enugu"),
                      read_partner(x, "Imo"),
                      read_partner(x, "Ebonyi"),
                      read_partner(x, "Delta")
      )
    }else if(str_detect(x,"CIHP")){
      print("Reading CIHP...")
      master <- rbind(master,
                      read_partner(x, "Kaduna"),
                      read_partner(x, "Gombe"),
                      read_partner(x, "Lagos"),
                      read_partner(x, "Kogi")
      )
    }else if(str_detect(x,"IHVN")){
      print("Reading IHVN...")
      master <- rbind(master,
                      read_partner(x, "Nasarawa"),
                      read_partner(x, "Katsina"),
                      read_partner(x, "FCT"),
                      read_partner(x, "Rivers")
      )
    }
  }
  
  master <- master %>% filter(!is.na(IMPLEMENTING_PARTNER)) %>%
    distinct()
  
  print(unique(master$IMPLEMENTING_PARTNER))
  print(unique(master$STATE))
  
  print("Final counts by implementing partner and state:")
  print(master %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  print("Final distinct counts by implementing partner and state:")
  print(master %>% distinct(SITE_PID,FACILITY_UID,IMPLEMENTING_PARTNER,STATE) %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  
  return(master)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

merge_ndrdf_partnersub <- function(ndr_df, partnersub){
  
  master_umb <- merge(ndr_df, 
                      partnersub[,c("NDR_PID",
                                    "SITE_PID",
                                    #"DATA_PULL",
                                    "FACILITY_UID",
                                    "LOST_IN_TX",
                                    "TRACK_ATTEMPTED",
                                    "REACHED",
                                    "REACHED_REFUSE_RETURN",
                                    "REACHED_RETURN",
                                    "DEAD",
                                    "TRANSFERRED_OUT_NOREC",
                                    "NOT_REACHED_REASON",
                                    "NARRATIVE"
                      )], 
                      by.x = c("SITE_PID", "FACILITY_UID"), 
                      by.y = c("SITE_PID", "FACILITY_UID"), 
                      all.x = T)%>%
    mutate(INACTIVE_MONTH = month(INACTIVE_DATE, label = T, abbr = T)) %>%
    
    select(c(
      "NDR_PID" = "NDR_PID.x",
      "SITE_PID",
      "SEX",
      "AGE",
      "FINE_AGE",
      
      "DATA_PULL",
      
      "IMPLEMENTING_PARTNER",
      "STATE",
      "LGA",
      "FACILITY_NAME",
      "FACILITY_UID",
      
      "ART_START", 
      "ART_TIME", 
      "INACTIVE_DATE",
      "INACTIVE_QTR",
      "INACTIVE_MONTH",
      "INACTIVE_TIME",
      
      "RETURN_VALIDATE",
      
      "LAST_DRUG_PICKUP_INACTIVE",
      "LAST_DRUG_MMD_INACTIVE",
      "MOST_RECENT_DRUG_PICKUP",
      
      "DIED_NDR", 
      "TRANSFERRED_NDR",
      
      "LOST_IN_TX", 
      "TRACK_ATTEMPTED", 
      "REACHED",
      "REACHED_REFUSE_RETURN",
      "REACHED_RETURN",
      "DEAD",
      "TRANSFERRED_OUT_NOREC",
      "NOT_REACHED_REASON", 
      "NARRATIVE"
    )) %>%
    distinct()
  
  print("Counts by implementing partner and state:")
  print(master_umb %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  print("Distinct counts by implementing partner and state:")
  print(master_umb %>% distinct(SITE_PID,FACILITY_UID,IMPLEMENTING_PARTNER,STATE) %>% group_by(IMPLEMENTING_PARTNER,STATE) %>% summarise(n = n())
  )
  
  return(master_umb)
}

