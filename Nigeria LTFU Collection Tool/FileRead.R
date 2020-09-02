#### Nigeria LTFU Partner Collection Tool Generation
#### GENERAL: UMB AND PARTNER TOOLS FILE READ FUNCTIONS
#### AUTHOR: Randy Yee (PCX5@cdc.gov)
#### CREATION DATE: 7/9/2019

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ndr_read <- function(f_name){
  print(paste("Reading", f_name, "..."))
  df <- read_excel(f_name, 
                  col_types = c("date", #DATA_PULL
                                "text", #NDR_PID
                                "text", #SITE_PID
                                "text", #IMPLEMENTING_PARTNER
                                "text", #STATE
                                "text", #LGA
                                "text", #FACILITY_NAME
                                "text", #FACILITY_UID
                                "text", #SEX
                                "numeric", #AGE
                                "text", #FINE_AGE
                                "date", #ART_START
                                "text", #ART_TIME
                                "date", #INACTIVE_DATE
                                "text", #INACTIVE_QTR
                                "date", #INACTIVE_MONTH
                                "text", #INACTIVE_TIME
                                "text", #RETURN_VALIDATE
                                "date", #DATE_DRUG_PICKUP
                                "numeric", #DRUG_MMD
                                "date", #DIED_NDR
                                "date", #TRANSFERRED_NDR
                                "date" #MOST_RECENT_DRUG_PICKUP
                  ))
  print(colnames(df))
  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read_partner <- function(x, sheet_name){
  print(paste("Reading", sheet_name, "..."))
  partnerfile <- readxl::read_xlsx(paste0("./Submissions/",x), sheet = sheet_name,
                                   skip = 2,
                                   col_names = T,
                                   col_types = c(
                                     "text", #NDR_PID
                                     "text", #SITE_PID
                                     "skip", #SEX
                                     "skip", #AGE
                                     "skip", #FINE_AGE
                                     "skip", #DATA_PULL
                                     "text", #IMPLEMENTING_PARTNER
                                     "text", #STATE
                                     "skip", #LGA
                                     "skip", #FACILITY_NAME
                                     "text", #FACILITY_UID
                                     "skip", #ART_START
                                     "skip", #ART_TIME
                                     "skip", #INACTIVE_DATE
                                     "skip", #INACTIVE_QTR
                                     "skip", #INACTIVE_MONTH
                                     "skip", #INACTIVE_TIME
                                     "skip", #RETURN_VALIDATE
                                     "skip", #DATE_DRUG_PICKUP
                                     "skip", #DRUG_MMD
                                     "skip", #MOST_RECENT_DRUG_PICKUP
                                     "skip", #DIED_NDR
                                     "skip", #TRANSFERRED_NDR
                                     "text", #LOST_IN_TX
                                     "text", #TRACKING_ATTEMPTED
                                     "text", #REACHED
                                     "date", #REACHED_REFUSE_RETURN
                                     "date", #REACHED_RETURN
                                     "date", #DEAD
                                     "date", #TRANSFERRED_OUT_NOREC
                                     "text", #NOT_REACHED
                                     "text" #NARRATIVE
                                   ))
  print(colnames(partnerfile))
  return(partnerfile)
}