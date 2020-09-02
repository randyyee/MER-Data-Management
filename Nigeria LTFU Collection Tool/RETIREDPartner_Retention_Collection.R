#### Nigeria LTFU Partner Collection Tool Generation
#### AUTHOR: Randy Yee (PCX5@cdc.gov)
#### CREATION DATE: 4/29/2019

library(readxl)
library(openxlsx)
library(tidyverse)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
umb_linelist <- read_excel(file.choose(), 
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
                                         "date" #TRANSFERRED_NDR
                           ))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wrangle_partner <- function(partner, period, ndr){
  
  
  partnerfile <- read_excel(file.choose(), sheet = "LTFU Tracking_Q3 FY20", skip = 5,
                            col_types = c("numeric", #S/No.
                                          "text", #NDR_ID
                                          "text", #IMPLEMENTING_PARTNER
                                          "text", #STATE
                                          "text", #LGA
                                          "text", #FACILITY_NAME
                                          "text", #FACILITY_UID
                                          "text", #INACTIVE (Patient ID)
                                          "text", #SEX
                                          "numeric", #AGE
                                          "date", #ART_START#(lf)[Enter Date]
                                          "date", #LAST_DRUG PICK UP DATE
                                          "numeric", #DRUG_REFILL DURATION (DAYS)
                                          "date", #INACTIVE_DATE#(lf)(on NDR)
                                          "text", #LOST_IN_TX#(lf)(Data Quality Issues#(lf)select from drop down)#(lf)
                                          "text", #TRACKING_ATTEMPTED#(lf)(select from drop down)
                                          "text", #REACHED#(lf)(select from drop down)
                                          "date", #REACHED_RETURN#(lf)Add [DATE] patient picked up drugs, otherwise leave blank
                                          "date", #REACHED_REFUSE_RETURN#(lf)Add [DATE] patient refused to return, otherwise leave blank
                                          "date", #Validate Returned to Treatment#(lf)(DATE_DRUG_PICKUP)
                                          "date", #DEAD #(lf)Add [DATE] of death, otherwise leave blank
                                          "date", #TRANSFERRED_OUT_NOREC#(lf)Add [DATE] of transfer out, otherwise leave blank
                                          "text", #NOT_TRACKED #(lf)(select from drop down)
                                          "text" #Tracked but not reached due to other reasons (List reasons)
                            ))
  
  merged_umb_partner <- merge(umb_linelist, 
                              partnerfile, 
                              by.x = c("NDR_PID", "FACILITY_UID"), 
                              by.y = c("NDR_ID", "FACILITY_UID (DATIM Facility ID Code)"), 
                              all = T) %>%
    
    mutate(PARTNER_CHECK = ifelse(IMPLEMENTING_PARTNER.x != IMPLEMENTING_PARTNER.y, "MISMATCH", "MATCH")) %>%
    mutate(STATE_CHECK = ifelse(STATE.x != STATE.y, "MISMATCH", "MATCH")) %>%
    mutate(LGA_CHECK = ifelse(LGA.x != LGA.y, "MISMATCH", "MATCH")) %>%
    mutate(FACILITYNAME_CHECK = ifelse(FACILITY_NAME.x != FACILITY_NAME.y, "MISMATCH", "MATCH")) %>%
    mutate(SEX_CHECK = ifelse(SEX.x != SEX.y, "MISMATCH", "MATCH")) %>%
    mutate(AGE_CHECK = ifelse(AGE.x != AGE.y, "MISMATCH", "MATCH")) %>%
    
    mutate(IMPLEMENTING_PARTNER = ifelse(is.na(IMPLEMENTING_PARTNER.x), IMPLEMENTING_PARTNER.y, IMPLEMENTING_PARTNER.x)) %>%
    mutate(STATE = ifelse(is.na(STATE.x), STATE.y, STATE.x)) %>%
    mutate(LGA = ifelse(is.na(LGA.x), LGA.y, LGA.x)) %>%
    mutate(FACILITY_NAME = ifelse(is.na(FACILITY_NAME.x), FACILITY_NAME.y, FACILITY_NAME.x)) %>%
    mutate(SEX = ifelse(is.na(SEX.x), SEX.y, SEX.x)) %>%
    mutate(AGE = ifelse(is.na(AGE.x), AGE.y, AGE.x)) %>%
    
    filter(IMPLEMENTING_PARTNER == partner) %>%
    
    mutate(COHORT = period,
           NARRATIVE = "") %>%
    
    select(c("DATA_PULL", "COHORT",
             "NDR_PID", "SITE_PID", "INACTIVE (Patient ID)",
             "SEX", "AGE", "FINE_AGE",
             
             "IMPLEMENTING_PARTNER", "STATE", "LGA", "FACILITY_NAME", "FACILITY_UID",
             
             "ART_START", "ART_TIME", "INACTIVE_DATE" = "INACTIVE_DATE\r\n(on NDR)", "INACTIVE_QTR", "INACTIVE_MONTH", "INACTIVE_TIME",
             "RETURN_VALIDATE", "DATE_DRUG_PICKUP" = "LAST_DRUG PICK UP DATE", "DRUG_MMD" = "DRUG_REFILL DURATION (DAYS)" ,
             "DIED_NDR", "TRANSFERRED_NDR",
             
             "LOST_IN_TX" = "LOST_IN_TX\r\n(Data Quality Issues\r\nselect from drop down)", 
             "TRACK_ATTEMPTED" = "TRACKING_ATTEMPTED\r\n(select from drop down)", 
             "REACHED" = "REACHED\r\n(select from drop down)",
             "REACHED_REFUSE_RETURN" = "REACHED_REFUSE_RETURN\r\nAdd [DATE] patient refused to return, otherwise leave blank",
             "REACHED_RETURN" = "REACHED_RETURN\r\nAdd [DATE] patient picked up drugs, otherwise leave blank" ,
             "DEAD" = "DEAD \r\nAdd [DATE] of death, otherwise leave blank",
             "TRANSFERRED_OUT_NOREC" = "TRANSFERRED_OUT_NOREC\r\nAdd [DATE] of transfer out, otherwise leave blank" ,
             "NOT_REACHED_REASON" = "Tracked but not reached due to other reasons (List reasons)", 
             "NARRATIVE",
             
             "PARTNER_CHECK", "PART_NDR" = "IMPLEMENTING_PARTNER.x", "PART_PART" = "IMPLEMENTING_PARTNER.y",
             "STATE_CHECK", "STATE_NDR" = "STATE.x", "STATE_PART" = "STATE.y",
             "LGA_CHECK", "LGA_NDR" = "LGA.x", "LGA_PART" = "LGA.y",
             "FACILITYNAME_CHECK", "FAC_NDR" = "FACILITY_NAME.x", "FAC_PART" = "FACILITY_NAME.y",
             "SEX_CHECK", "SEX_NDR" = "SEX.x", "SEX_PART" = "SEX.y",
             "AGE_CHECK", "AGE_NDR" = "AGE.x", "AGE_PART" = "AGE.y"

             ))
  
  return(merged_umb_partner)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ------------------------------     MAIN    ------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("~/Randy Codebase/R/Nigeria_PT_Retention")

partner <- "IHVN"
period <- "LTFU Tracking_Q3 FY20"
date <- format(Sys.Date(), format =  "%Y_%m_%d")
getOption("openxlsx.dateFormat", "mm/dd/yyyy")

df <- wrangle_partner(partner, period, umb_linelist)
list_of_states <- list()

# Initial Cleaning Appendix ~~~~~~~~~~~~~~~~~~~~~~~~~~
df <- read_excel(file.choose(), sheet = "All_States")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

for(state in unique(df$STATE)){
  df2 <- df %>% filter(STATE == state)
  list_of_states[[state]] <- assign(state, df2)
}

do{
wb <- loadWorkbook(file = "LTFU_template.xlsx")
#names(wb)

for(counter in unique(df$STATE)){
  addWorksheet(wb, counter, tabColour = "red")
  writeDataTable(wb, 
                 counter, 
                 list_of_states[[counter]],
                 startRow = 3,
                 tableStyle = "TableStyleMedium4",
                 tableName = counter)
  
  # Set Column Widths
  setColWidths(wb, counter, cols = 1:40, widths = "auto")
  
  # Create Instruction Titles
  writeData(wb, counter, "Instructions: Please complete the cells in columns \"Y\" through \"AG\"",
            startCol = 1, startRow = 1)
  writeData(wb, counter, "Please fill in these columns for each inactive patient.",
            startCol = 25, startRow = 1)
  writeData(wb, counter, "Select reason to indicate why patient confirmed on ART is inactive in NDR",
            startCol = 25, startRow = 2)
  writeData(wb, counter, "Select option to indicate if the patient was tracked Note: For patients found LOST_IN_TX, select NA",
            startCol = 26, startRow = 2)
  writeData(wb, counter, "Select option to indicate if tracked patient was successfully reached Note: For patients found LOST_IN_TX, select NA",
            startCol = 27, startRow = 2)
  writeData(wb, counter, "Add date patient refused to return, otherwise leave blank(MM/DD/YYYY)",
            startCol = 28, startRow = 2)
  writeData(wb, counter, "Add date patient picked up drugs, otherwise leave blank(MM/DD/YYYY)",
            startCol = 29, startRow = 2)
  writeData(wb, counter, "Add date of death, otherwise leave blank(MM/DD/YYYY)",
            startCol = 30, startRow = 2)
  writeData(wb, counter, "Add date of transfer out, otherwise leave blank (MM/DD/YYYY)",
            startCol = 31, startRow = 2)
  writeData(wb, counter, "Select option to indicate why patient could not be reached",
            startCol = 32, startRow = 2)
  writeData(wb, counter, "Provide details if the response to LOST_IN_TX or NOT_REACHED_REASON was OTHER",
            startCol = 33, startRow = 2)

  # Instruction Formatting
  title_style <- createStyle(fontSize = 24, fontColour = "white", fgFill = "#222B35", valign = "center")
  entry_style_h <- createStyle(fontSize = 16, fontColour = "white", fgFill = "#222B35", valign = "center", halign = "center")
  entry_style <- createStyle(fontSize = 12, fontColour = "white", fgFill = "#0D5279", valign = "center", halign = "center", wrapText = T)
  
  addStyle(wb, counter, title_style, rows = 1, cols = 1)
  addStyle(wb, counter, entry_style_h, rows = 1, cols = 25)
  addStyle(wb, counter, entry_style, rows = 2, cols = 25:33)
  
  # Merge Title Cells
  mergeCells(wb, counter, cols = 1:24, rows = 1:2)
  mergeCells(wb, counter, cols = 25:33, rows = 1)
  
  setRowHeights(wb, counter, rows = 2, heights = 90)
  
  # Apply Data Validation
  dataValidation(wb, counter, col = 25, rows = 4:(nrow(list_of_states[[counter]]) + 5), type = "list", value = "'dropdown list'!$G$7:$G$10") #LOST_IN_TX (Y)
  dataValidation(wb, counter, col = 26, rows = 4:(nrow(list_of_states[[counter]]) + 5), type = "list", value = "'dropdown list'!$J$7:$J$8") #TRACKING_ATTEMPTED (Y)
  dataValidation(wb, counter, col = 27, rows = 4:(nrow(list_of_states[[counter]]) + 5), type = "list", value = "'dropdown list'!$K$7:$K$8") #REACHED(Y)
  dataValidation(wb, counter, col = 32, rows = 4:(nrow(list_of_states[[counter]]) + 5), type = "list", value = "'dropdown list'!$L$7:$L$9") #NOT_REACHED_REASON (Y)

  # Style Date Columns
  addStyle(wb, counter, style = createStyle(numFmt = "DATE"), rows = 4:(nrow(list_of_states[[counter]]) + 5), cols = c(1,14,16,18,21,23,24,28,29,30,31), gridExpand = TRUE)
  setColWidths(wb, counter, cols = c(1,14,16,18,21,23:33), widths = 15)
}

# addWorksheet(wb, "All_States")
# writeDataTable(wb, 
#                "All_States", 
#                df,
#                tableStyle = "TableStyleMedium4",
#                tableName = "All_States")


names(wb)

saveWorkbook(wb, paste(partner, "_LTFUTrackingTool_", date, ".xlsx", sep = ""), overwrite = TRUE)
