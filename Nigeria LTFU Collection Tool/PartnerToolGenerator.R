#### Nigeria LTFU Partner Collection Tool Generation
#### STAGE 2: PARTNER TOOL GENERATOR
#### AUTHOR: Randy Yee (PCX5@cdc.gov)
#### CREATION DATE: 7/9/2019

generatetools <- function(master_umb){

  for(partner in (unique(master_umb$IMPLEMENTING_PARTNER))){
    container <- filter(master_umb, IMPLEMENTING_PARTNER == partner)
    
    # Get Current Date
    date <- format(Sys.Date(), format =  "%Y_%m_%d")
    getOption("openxlsx.dateFormat", "mm/dd/yyyy")
    
    # Initialize List of State DFs
    list_of_states <- list()
    
    # Divide Partner DF to Individual State DFs
    for(state in unique(container$STATE)){
      df2 <- filter(container, STATE == state)
      list_of_states[[state]] <- assign(state, df2)
    }
    
    # Import Excel Tool Template
    wb <- loadWorkbook(file = "LTFU_template.xlsx")
    #names(wb)
    
    # Write State DFs to Individual Tabs
    for(counter in unique(container$STATE)){
      addWorksheet(wb, counter, tabColour = "red")
      writeDataTable(wb, 
                     counter, 
                     list_of_states[[counter]],
                     startRow = 3,
                     tableStyle = "TableStyleMedium4",
                     tableName = counter)
      
      # Set Column Widths
      setColWidths(wb, counter, cols = 1:32, widths = "auto")
      
      # Create Instruction Titles
      writeData(wb, counter, "Instructions: Please complete the cells in columns \"X\" through \"AF\". 
              Do Not Make Changes to any other cells in the table.",
                startCol = 1, startRow = 1)
      writeData(wb, counter, "Please fill in these columns for each inactive patient.",
                startCol = 24, startRow = 1)
      writeData(wb, counter, "Select reason to indicate why patient confirmed on ART is inactive in NDR",
                startCol = 24, startRow = 2)
      writeData(wb, counter, "Select option to indicate if the patient was tracked Note: For patients found LOST_IN_TX, select NA",
                startCol = 25, startRow = 2)
      writeData(wb, counter, "Select option to indicate if tracked patient was successfully reached Note: For patients found LOST_IN_TX, select NA",
                startCol = 26, startRow = 2)
      writeData(wb, counter, "Add date patient refused to return, otherwise leave blank(MM/DD/YYYY)",
                startCol = 27, startRow = 2)
      writeData(wb, counter, "Add date patient picked up drugs, otherwise leave blank(MM/DD/YYYY)",
                startCol = 28, startRow = 2)
      writeData(wb, counter, "Add date of death, otherwise leave blank(MM/DD/YYYY)",
                startCol = 29, startRow = 2)
      writeData(wb, counter, "Add date of transfer out, otherwise leave blank (MM/DD/YYYY)",
                startCol = 30, startRow = 2)
      writeData(wb, counter, "Select option to indicate why patient could not be reached",
                startCol = 31, startRow = 2)
      writeData(wb, counter, "Provide details if the response to LOST_IN_TX or NOT_REACHED_REASON was OTHER",
                startCol = 32, startRow = 2)
      
      # Instruction Formatting
      title_style <- createStyle(fontSize = 24, fontColour = "white", fgFill = "#222B35", valign = "center")
      entry_style_h <- createStyle(fontSize = 16, fontColour = "white", fgFill = "#833C0C", valign = "center", halign = "center")
      entry_style <- createStyle(fontSize = 12, fontColour = "white", fgFill = "#C65911", valign = "center", halign = "center", wrapText = T)
      entry_style_h2 <- createStyle(fontColour = "white", fgFill = "#833C0C")
      entry_style_cells <- createStyle(borderStyle = getOption("openxlsx.borderStyle", "double"), 
                                       border = "TopBottomLeftRight",
                                       borderColour = "orange")
      
      # Apply Instruction Formatting
      addStyle(wb, counter, title_style, rows = 1, 
               cols = 1)
      addStyle(wb, counter, entry_style_h, rows = 1, 
               cols = 24)
      addStyle(wb, counter, entry_style, rows = 2, 
               cols = 24:32)
      addStyle(wb, counter, entry_style_h2, rows = 3, 
               cols = 24:32)
      addStyle(wb, counter, entry_style_cells, rows = 4:(nrow(list_of_states[[counter]])), cols = 24)
      addStyle(wb, counter, entry_style_cells, rows = 4:(nrow(list_of_states[[counter]])), cols = 25)
      addStyle(wb, counter, entry_style_cells, rows = 4:(nrow(list_of_states[[counter]])), cols = 26)
      addStyle(wb, counter, entry_style_cells, rows = 4:(nrow(list_of_states[[counter]])), cols = 31)
      addStyle(wb, counter, entry_style_cells, rows = 4:(nrow(list_of_states[[counter]])), cols = 32)
      # Merge Title Cells
      mergeCells(wb, counter, cols = 1:23, rows = 1:2)
      mergeCells(wb, counter, cols = 24:32, rows = 1)
      
      setRowHeights(wb, counter, rows = 2, heights = 90)
      
      # Apply Data Validation
      dataValidation(wb, counter, col = 24, rows = 4:(nrow(list_of_states[[counter]])), type = "list", value = "'dropdown list'!$G$7:$G$10") #LOST_IN_TX (Y)
      dataValidation(wb, counter, col = 25, rows = 4:(nrow(list_of_states[[counter]])), type = "list", value = "'dropdown list'!$J$7:$J$8") #TRACKING_ATTEMPTED (Y)
      dataValidation(wb, counter, col = 26, rows = 4:(nrow(list_of_states[[counter]])), type = "list", value = "'dropdown list'!$K$7:$K$8") #REACHED(Y)
      dataValidation(wb, counter, col = 31, rows = 4:(nrow(list_of_states[[counter]])), type = "list", value = "'dropdown list'!$L$7:$L$11") #NOT_REACHED_REASON (Y)
      
      # Style Date Columns
      addStyle(wb, counter, style = createStyle(numFmt = "DATE"), rows = 4:(nrow(list_of_states[[counter]])), cols = c(6,12,14,19,21,22,23), gridExpand = TRUE)
      addStyle(wb, counter, style = createStyle(numFmt = "DATE", 
                                                borderStyle = getOption("openxlsx.borderStyle", "double"), 
                                                border = "TopBottomLeftRight",
                                                borderColour = "orange"), rows = 4:(nrow(list_of_states[[counter]])), 
               cols = c(27,28,29,30), 
               gridExpand = TRUE)
      
      # Set Column Widths
      setColWidths(wb, counter, cols = c(1:24), widths = 15)
      setColWidths(wb, counter, cols = c(24:32), widths = 25)
    }
    
    # Add RawData Tab
    addWorksheet(wb, "RawData", tabColour = "black")
    writeDataTable(wb, 
                   "RawData", 
                   container,
                   startRow = 1,
                   tableStyle = "TableStyleMedium4",
                   tableName = "RawData")
    addStyle(wb, 
             "RawData", 
             style = createStyle(numFmt = "DATE"), 
             rows = 4:nrow(container), 
             cols = c(6,12,14,19,21,22,23), 
             gridExpand = TRUE)
    
    names(wb)
    
    # Hide Select Tabs
    sheetVisibility(wb)[2] <- FALSE
    sheetVisibility(wb)[3] <- FALSE
    
    # Protect Workbook
    if(partner == "APIN"){
      protectWorkbook(wb,
                      protect = T,
                      password = "xxxx1234")
    }else if(partner == "CCFN"){
      protectWorkbook(wb,
                      protect = T,
                      password = "xxxx1234")
    }else if(partner == "CIHP"){
      protectWorkbook(wb,
                      protect = T,
                      password = "xxxx1234")
    }else if(partner == "IHVN"){
      protectWorkbook(wb,
                      protect = T,
                      password = "xxxx1234")
    }
    
    # Save Workbook and Export
    saveWorkbook(wb, paste("./New Tools/",partner, "_LTFUTrackingToolV1_", date, ".xlsx", sep = ""), overwrite = TRUE)
    
  }
}