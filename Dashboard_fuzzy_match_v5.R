library(stringdist)
library(reshape2)
library(readxl)
library("shiny")
library("shinydashboard")
library(DT)

#define the body
ui <- tagList(
  
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Fuzzy Matchor",
    tabPanel("Home",
             sidebarPanel(
               fileInput("file1", "Choose Client File",
                         multiple = FALSE,
                         accept = c(".xlsx")),
               # Horizontal line ----
               tags$hr(),
               
               # Input: Select a file ----
               fileInput("file2", "Choose Source of Truth File",
                         multiple = FALSE,
                         accept = c(".xlsx"))
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Input File",
                          
                          DT::dataTableOutput("file")),
                 tabPanel("Source of Truth",
                          DT::dataTableOutput("Truth"))
               )
             )
    ),
    
    
    tabPanel("Exact Match",
             mainPanel(
               tabsetPanel(
                 tabPanel("Matching VAT",
                          DT::dataTableOutput("matching_vat")),
                 tabPanel("non matching VAT",
                          DT::dataTableOutput("non_matching_vat")))
             )
    ),
    tabPanel("Quantitative Check",
             mainPanel(
               tabsetPanel(
                 tabPanel("Step4",
                          DT::dataTableOutput("step4"))
               )
             )
    ),
    tabPanel("Exact Match VAT ID",
             mainPanel(
               tabsetPanel(
                 tabPanel("Fully Matching",
                          DT::dataTableOutput("step5_matching")),
                 tabPanel("Non Fully Matching",
                          DT::dataTableOutput("step5_non_matching"))
               )
             )
    ),
    tabPanel("Results Step 7",
             mainPanel(
               tabsetPanel(
                 tabPanel("Above 85%",
                          DT::dataTableOutput("step7_above")),
                 tabPanel("Below 85%",
                          DT::dataTableOutput("step7_below"))
               )
             )
    ),
    tabPanel("Results Step 9",
             mainPanel(
               tabsetPanel(
                 tabPanel("Above 85%",
                          DT::dataTableOutput("step9_above")),
                 tabPanel("Below 85%",
                          DT::dataTableOutput("step9_below"))
               )
             ))
    
  )
)




# Define server logic to read selected file ----
server <- function(input, output) {
  
  
  #Take all input as reactive values to be able to use them
  values <- reactiveValues()
  
  
  #This used to be able to get what is in this part as an output
  observe({
    
    
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        file <- read_excel(input$file1$datapath, skip = 3)
        values$file <- file
        
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    req(input$file2)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        Truth <- read_excel(input$file2$datapath)
        values$Truth <- Truth
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    
    library(readxl)
    library(dplyr)
    
    
    #Source of truth
    
    Truth$name <- paste(Truth$`Supplier Name`,  Truth$`Supplier Tax Number`)
    
    
    #Remove Whitespace
    Truth$`Supplier Tax Number` <- gsub(" ", "", Truth$`Supplier Tax Number`, fixed = T)
    #remove dots
    Truth$`Supplier Tax Number` <- gsub(".", "", Truth$`Supplier Tax Number`, fixed = T)
    #remove comma
    Truth$`Supplier Tax Number` <- gsub(",", "", Truth$`Supplier Tax Number`, fixed = T)
    #remove dash
    Truth$`Supplier Tax Number` <- gsub("/", "", Truth$`Supplier Tax Number`, fixed = T)
    Truth$`Supplier Tax Number`
    
    Truth_subsetted_tax_number <- subset(Truth, select = c("Supplier Tax Number"))
    
    Truth_subset_address <- subset(Truth, select = c("Supplier Tax Number", "Supplier Name", "Supplier Address"))
    
    #file to check the quality
    
    
    file$name <- paste(file$`Supplier Name`,  file$`Supplier Tax Number`)
    
    
    #Remove Whitespace
    file$`Supplier Tax Number` <- gsub(" ", "", file$`Supplier Tax Number`, fixed = T)
    #remove dots
    file$`Supplier Tax Number` <- gsub(".", "", file$`Supplier Tax Number`, fixed = T)
    #remove comma
    file$`Supplier Tax Number` <- gsub(",", "", file$`Supplier Tax Number`, fixed = T)
    #remove dash
    file$`Supplier Tax Number` <- gsub("/", "", file$`Supplier Tax Number`, fixed = T)
    file$`Supplier Tax Number`
    
    #only take relevant columns
    file_subsetted <- subset(file, select = c("Supplier Tax Number", "Supplier Name", "Supplier Address"))
    
    #null VAT number
    null_value <- file_subsetted[is.na(file_subsetted$`Supplier Tax Number`),]
    
    #non null VAT Number
    non_null_value <- file_subsetted[!is.na(file_subsetted$`Supplier Tax Number`),]
    
    #Remove duplicated (do a group by)
    grouped_file <- non_null_value[!duplicated(non_null_value),]
    
    #file comparison only VAT Number
    file_subsetted_tax_number <- subset(grouped_file, select = c("Supplier Tax Number"))
    
    
    #Step 2
    #Fully matching VAT Number
    matching_vat <- merge(x = file_subsetted_tax_number, y = Truth_subsetted_tax_number, by = 'Supplier Tax Number')
    values$matching_vat <- matching_vat
    
    values$percentage_exact_match <- round((nrow(matching_vat) / nrow(file_subsetted_tax_number)), 4) * 100
    
    values$step1 <- renderText(percentage_exact_match)
    
    #Non Matching VAT Number
    non_matching_vat <- file_subsetted_tax_number %>% anti_join(Truth_subsetted_tax_number,by = 'Supplier Tax Number')
    values$non_matching_vat <- non_matching_vat
    
    #Initialized column without_prefix & Iso
    non_matching_vat$without_prefix <- non_matching_vat$`Supplier Tax Number`
    non_matching_vat$iso_code <- ""
    
    
    #Step 3
    #Remove Prefix is it is an iso code
    
    #IsoCode in scope
    iso_code <- c('AT', 'BE' , 'BG', 'CY', 'CZ', 'DE', 'DK', 'EE', 'EL', 'ES', 'FI', 'FR', 'GB', 'HR', 'HU', 'IE',
                  'IT', 'LT', 'LT', 'LU', 'LV', 'MT', 'NL', 'PL', 'PT', 'RO', 'SE', 'SI', 'SK')
    
    for(i in 1 : nrow(non_matching_vat)) {
      if((substr(non_matching_vat$`Supplier Tax Number`[i], 0,
                 2) %in% iso_code) == T) {
        non_matching_vat$`without_prefix`[i] <- substr(non_matching_vat$`Supplier Tax Number`[i], 3,
                                                       nchar(non_matching_vat$`Supplier Tax Number`[i]))
        non_matching_vat$iso_code[i] <- (substr(non_matching_vat$`Supplier Tax Number`[i], 0, 2))
      }
      
      else {
        non_matching_vat$`without_prefix`[i] <- non_matching_vat$`Supplier Tax Number`[i]
      }
    }
    
    #Iso COde with clear structure
    eight_characters <- c('DK', 'FI', 'HU', 'LU', 'MT', 'SI')
    nine_characters <- c('AT', 'CY', 'DE', 'EE', 'EL', 'ES', 'PT')
    ten_characters <- c('BE', 'PL', 'SK')
    eleven_characters <- c('HR', 'IT', 'LV')
    twelve_characters <- c('NL', 'SE')
    
    #Step 4 - Check Structure of VAT
    #Initialized column without_prefix & Iso
    non_matching_vat$Structure_vat <-  ""
    
    #Logic of the check - First country with only 1 structure then country specific structure
    
    for (j in 1:nrow(non_matching_vat)) {
      if( (non_matching_vat$iso_code[j] %in% eight_characters) == T & nchar(non_matching_vat$without_prefix[j]) == 8) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ( ((non_matching_vat$iso_code[j] %in% nine_characters) == T) & (nchar(non_matching_vat$without_prefix[j]) == 9) ) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ((non_matching_vat$iso_code[j] %in% ten_characters) == T & nchar(non_matching_vat$without_prefix[j]) == 10) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
        
      }
      else if ((non_matching_vat$iso_code[j] %in% eleven_characters) == T & nchar(non_matching_vat$without_prefix[j]) == 11) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ((non_matching_vat$iso_code[j] %in% twelve_characters) == T & nchar(non_matching_vat$without_prefix[j]) == 12) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if((non_matching_vat$iso_code[j] == "BG") == T & (nchar(non_matching_vat$without_prefix[j]) == 9 ||
                                                             nchar(non_matching_vat$without_prefix[j]) == 10)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if((non_matching_vat$iso_code[j] == "CZ") == T & (nchar(non_matching_vat$without_prefix[j]) == 8 ||
                                                             nchar(non_matching_vat$without_prefix[j]) == 9 ||
                                                             nchar(non_matching_vat$without_prefix[j]) == 10)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if((non_matching_vat$iso_code[j] == "FR") == T & (grepl("^[A-Za-z]+$", substr(non_matching_vat$without_prefix[j], 0,2)) == T) &
              (nchar(non_matching_vat$without_prefix[j]) == 11)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ((non_matching_vat$iso_code[j] == "GB") == T & (nchar(non_matching_vat$without_prefix[j]) == 9 ||
                                                              nchar(non_matching_vat$without_prefix[j]) == 12 ||
                                                              nchar(non_matching_vat$without_prefix[j]) == 5)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ((non_matching_vat$iso_code[j] == "IE") == T & (nchar(non_matching_vat$without_prefix[j]) == 8 ||
                                                              nchar(non_matching_vat$without_prefix[j]) == 9)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ((non_matching_vat$iso_code[j] == "LT") == T & (nchar(non_matching_vat$without_prefix[j]) == 9 ||
                                                              nchar(non_matching_vat$without_prefix[j]) == 12)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      else if ((non_matching_vat$iso_code[j] == "RO") == T & (nchar(non_matching_vat$without_prefix[j]) >= 2 &
                                                              nchar(non_matching_vat$without_prefix[j]) <= 10)) {
        non_matching_vat$Structure_vat[j] <- "Correct Structure"
      }
      
      else  {
        non_matching_vat$Structure_vat[j] <- "Incorrect Structure"
      }
    }
    
    values$non_matching_vat_step4 <- non_matching_vat
    
    #Step 5 - Exact Match without IsoCode
    
    #Remove Iso code in source of Truth
    Truth_subsetted_tax_number$without_prefix <- substr(Truth_subsetted_tax_number$`Supplier Tax Number`, 3, nchar(Truth_subsetted_tax_number$`Supplier Tax Number`))
    Truth_subsetted_tax_number$iso_code <- substr(Truth_subsetted_tax_number$`Supplier Tax Number`, 0, 2)
    
    #Fully matching VAT Number
    matching_vat_without_ISO <- merge(x = non_matching_vat, y = Truth_subsetted_tax_number, by = "without_prefix")
    
    values$fully_matching_step5 <- matching_vat_without_ISO
    #Non Matching VAT Number
    non_matching_vat_without_ISO <- non_matching_vat %>% anti_join(Truth_subsetted_tax_number,by = "without_prefix")
    
    values$non_fully_matching_step5 <- non_matching_vat_without_ISO
    
    
    #Step 6 - Fuzzy Match on Name + VAT ID digits
    
    #Concatenate Name + VAT ID digits into concatenate_name
    Truth_subset_address$concatenate_name <- paste(Truth_subset_address$`Supplier Name`, Truth_subset_address$`Supplier Tax Number`)
    
    file_subsetted_address <- grouped_file
    file_subsetted_address$concatenate_name <- paste(file_subsetted_address$`Supplier Name`, file_subsetted_address$`Supplier Tax Number`)
    
    #Fuzzy Matching 2 files
    
    library(stringdist)
    
    distance.methods<-c('jaccard')
    dist.methods<-list()
    for(m in 1:length(distance.methods))
    {
      dist.name.enh<-matrix(NA, ncol = length(Truth_subset_address$concatenate_name),nrow = length(file_subsetted_address$concatenate_name))
      #take first element in source 2 and will look for the distance for each element in source 1
      for(i in 1:length(Truth_subset_address$concatenate_name)) {
        for(j in 1:length(file_subsetted_address$concatenate_name)) {
          dist.name.enh[j,i]<-stringdist(tolower(Truth_subset_address$concatenate_name[i]),tolower(file_subsetted_address$concatenate_name[j]),method = distance.methods[m])     
          #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
        } 
      }
      dist.methods[[distance.methods[m]]]<-dist.name.enh
    }
    #In this part, you will extract the smallest value for each distance calculated before
    
    match.s1.s2.enh<-NULL
    for(m in 1:length(dist.methods))
    {
      
      dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
      min.name.enh<-apply(dist.matrix, 1, base::min)
      for(i in 1:nrow(dist.matrix))
      {
        s2.i<-match(min.name.enh[i],dist.matrix[i,])
        s1.i<-i
        match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=Truth_subset_address$concatenate_name[s2.i],
                                          s1name=file_subsetted_address$concatenate_name[s1.i],
                                          s1_bp_name = file_subsetted_address$`Supplier Name`[s1.i],
                                          s1_bp_address = file_subsetted_address$`Supplier Address`[s1.i],
                                          adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
      }
    }
    # Let's have a look at the results
    library(reshape2)
    matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name+s1_bp_name+s1_bp_address~method, value.var = "adist")
    matched.names.matrix$'Percentage_Matching' <- (1 - matched.names.matrix$jaccard) * 100
    
    
    #Take the element with matching > 85%
    
    #matching
    fuzzy_match_address <- subset(x = matched.names.matrix, Percentage_Matching >=85)
    
    values$fuzzy_match_address_above_85 <- fuzzy_match_address
    #non matching
    fuzzy_non_match_address <- subset(x = matched.names.matrix, Percentage_Matching < 85)
    
    values$fuzzy_non_match_address_below_85 <- fuzzy_non_match_address
    
    
    
    #Step 8
    
    #Concatenate Name + VAT ID digits into concatenate_name
    Truth_subset_name <- Truth_subset_address
    Truth_subset_name$concatenate_name <- paste(Truth_subset_address$`Supplier Name`, Truth_subset_address$`Supplier Address`)
    
    
    fuzzy_non_match_address$concatenate_name <- paste(fuzzy_non_match_address$s1_bp_name, fuzzy_non_match_address$s1_bp_address)
    
    #Fuzzy Matching 2 files
    
    library(stringdist)
    
    distance.methods<-c('jaccard')
    dist.methods<-list()
    for(m in 1:length(distance.methods))
    {
      dist.name.enh<-matrix(NA, ncol = length(Truth_subset_name$concatenate_name),nrow = length(fuzzy_non_match_address$concatenate_name))
      #take first element in source 2 and will look for the distance for each element in source 1
      for(i in 1:length(Truth_subset_name$concatenate_name)) {
        for(j in 1:length(fuzzy_non_match_address$concatenate_name)) {
          dist.name.enh[j,i]<-stringdist(tolower(Truth_subset_name$concatenate_name[i]),tolower(fuzzy_non_match_address$concatenate_name[j]),method = distance.methods[m])     
          #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
        } 
      }
      dist.methods[[distance.methods[m]]]<-dist.name.enh
    }
    #In this part, you will extract the smallest value for each distance calculated before
    
    match.s1.s2.enh<-NULL
    for(m in 1:length(dist.methods))
    {
      
      dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
      min.name.enh<-apply(dist.matrix, 1, base::min)
      for(i in 1:nrow(dist.matrix))
      {
        s2.i<-match(min.name.enh[i],dist.matrix[i,])
        s1.i<-i
        match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=Truth_subset_name$concatenate_name[s2.i], s1name=fuzzy_non_match_address$concatenate_name[s1.i], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
      }
    }
    # Let's have a look at the results
    library(reshape2)
    matched.names.matrix.2<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
    matched.names.matrix.2$'Percentage_Matching' <- (1 - matched.names.matrix.2$jaccard) * 100
    
    
    #Take the element with matching > 85%
    
    #matching
    fuzzy_match_name <- subset(x = matched.names.matrix.2, Percentage_Matching >=85)
    
    values$fuzzy_match_name_above85 <- fuzzy_match_name
    
    #non matching
    fuzzy_non_match_name <- subset(x = matched.names.matrix.2, Percentage_Matching < 85)
    
    values$fuzzy_match_name_below85 <- fuzzy_non_match_name
    
  })
  
  
  #output the client file
  output$file <- DT::renderDataTable(DT::datatable({
    
    obj <- values$file
    
    return(obj)
  }))    
  
  #output the Source of Truth
  output$Truth <- DT::renderDataTable(DT::datatable({
    
    obj_truth <- values$Truth
    return(obj_truth)
  }))
  
  #output Step2 matching
  output$matching_vat <- DT::renderDataTable(DT::datatable({
    
    matching <- values$matching_vat
    return(matching)
  }))
  
  
  #output Step2 non matching
  output$non_matching_vat <- DT::renderDataTable(DT::datatable({
    non_matching <- values$non_matching_vat
    return(non_matching)
  }))
  
  #output Step4
  output$step4 <- DT::renderDataTable(DT::datatable({
    step_4 <- values$non_matching_vat_step4
    return(step_4)
  }))
  
  #output step5 - fully matching
  output$step5_matching <- DT::renderDataTable(DT::datatable({
    step5_match <- values$fully_matching_step5
    return(step5_match)
  }))
  
  #output step5 - non fully matching
  output$step5_non_matching <- DT::renderDataTable(DT::datatable({
    step5_non_match <- values$non_fully_matching_step5
    return(step5_non_match)
  }))
  
  #output step7 - above 85
  output$step7_above <- DT::renderDataTable(DT::datatable({
    step7_a <- values$fuzzy_match_address_above_85
    return(step7_a)
  }))
  
  #output step7 - below 85
  output$step7_below <- DT::renderDataTable(DT::datatable({
    step7_b <- values$fuzzy_non_match_address_below_85
    return(step7_b)
  }))
  
  #output step9 - above 85
  output$step9_above <- DT::renderDataTable(DT::datatable({
    step9_a <- values$fuzzy_match_name_above85
    return(step9_a)
  }))
  
  #output step9 - below 85
  output$step9_below <- DT::renderDataTable(DT::datatable({
    step9_b <- values$fuzzy_match_name_below85
    return(step9_b)
  }))
  
  
}



# Create Shiny app ----
shinyApp(ui, server)