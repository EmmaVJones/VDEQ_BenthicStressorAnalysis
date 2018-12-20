logiManipulationUI <- function(id){
  ns <- NS(id)
  tagList(
    h3('Logi Data Manipulation'),
    p('Users can upload raw data from a LOGI query directly into the app to have R handle all data 
      manipulation steps necessary to run the Benthic Stressor Analysis Tool. Uploaded data must be 
      formatted in a specific way; therefore, it is crucial that the user pull data using the following 
      queries: ',span(em('BSA TOOL_BENSTRESS Parameters by Station')), span(em('BSA TOOL_Field  Parameters by Station')),', and ',
      span(em('BSA TOOL_Dissolved Metals by Station')),' located in the ',span(em('Benthic Stressor Tool Data Pulls')), ' folder under
      the ',span(em('Shared TMDL')),' LOGI folder.  Enter the appropriate station and date range and select ',span(em('Run and Hide
      Parameters')),'. Download the excel spreadsheet and save to a raw data folder within your project folder,
      then upload the LOGI pull without modifying the formatting.'),br(),
    p('It is advised that users only use this page once per site as one can download the manipulated data for 
      future app uploads if analyses or reports need to be rerun.'),br(),
    p(strong('It is important to review all steps in the data manipulation process from ensuring
             all expected data was retrieved by LOGI and that no data was lost throughout manipulation
             and after joining additional datasets.')),
    fluidRow(
      column(4,
             fileInput(ns('siteData_LOGIfield'),'Upload BSA Tool_Field',accept = c(".xls",".xlsx"),width='50%')),
      column(4,
             fileInput(ns('siteData_LOGIBenStress'),'Upload BSA Tool_BenStress',accept = c(".xls",".xlsx"),width='50%')),
      column(4,
             fileInput(ns('siteData_EDASRBP'),'Upload EDAS Total Hab Values Score Query',accept = c(".xls",".xlsx"),width='50%'))),
    h4('Field Data'),
    h5('Data Preview'),
    DT::dataTableOutput(ns('rawLOGI_field')),br(),
    h5('Field Data Manipulation'),
    p("Next we are going to fill the StationID column with the appropriate StationID, combine the DO Optical
      and DO Probe columns into a single DO field, change the name of the Temp Celsius field to Temp, remove
      rows of data containing only NA's, and change the CollectionDateTime field to a Date format because it 
      does not contain any time values."),
    DT::dataTableOutput(ns('LOGI_fieldmanipulated')),
    br(),br(),
    h4('Benstress Data'),
    h5('Data Preview'),
    DT::dataTableOutput(ns('rawLOGI_BenStress')),br(),
    h5('Benstress Data Manipulation'),
    p("Next we are going to rename all variables in the Parm Name to match the necessary data template, 
      identify if any parameters were duplicated on a single sample day and take the mean of those measures
      if so, remove rows of data containing only NA's, convert the data from long to wide format, and 
      change the CollectionDateTime field to a Date format to facilitate table joins."),
    DT::dataTableOutput(ns('LOGI_BenStressmanipulated')),
    br(),br(),
    h4('Total Habitat Data'),
    h5('Data Preview'),
    DT::dataTableOutput(ns('rawEDASRBP')),br(),
    h5('Total Habitat Data Manipulation'),
    p("Next we are going to rename the CollDate field to match the CollectionDateTime field for joining purposes
     rename TotHabSc to TotalHabitat, and change the CollectionDateTime field to a Date format because it 
      does not contain any time values."),
    DT::dataTableOutput(ns('EDASRBPmanipulated')),
    br(),br(),
    h3('Join Field, Benstress, and Total Habitat Data'),
    p('All manipulated datasets are now joined by the StationID and CollectionDateTime
      fields. Additionally, columns for MetalsCCU and LRBS are added for manual user entry.'),
    p(span(strong('After you have finalized entering all additional data, please click the csv button
             in the top left corner to download the final dataset.')),'The file is automatically saved 
      to your Downloads folder. It is ',strong('HIGHLY ADVISED', style="color:red"),' that you move this dataset 
      to a folder with other datasets pertaining to your project.'),
    DT::dataTableOutput(ns('allTogether')),
    p(span(strong('Note: This table is editable.')),'You may double click any of the cells to edit data.
      This feature is especially helpful for adding in MetalsCCU and LRBS data.'),
    br(),br(), hr(),
    h4('Dissolved Metals Data'),
    fileInput(ns('siteData_LOGImetals'),'Upload BSA Tool_Metals',accept = c(".xls",".xlsx"),width='50%'),
    h5('Data Preview'),
    DT::dataTableOutput(ns('rawLOGI_Metals')),br(),
    h5('Dissolved Metals Data Manipulation'),
    p("Next we are going to rename all variables in the Parm Name to match the necessary data template, 
      identify if any parameters were duplicated on a single sample day and take the mean of those measures
      if so, remove rows of data containing only NA's, convert the data from long to wide format, and 
      change the CollectionDateTime field to a Date format to facilitate table joins."),
    DT::dataTableOutput(ns('LOGI_metalsmanipulated')),
    p('Please download a .csv of the manipulated dissolved metals data for your records and to upload into 
      the Dissolved Metals Tab.'),
    br(),br(),
    h5('Metals CCU Calculation'),
    p('Below you will find the MetalsCCU calculation for input dissolved metals data. You can copy/paste
      this number into the joined field, Benstress, and habitat table above before exporting it to a .csv .'),
    DT::dataTableOutput(ns('LOGI_MetalsCCU'))
    
    #verbatimTextOutput(ns('field'))
    #verbatimTextOutput(ns('benstress'))
    
  )
}


logiManipulation <- function(input,output,session){
  # Upload field data straight from Logi
  inputFile_LOGIfield <- reactive({inFile <- input$siteData_LOGIfield
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath)})
  
  inputFile_LOGIBenStress <- reactive({inFile <- input$siteData_LOGIBenStress
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath)})
  
  inputFile_rawEDASRBP <- reactive({inFile <- input$siteData_EDASRBP
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath)})
  
  
  # Manipulate Logi raw data
  # Field data
  output$rawLOGI_field <- DT::renderDataTable({
    datatable(inputFile_LOGIfield(),rownames = F,
              options=list(pageLength = nrow(inputFile_LOGIfield()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the raw LOGI field data pull.')})
  LOGI_field <- reactive({
    req(inputFile_LOGIfield())
    field <- mutate(inputFile_LOGIfield(), StationID=unique(StationID)[1], # Fill StationID with value
           DO = ifelse(is.na(`DO Optical`),`DO Probe`,`DO Optical`),
           Temp = `Temp Celsius`) %>% # combine DO Optical and DO Probe fields
      select(-c(`DO Optical`,`DO Probe`,`Temp Celsius`)) # Now remove those columns bc no longer needed
    field <- field[complete.cases(field[ , 3:4]),] # remove rows without real data
    field$CollectionDateTime <- as.Date(field$CollectionDateTime) # change to just date format bc field query will not bring in times to match benstress pull
    return(field)
  })
  output$LOGI_fieldmanipulated <- DT::renderDataTable({
    datatable(LOGI_field(),rownames = F,
              options=list(pageLength = nrow(LOGI_field()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the manipulated LOGI field data.')})
  # Benstress data
  output$rawLOGI_BenStress <- DT::renderDataTable({
    datatable(inputFile_LOGIBenStress(),rownames = F,
              options=list(pageLength = nrow(inputFile_LOGIBenStress()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the raw LOGI Benstress data pull.')})
  LOGI_BenStress <- reactive({
    req(inputFile_LOGIBenStress())
    benstress <- mutate(inputFile_LOGIBenStress(),
                        `Parm Name` = recode(`Parm Name`, 
                                "PHOSPHORUS, TOTAL (MG/L AS P)" = "TP",
                                "NITROGEN, TOTAL (MG/L AS N)" = "TN",
                                "POTASSIUM, DISSOLVED (MG/L AS K)" = "DPotassium",
                                "SULFATE, DISSOLVED (MG/L AS SO4)" = "DSulfate",
                                "CHLORIDE, DISSOLVED IN WATER MG/L" = "DChloride",
                                "SODIUM, DISSOLVED (MG/L AS NA)" = "DSodium",
                                "TDS RESIDUE,TOTAL FILTRABLE (DRIED AT 180C),MG/L" = "TDS")) %>%
      group_by(StationID, CollectionDateTime, Latitude,Longitude,  `Parm Name`) %>%
      summarise_at('Value',mean) # average any rows that have same date/time but additional measures (i.e. run not scheduled correctly and have benstress and TN bottles collected at same time)
    benstress <- benstress[complete.cases(benstress[, 3:4]),] # get rid of extra empty rows 
    benstress <- spread(benstress,"Parm Name","Value") # go from Long to Wide format
    benstress$CollectionDateTime <- as.Date(benstress$CollectionDateTime) # change to just date format bc field query will not bring in times to match benstress pull
    # add Parameters columns if they were not sampled
    if(!("TN" %in% names(benstress))){benstress <- mutate(benstress,TN=NA)}
    if(!("TP" %in% names(benstress))){benstress <- mutate(benstress,TP=NA)}
    if(!("TDS" %in% names(benstress))){benstress <- mutate(benstress,TDS=NA)}
    if(!("DSulfate" %in% names(benstress))){benstress <- mutate(benstress,DSulfate=NA)}
    if(!("DChloride" %in% names(benstress))){benstress <- mutate(benstress,DChloride=NA)}
    if(!("DPotassium" %in% names(benstress))){benstress <- mutate(benstress,DPotassium=NA)}
    if(!("DSodium" %in% names(benstress))){benstress <- mutate(benstress,DSodium=NA)}
    return(benstress)
  })
  output$LOGI_BenStressmanipulated <- DT::renderDataTable({
    datatable(LOGI_BenStress(),rownames = F,
              options=list(pageLength = nrow(LOGI_BenStress()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the manipulated LOGI Benstress data.')})
  # Total Habitat data
  output$rawEDASRBP <- DT::renderDataTable({
    datatable(inputFile_rawEDASRBP(),rownames = F,
              options=list(pageLength = nrow(inputFile_rawEDASRBP()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the raw EDAS Total Habitat Parameters query.')})
  EDASRBP <- reactive({
    req(inputFile_rawEDASRBP())
    totHab <- dplyr::distinct(inputFile_rawEDASRBP(),StationID,CollDate,TotHabSc) %>%
      mutate(CollectionDateTime = CollDate, TotalHabitat = TotHabSc) %>%
      select(StationID,CollectionDateTime,TotalHabitat )
    totHab$CollectionDateTime <- as.Date(totHab$CollectionDateTime)
    return(totHab)
  })
  output$EDASRBPmanipulated <- DT::renderDataTable({
    datatable(EDASRBP(),rownames = F,
              options=list(pageLength = nrow(EDASRBP()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the manipulated EDAS Total Habitat data.')})
  
  # Combine field, benstress, and total habitat data
  
  allTogetherData <- reactive({req(LOGI_field(),LOGI_BenStress(),EDASRBP())
    # add lat/long to habitat to make join cleaner
    totHab <- mutate(EDASRBP(),Latitude = unique(LOGI_BenStress()$Latitude)[1], 
                     Longitude = unique(LOGI_BenStress()$Longitude)[1])
    envData <- full_join(LOGI_field(),LOGI_BenStress(),by=c('StationID','CollectionDateTime','Longitude','Latitude')) %>%
      full_join(totHab, by = c('StationID','CollectionDateTime','Longitude','Latitude')) %>%
      mutate(LRBS = NA, MetalsCCU = NA) %>% # add spot for parameters to be entered by hand
      select(StationID, CollectionDateTime, Longitude, Latitude, pH, DO, TN, TP, TotalHabitat, LRBS, MetalsCCU, 
             SpCond, TDS, DSulfate, DChloride, DPotassium, DSodium, Temp) %>% # rearrange data according to template
      arrange(CollectionDateTime) # sort oldest to newest
    addUnits_envDataDF(envData) # add units to table for export
      
  })
  output$allTogether <-  DT::renderDataTable({
    datatable(allTogetherData(),
              extensions = 'Buttons', escape=F,rownames = F, editable = TRUE,
              options=list(dom='Bt',pageLength = nrow(allTogetherData()), scrollY = "200px",scrollX = TRUE,
                           buttons=list('copy',
                                         list(extend='csv',filename=
                                                paste('LOGImanipulatedData_',Sys.Date(),unique(allTogetherData()$StationID[1]),sep='')))),
              caption = 'Below is a table of the joined field, Benstress, and Total Habitat data. Please
              manually enter any MetalsCCU and LRBS data you have for the site on a row with appropriate
              collection date.')})
  
  # Dissolved Metals
  inputFile_LOGImetals <- reactive({inFile <- input$siteData_LOGImetals
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath)})
  
  output$rawLOGI_Metals <- DT::renderDataTable({
    datatable(inputFile_LOGImetals(),rownames = F,
              options=list(pageLength = nrow(inputFile_LOGImetals()), scrollY = "200px",scrollX = TRUE),
              caption = 'Below is a table of the raw LOGI dissolved metals data pull.')})
  LOGI_metals <- reactive({
    req(inputFile_LOGImetals())
    metals <- mutate(inputFile_LOGImetals(), StationID = `Station ID`, CollectionDateTime = `Date Time`,
                     `Parm Name` = recode(`Parm Name`, 
                                "SELENIUM, DISSOLVED (UG/L AS SE)" = "Selenium",
                                "CALCIUM, DISSOLVED (MG/L AS CA)" = "Calcium",
                                "MAGNESIUM, DISSOLVED (MG/L AS MG)" = "Magnesium",
                                "ARSENIC, DISSOLVED  (UG/L AS AS)" = "Arsenic",
                                "BARIUM, DISSOLVED (UG/L AS BA)" = "Barium",
                                "ALUMINUM, DISSOLVED (UG/L AS AL)" = "Aluminum",
                                "BERYLLIUM, DISSOLVED (UG/L AS BE)" = "Beryllium",
                                "CADMIUM, DISSOLVED (UG/L AS CD)" = "Cadmium",
                                "CHROMIUM, DISSOLVED (UG/L AS CR)" = "Chromium",
                                "COPPER, DISSOLVED (UG/L AS CU)" = "Copper", 
                                "IRON, DISSOLVED (UG/L AS FE)" = "Iron",
                                "LEAD, DISSOLVED (UG/L AS PB)" = "Lead",
                                "MANGANESE, DISSOLVED (UG/L AS MN)" = "Manganese",
                                "THALLIUM, DISSOLVED (UG/L AS TL)" = "Thallium",
                                "NICKEL, DISSOLVED (UG/L AS NI)" = "Nickel",
                                "SILVER, DISSOLVED (UG/L AS AG)" = "Silver",
                                "ZINC, DISSOLVED (UG/L AS ZN)" = "Zinc",
                                "ANTIMONY, DISSOLVED (UG/L AS SB)" = "Antimony", 
                                "HARDNESS, CA MG CALCULATED (MG/L AS CACO3) AS DISSOLVED" = "Hardness")) %>%
      group_by(StationID, CollectionDateTime, Latitude,Longitude,  `Parm Name`) %>%
      summarise_at('Value',mean) %>% # if lab duplicated result, handle it by taking the mean of the values (if truly duplicated then it won't change values)
      select(StationID, CollectionDateTime, Longitude, Latitude, `Parm Name`, Value) %>%
      filter(!(`Parm Name` =='SODIUM, DISSOLVED (MG/L AS NA)')) # make sure no sodium left in there, relic of bad queries
    metals <- metals[complete.cases(metals[, 3:4]),] # get rid of extra empty rows 
    metals <- spread(metals,"Parm Name","Value") # go from Long to Wide format
    metals$CollectionDateTime <- as.Date(metals$CollectionDateTime) # change to just date format bc field query will not bring in times to match benstress pull
    return(metals)
  })
  output$LOGI_metalsmanipulated <- DT::renderDataTable({
    datatable(LOGI_metals(), extensions = 'Buttons', escape=F,rownames = F, 
              options=list(dom='Bt',pageLength = nrow(LOGI_metals()), scrollY = "80px",scrollX = TRUE,
                           buttons=list('copy',
                                        list(extend='csv',filename=
                                               paste('LOGImanipulatedMetalData_',Sys.Date()),sep=''))),
              caption = 'Below is a table of the manipulated LOGI dissolved metals data.')})

  output$LOGI_MetalsCCU <-  DT::renderDataTable({
    req(LOGI_metals())
    calc <- metalsCCUcalcDF(data.frame(LOGI_metals()))%>%
      mutate(StationID=LOGI_metals()$StationID)%>%
      select(StationID,CollectionDateTime,MetalsCCU)
    
    datatable(calc,rownames = F, editable = TRUE,
              options=list(pageLength = nrow(calc), scrollY = "80px",scrollX = TRUE),
              caption = 'Below is a table of the MetalsCCU calculation for the input StationID(s). Please
              copy the MetalsCCU value to manually enter the appropriate MetalsCCU data in the joined 
              table above on the appropriate collection date.')})
  #output$field <- renderPrint({allTogetherData()})
  #output$benstress <- renderPrint({inputFile_LOGIBenStress()})
}


## TESTING ZONE

#ui <- fluidPage(theme = shinythemes::shinytheme("yeti"),
#  logiManipulationUI('logi')
#)

#server <- shinyServer(function(input, output, session) {
#  callModule(logiManipulation,'logi')
#})

#shinyApp(ui,server)

