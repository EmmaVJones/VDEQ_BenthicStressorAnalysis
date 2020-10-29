finalReportUI <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Report",
               column(3,wellPanel(
                 h4("Instructions:"),
                 p("This part of the application generates a Microsoft Word document with a report customized to the user input
        data. The report generates figures and language applicable to benthic TMDL stressor reports and nesting rationales. 
        The document should be edited for additional content and language. Again, this app can only handle a single site at 
        one time, so each station within a single watershed will require a separate application instance. "),
                 p("Please upload all required datasets and complete all text boxes before clicking the 'Generate Report' button"),
                 p("All data uploaded to the app must be formatted correctly. If you are unsure whether your
        data is in the correct format, please download the appropriate template file first to check 
        your data structure."))),
               column(9,
                      p('In order to format your report correctly, please fill in the fields provided below.'),
                      helpText('The templates are downloaded as .csv due to Java error downloading .xlsx files. Please 
                               convert files back to .xlsx prior to uploading to this page for full report functionality.'),
                      downloadButton(ns('downloadTemplate_bugs'),"Download bug template (VSCI)"),
                      downloadButton(ns('downloadTemplate_bugsVCPMI63'),"Download bug template (VCPMI63+Chowan)"),
                      downloadButton(ns('downloadTemplate_bugsVCPMI65'),"Download bug template (VCPMI65-Chowan)"),
                      downloadButton(ns('downloadTemplate_habitat'),"Download habitat template"),br(),br(),
                      fileInput(ns('siteData_bugs'),'Upload Macroinvertebrate Data (.xlsx)',accept=c('.xlsx'),width='75%'),
                      fileInput(ns('siteData_habitat'),'Upload Habitat Data (.xlsx)',accept=c('.xlsx'),width='75%'),
                      textInput(ns('prettyStreamName'),"Stream Name", placeholder = "Briery Creek"),
                      br(),br(),hr(),
                      p('Click the appropriate generate report button to generate a preliminary report based on the data analyzed by the  
             Benthic Stressor Tool and additional user input datasets. The included text is preliminary language 
             that is applicable to most reports. Please revise according to your needs.'),
                      downloadButton(ns('report_Word'),'Generate Report- VSCI'),br(),
                      downloadButton(ns('report_Word63'),'Generate Report- VCPMI Ecoregion 63 + Chowan Basin'),br(),
                      downloadButton(ns('report_Word65'),'Generate Report- VSCI Ecoregion 65 - Chowan Basin'))),
      tabPanel("Heatmap",
               column(3,
                      wellPanel(
                        h4("Instructions:"),
                        p("This part of the application generates the HTML heatmaps that may have issues rendering in your
                          Word document, depending on which version of Microsoft Office you have installed."),
                        p("The current work around is to upload all data necessary to run the report, run the appropriate report, and
                          then come to this tab to generate the heatmaps. It is advised that you use the 'Snip' tool to 
                          save a .png of the heatmaps you wish to import into your Word document. From there, you can either
                          manually add the heatmap into your final Word document, or you can program your .Rmd to call the
                          .png into your Rmarkdown report (example script: ![altTextToPrint](directoryName/fileName.png) ."))),
               column(9,
                      h5(strong('Table 5')),
                      DT::dataTableOutput(ns('table5'), width = '400px'),br(),br(),
                      h5(strong('Table 6')),
                      DT::dataTableOutput(ns('table6'), width = '200px'),br(),br(),
                      h5(strong('Table 7')),
                      DT::dataTableOutput(ns('table7'), width = '200px')
               )
                   
      ))
    
    
      
  )
}

finalReport <- function(input,output,session, userData, stats){
  
  # Download data templates
  output$downloadTemplate_bugs <- downloadHandler(filename=function(){'bugTemplate.csv'},
                                                  content=function(file){write.csv(bugTemplate,file,row.names=FALSE)})
  
  output$downloadTemplate_bugsVCPMI63 <- downloadHandler(filename=function(){'bugTemplateVCMPI63.csv'},
                                                         content=function(file){write.csv(bugTemplateVCPMI63,file,row.names=FALSE)})
  
  output$downloadTemplate_bugsVCPMI65 <- downloadHandler(filename=function(){'bugTemplateVCMPI65.csv'},
                                                         content=function(file){write.csv(bugTemplateVCPMI65,file,row.names=FALSE)})
  
  output$downloadTemplate_habitat <- downloadHandler(filename=function(){'habitatTemplate.csv'},
                                                     content=function(file){write.csv(habitatTemplate,file,row.names=FALSE)})
  
  # Bring in User Bug and habitat data
  usr_bugData <- reactive({inFile <- input$siteData_bugs
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath) %>%
    filter(RepNum=='1')
  })
  
  usr_habitatData <- reactive({inFile <- input$siteData_habitat
  if(is.null(inFile))
    return(NULL)
  read_excel(inFile$datapath)
  })
  # save it to the reactive environment for Rmd
  observe(userData$bugData <- usr_bugData())
  observe(userData$habitatData <- usr_habitatData())
  observe(userData$prettyStreamName <- input$prettyStreamName)

  # Send input data and report data to html report
  output$report_Word <- downloadHandler(
    paste(unique(userData[["stats_wTemp"]][,1])[1],"_BenthicStressorReportTemplate.docx",sep=""),
    content= function(file){
      tempReport <- normalizePath('BenthicStressorReportTemplate.Rmd')
      imageToSend1 <- normalizePath('RmdFiller_Figure1.jpg') #NEW 
      imageToSend2 <- normalizePath('RmdFiller_Figure2.jpg') #NEW 
      imageToSend3 <- normalizePath('RmdFiller_Figure3.jpg') #NEW 
      imageToSend4 <- normalizePath('RmdFiller_Table1.jpg') #NEW 
      imageToSend5 <- normalizePath('RmdFiller_Table2.jpg') #NEW 
      imageToSend6 <- normalizePath('RmdFiller_Table3.jpg') #NEW 
      imageToSend7 <- normalizePath('Parameters_Briery.PNG') #NEW 
      imageToSend8 <- normalizePath('Stressor_legend.PNG') #NEW 
      imageToSend9 <- normalizePath('Table1_VSCImetrics.PNG') #NEW 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(tempReport, 'BenthicStressorReportTemplate.Rmd')
      file.copy(imageToSend1, 'RmdFiller_Figure1.jpg') #NEW
      file.copy(imageToSend2, 'RmdFiller_Figure2.jpg') #NEW
      file.copy(imageToSend3, 'RmdFiller_Figure3.jpg') #NEW
      file.copy(imageToSend4, 'RmdFiller_Table1.jpg') #NEW
      file.copy(imageToSend5, 'RmdFiller_Table2.jpg') #NEW
      file.copy(imageToSend6, 'RmdFiller_Table3.jpg') #NEW
      file.copy(imageToSend7, 'Parameters_Briery.PNG') #NEW
      file.copy(imageToSend8, 'Stressor_legend.PNG') #NEW
      file.copy(imageToSend9, 'Table1_VSCImetrics.PNG') #NEW
      params <- list(userDataRMD=userData)
      
      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})
  
  output$report_Word63 <- downloadHandler(
    paste(unique(userData[["stats_wTemp"]][,1])[1],"_BenthicStressorReportTemplateVCPMI63+Chowan.docx",sep=""),
    content= function(file){
      tempReport <- normalizePath('BenthicStressorReportTemplate_63.Rmd')
      imageToSend1 <- normalizePath('RmdFiller_Figure1.jpg') #NEW 
      imageToSend2 <- normalizePath('RmdFiller_Figure2.jpg') #NEW 
      imageToSend3 <- normalizePath('RmdFiller_Figure3.jpg') #NEW 
      imageToSend4 <- normalizePath('RmdFiller_Table1.jpg') #NEW 
      imageToSend5 <- normalizePath('RmdFiller_Table2.jpg') #NEW 
      imageToSend6 <- normalizePath('RmdFiller_Table3.jpg') #NEW 
      imageToSend7 <- normalizePath('Parameters_Briery.PNG') #NEW 
      imageToSend8 <- normalizePath('Stressor_legend.PNG') #NEW 
      imageToSend9 <- normalizePath('Table1_VSCImetrics.PNG') #NEW 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(tempReport, 'BenthicStressorReportTemplate_63.Rmd')
      file.copy(imageToSend1, 'RmdFiller_Figure1.jpg') #NEW
      file.copy(imageToSend2, 'RmdFiller_Figure2.jpg') #NEW
      file.copy(imageToSend3, 'RmdFiller_Figure3.jpg') #NEW
      file.copy(imageToSend4, 'RmdFiller_Table1.jpg') #NEW
      file.copy(imageToSend5, 'RmdFiller_Table2.jpg') #NEW
      file.copy(imageToSend6, 'RmdFiller_Table3.jpg') #NEW
      file.copy(imageToSend7, 'Parameters_Briery.PNG') #NEW
      file.copy(imageToSend8, 'Stressor_legend.PNG') #NEW
      file.copy(imageToSend9, 'Table1_VSCImetrics.PNG') #NEW
      params <- list(userDataRMD=userData)
      
      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})
  
  output$report_Word65 <- downloadHandler(
    paste(unique(userData[["stats_wTemp"]][,1])[1],"_BenthicStressorReportTemplateCPMI65-Chowan.docx",sep=""),
    content= function(file){
      tempReport <- normalizePath('BenthicStressorReportTemplate_65.Rmd')
      imageToSend1 <- normalizePath('RmdFiller_Figure1.jpg') #NEW 
      imageToSend2 <- normalizePath('RmdFiller_Figure2.jpg') #NEW 
      imageToSend3 <- normalizePath('RmdFiller_Figure3.jpg') #NEW 
      imageToSend4 <- normalizePath('RmdFiller_Table1.jpg') #NEW 
      imageToSend5 <- normalizePath('RmdFiller_Table2.jpg') #NEW 
      imageToSend6 <- normalizePath('RmdFiller_Table3.jpg') #NEW 
      imageToSend7 <- normalizePath('Parameters_Briery.PNG') #NEW 
      imageToSend8 <- normalizePath('Stressor_legend.PNG') #NEW 
      imageToSend9 <- normalizePath('Table1_VSCImetrics.PNG') #NEW 
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(tempReport, 'BenthicStressorReportTemplate_65.Rmd')
      file.copy(imageToSend1, 'RmdFiller_Figure1.jpg') #NEW
      file.copy(imageToSend2, 'RmdFiller_Figure2.jpg') #NEW
      file.copy(imageToSend3, 'RmdFiller_Figure3.jpg') #NEW
      file.copy(imageToSend4, 'RmdFiller_Table1.jpg') #NEW
      file.copy(imageToSend5, 'RmdFiller_Table2.jpg') #NEW
      file.copy(imageToSend6, 'RmdFiller_Table3.jpg') #NEW
      file.copy(imageToSend7, 'Parameters_Briery.PNG') #NEW
      file.copy(imageToSend8, 'Stressor_legend.PNG') #NEW
      file.copy(imageToSend9, 'Table1_VSCImetrics.PNG') #NEW
      params <- list(userDataRMD=userData)
      
      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})
  
  output$table5 <- DT::renderDataTable({
    req(usr_bugData())
    
    bugData <- usr_bugData() %>%
      dplyr::rename(FamSCI = `Fam SCI`,SampleSeason= `Sample Season`) 
    
    bugData$SampleSeason <- factor(bugData$SampleSeason, levels=unique(as.character(bugData$SampleSeason)) )
    
    
    bugScores <- select(bugData, -c(StationID,CollDate, RepNum, FamTotTaxa, FamEPTTax, `%Ephem`, `%PT - Hydropsychidae`, `Fam%Scrap`, `%Chiro`, 
                                    `Fam%2Dom`, FamHBI, Season, Year, Target_Count, StreamName, Location ))
    bugScores <- select(bugData, SampleSeason,`%Ephem Score`,`%PT-H Score`,`%Chironomidae Score`,`Fam Richness Score`,
                        `Fam EPT Score`,`Fam %Scraper Score`,`Fam %2Dom Score`,`Fam %MFBI Score`,FamSCI)
    
    bugScores[1:nrow(bugScores),2:10]%>% 
      datatable(rownames =  as.character(bugScores$SampleSeason),
                colnames = c("% Ephem Score","% PT- H Score","% Chironomidae Score","Fam Richness Score",
                             "Fam EPT Score","Family %Scraper Score","Family %2 Domniant Score","Family %MFBI Score","VSCI"),
                options=list(dom= 't',
                             rowCallback = # the data.length-1) part makes it only calculate each column but VSCI
                               JS('function(row, data) {
                                  var num_data = data.slice(1,data.length-1)
                                  num_data.sort(function (a, b) {  return a - b;  });
                                  for(i=1;i < data.length; i++) {
                                  if(data[i]==num_data[num_data.length-1]) {
                                  $("td:eq("+i+")", row).css("background-color", "none")
                                  } else if(data[i]==num_data[0]) {
                                  $("td:eq("+i+")", row).css("background-color", "orange")
                                  } else if(data[i]==num_data[1]) {
                                  $("td:eq("+i+")", row).css("background-color", "orange")
                                  }}}')
                         )) %>% 
      formatStyle("FamSCI", backgroundColor = "lightgray", color=styleInterval(c(60),c('red','black')),
                  fontWeight = styleInterval(c(60),c('bold','normal'))) %>% 
      formatRound(c("%Ephem Score","%PT-H Score","%Chironomidae Score","Fam Richness Score","Fam EPT Score","Fam %Scraper Score",
                    "Fam %2Dom Score","Fam %MFBI Score","FamSCI"), 2)
    
    
  })
  
  output$table6 <- DT::renderDataTable({
    req(usr_habitatData())
    
    habData <- usr_habitatData()
    
    brks <- 1:19
    clrs <- c("#8B0000", "#9D0000", "#AF0000", "#C10000", "#D40000", "#E60000", "#F80000", "#FF1415", "#FF3235", "#FF5055", "#FF6F75",
              "#FF8D95", "#FFABB5", "#FFC3CD", "#FFCDD5", "#FFD7DE", "#FFE1E6", "#FFEBEE", "#FFF5F6", "#FFFFFF")
    
    
    if("POOLSUB" %in% unique(habData$HabParameter) | 'POOLVAR' %in% unique(habData$HabParameter) ){ 
      # Low Gradient Habitat Method
      habData$CollDate <- as.Date(as.character(habData$CollDate),format="%Y-%m-%d")
      totHabData <- select(habData,StationID,CollDate,TotHabSc)[!duplicated(select(habData,StationID,CollDate,TotHabSc)),] # get unique sample dates
      habData1 <- select(habData,-c(TotHabSc)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
        filter(HabParameter %in% c("ALTER","BANKS","BANKVEG","FLOW","POOLSUB","POOLVAR","RIPVEG","SEDIMENT","SINUOSITY",
                                   "SUBSTRATE","COVER")) %>% # get only low gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
        spread(HabParameter,HabValue) %>%
        filter(!is.na(POOLSUB) & !is.na(POOLVAR) & !is.na(SINUOSITY)) # if both methods used then get rid of sample dates not sampled as low gradient
      
      # Deal with COVER terminology if present
      if("COVER" %in% names(habData1)){
        habData1 <- habData1 %>% rowwise()%>%
          mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::rename(TotalHabitat = TotHabSc) %>%
          arrange(CollDate)
      }else{
        habData1 <- habData1 %>% rowwise()%>%
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::rename(TotalHabitat = TotHabSc) %>%
          arrange(CollDate)
      }
      
      DT::datatable(habData1,escape=F, rownames = F, 
                    colnames = c('Station ID','Date','Channel Alteration','Bank Stability','Bank Vegetation', 
                                 'Flow', 'Pool Substrate','Pool Variability', 'Riparian Vegetation', 
                                 'Sediment', 'Sinuosity', 'Substrate', 'Total Habitat'),
                    options=list(pageLength=nrow(habData1),dom= 'Bt' )) %>%
        formatStyle(names(habData1)[3:12], backgroundColor = styleInterval(brks, clrs), 
                    textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal')), 
                    textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle('TotalHabitat', backgroundColor = "lightgray")
    }
    if("RIFFLES" %in% unique(habData$HabParameter) | "VELOCITY" %in% unique(habData$HabParameter) ){ # High Gradient Habitat Method
      habData$CollDate <- as.Date(as.character(habData$CollDate),format="%Y-%m-%d")
      totHabData <- select(habData,StationID,CollDate,TotHabSc)[!duplicated(select(habData,StationID,CollDate,TotHabSc)),] # get unique sample dates
      habData1 <- select(habData,-c(TotHabSc)) %>% # these two fields can cause multiple rows for each date in the spread() step, so get rid of them for now
        filter(HabParameter %in% c('ALTER','BANKS','BANKVEG','COVER','EMBED','FLOW','RIFFLES','RIPVEG','SEDIMENT',
                                   'SUBSTRATE','VELOCITY')) %>% # get only high gradient parameters in case both happened to be sampled for site) and using filter is safer than select at a later step
        spread(HabParameter,HabValue) %>%
        filter(!is.na(EMBED) & !is.na(RIFFLES) & !is.na(VELOCITY)) # if both methods used then get rid of sample dates not sampled as high gradient
      
      # Deal with COVER terminology if present
      if("COVER" %in% names(habData1)){
        habData1 <- habData1 %>% rowwise()%>%
          mutate(SUBSTRATE=sum(COVER,SUBSTRATE,na.rm=T)) %>% select(-c(COVER)) %>% # Get rid of old cover terminology
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::rename(TotalHabitat = TotHabSc) %>%
          arrange(CollDate)
      }else{
        habData1 <- habData1 %>% rowwise()%>%
          left_join(totHabData, by=c("StationID","CollDate")) %>%
          dplyr::rename(TotalHabitat = TotHabSc) %>%
          arrange(CollDate)
      }
      DT::datatable(habData1,escape=F, rownames = F, colnames = c('Station ID','Date','Channel Alteration','Banks', 
                                                                  'Bank Vegetation', 'Embeddedness', 
                                                                  'Flow', 'Riffles', 'Riparian Vegetation', 
                                                                  'Sediment', 'Substrate','Velocity', 'Total Habitat'),
                    options=list(pageLength=nrow(habData1),dom= 'Bt' )) %>%
        formatStyle(names(habData1)[3:12], backgroundColor = styleInterval(brks, clrs), 
                    textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle(names(habData1), fontWeight = styleInterval(10, c('bold','normal')), 
                    textAlign = 'center', `font-family` = 'Arial') %>%
        formatStyle('TotalHabitat', backgroundColor = "lightgray")
    }
  })
  
  output$table7 <- DT::renderDataTable({
    req(stats())
    datatable(userData$stats, escape=F, rownames = F,
              options=list(dom='Bt'))%>%
      formatStyle("pH", backgroundColor = styleInterval(pHRiskTable$brks, pHRiskTable$clrs))%>%
      formatStyle("DO", backgroundColor = styleInterval(DORiskTable$brks, DORiskTable$clrs)) %>%
      formatStyle("TN", backgroundColor = styleInterval(TNRiskTable$brks, TNRiskTable$clrs))%>%
      formatStyle("TP", backgroundColor = styleInterval(TPRiskTable$brks, TPRiskTable$clrs))%>%
      formatStyle("TotalHabitat", backgroundColor = styleInterval(TotHabRiskTable$brks, TotHabRiskTable$clrs))%>%
      formatStyle("LRBS", backgroundColor = styleInterval(LRBSRiskTable$brks, LRBSRiskTable$clrs))%>%
      formatStyle("MetalsCCU", backgroundColor = styleInterval(MetalsCCURiskTable$brks, MetalsCCURiskTable$clrs))%>%
      formatStyle("SpCond", backgroundColor = styleInterval(SpCondRiskTable$brks, SpCondRiskTable$clrs))%>%
      formatStyle("TDS", backgroundColor = styleInterval(TDSRiskTable$brks, TDSRiskTable$clrs))%>%
      formatStyle("DSulfate", backgroundColor = styleInterval(DSulfateRiskTable$brks, DSulfateRiskTable$clrs))%>%
      formatStyle("DChloride", backgroundColor = styleInterval(DChlorideRiskTable$brks, DChlorideRiskTable$clrs))%>%
      formatStyle("DPotassium", backgroundColor = styleInterval(DPotassiumRiskTable$brks, DPotassiumRiskTable$clrs))%>%
      formatStyle("DSodium", backgroundColor = styleInterval(DSodiumRiskTable$brks, DSodiumRiskTable$clrs))
  }, digits=4,border=TRUE)

  
  
  
  
  }


## TESTING ZONE

#ui <- fluidPage(theme = shinythemes::shinytheme("yeti"),
#                fileInput('siteData','Upload Site (flat file)',accept='.csv',width='100%'),
#                div(style = 'overflow-x: scroll',tableOutput("summaryStats")),
#                finalReportUI('report')
#)

#server <- shinyServer(function(input, output, session) {
#  userData <- reactiveValues()
  
#  inputFile <- reactive({read.csv('testData/LOGImanipulatedData_2018-04-232-CNE000.96.csv')
    #inFile <- input$siteData
  #if(is.null(inFile))
  #  return(NULL)
  #read.csv(inFile$datapath)
#    })
  
#  stats <- reactive({
#    req(inputFile())
#    
#    # Deal with columns of only NA coming in as logical
#    dat <- inputFile() %>% select(-(Temp))
#    dat <- japply( dat, which(sapply(dat, class)=="logical"), as.numeric )
#    
#    datamean <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
#      summarise_all(funs(format(mean(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Average")
#    datamean[datamean %in% c("NaN","NA")] <- NA
#    datamedian <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
#      summarise_all(funs(format(median(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Median")
#    datamedian[datamedian %in% c("NaN","NA")] <- NA
#    data_all <- rbind(datamean,datamedian)%>%select(Statistic,everything())
#    return(data_all)
#  })
  # Save objects to reactive values for better organization, keep stats() separate to enable easy req() calls 
  #   for later functions
#  observe(userData$stats <- stats())
#  observe(userData$stats_wTemp <- inputFile())
#  output$summaryStats <- renderTable(userData$stats_wTemp)
  
  
#  callModule(finalReport,'report', userData)
#})

#shinyApp(ui,server)

