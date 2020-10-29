source('global.R')
#source('global_appModules.R')
source('global_appModules_new.R')
source('global_logiManipulationModule.R')
source('global_finalReportModule.R')



# Upload GIS data here to avoid uploading it twice (if it were in the global.R file)
#eco <- readOGR('data','vaECOREGIONlevel3__proj84')
#supaB <- readOGR('data','VAsuperbasins_proj84')
eco <- st_read('data/vaECOREGIONlevel3__proj84.shp')
supaB <- st_read('data/VAsuperbasins_proj84.shp')

options(DT.options = list(dom = 't'))

shinyServer(function(input, output, session) {
  # Reactive Value to store all user data
  userData <- reactiveValues()
  
  
  ##### How To Tab ----------------------------------------------------------------------------------------------------------------
  # Bring in pre-rendered HTML instruction page. This is built from the TMDLBenthicStressorToolHowTo.Rmd run outside the app itself
  #   to save time and to ensure all HTML formatting comes in correctly. *NOTE* includeHTML() in the ui broke the app
  #   bc of the inherent <body></body> within the ui <body></body> so this server workaround is necessary.
  output$TMDLBenthicStressorToolHowTo<-renderUI({includeHTML("TMDLBenthicStressorToolHowTo.html")})
  
  ##### LOGI Manipulation Tab ----------------------------------------------------------------------------------------------------------------
  
  # Module for LOGI data manipulation
  callModule(logiManipulation,'logi')
  
  
  
  ##### Data Upload Tab ------------------------------------------------------------------------------------------------------------
  
  # User Upload- Bring in user manipulated chemistry/field data
  # Download data template
  output$downloadTemplate <- downloadHandler(filename=function(){'template.csv'},
                                             content=function(file){write.csv(template,file,row.names=FALSE)})
  
  # Upload user manipulated site data
  inputFile <- reactive({inFile <- input$siteData
  if(is.null(inFile))
    return(NULL)
  removeUnits_envDataDF(read.csv(inFile$datapath)) # Remove Units for all analysis purposes
  })
  
  # Display user input data
  output$inputTable <- DT::renderDataTable({
    DT::datatable(inputFile(),escape=F, rownames = F,
                  options=list(scrollX = TRUE, scrollY = "300px",pageLength=nrow(inputFile())))})
  
  # Calculate statistics on input table
  stats <- reactive({
    req(input$siteData)
    
    # Deal with columns of only NA coming in as logical
    dat <- inputFile() %>% select(-(Temp))
    dat <- japply( dat, which(sapply(dat, class)=="logical"), as.numeric )

    datamean <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
      summarise_all(funs(format(mean(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Average")
    datamean[datamean %in% c("NaN","NA")] <- NA
    datamedian <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
      summarise_all(funs(format(median(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Median")
    datamedian[datamedian %in% c("NaN","NA")] <- NA
    data_all <- rbind(datamean,datamedian)%>%select(Statistic,everything())
    return(data_all)
    })
  # Save objects to reactive values for better organization, keep stats() separate to enable easy req() calls 
  #   for later functions
  observe(userData$stats <- stats())
  
  observe(userData$stats_wTemp <- inputFile())
  
  observe(
    userData$stats_wGIS <- reshape2::melt(stats(),c("Statistic"))%>%
      dcast(variable~Statistic)%>%
      mutate(StationID=unique(inputFile()$StationID),
             Longitude=unique(inputFile()$Longitude),
             Latitude=unique(inputFile()$Latitude)))
  
  observe(userData$userInputs <- list(Basin = input$Basin, Ecoregion = input$Ecoregion, StreamOrder = input$StreamOrder))

  # Display summary statistics
  output$summaryStats <- renderTable(userData$stats)

  
  # Metals CCU Calculation
  callModule(metalCCUsingleSite,'metalCCUsingleSite')
  
  
  ##### Data Summary Tab ------------------------------------------------------------------------------------------------------------
  
  # Reactive Value to store all percentile tables
  observe(
    userData$percentiles <- list(pH = percentileTable(stats(),"pH",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 DO = percentileTable(stats(),"DO",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 TN = percentileTable(stats(),"TN",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 TP = percentileTable(stats(),"TP",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 TotHab = percentileTable(stats(),"TotalHabitat",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 LRBS = percentileTable(stats(),"LRBS",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 MetalsCCU = percentileTable(stats(),"MetalsCCU",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 SpCond = percentileTable(stats(),"SpCond",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 TDS = percentileTable(stats(),"TDS",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 DSulfate = percentileTable(stats(),"DSulfate",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 DChloride = percentileTable(stats(),"DChloride",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 DPotassium = percentileTable(stats(),"DPotassium",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID)),
                                 DSodium = percentileTable(stats(),"DSodium",input$Basin,input$Ecoregion,input$StreamOrder,unique(inputFile()$StationID))))
  
  ### Composite Table Tab

  # Output Colored datatable
  output$colors <- DT::renderDataTable({
    req(stats())
    datatable(userData$stats, extensions = 'Buttons', escape=F, rownames = F,
              options=list(dom='Bt',buttons=list('copy',
                                                 list(extend='csv',filename=paste('CompositeTable_',Sys.Date(),sep='')),
                                                 list(extend='excel',filename=paste('CompositeTable_',Sys.Date(),sep='')),
                                                 list(extend='pdf',orientation='landscape',filename=paste('CompositeTable_',Sys.Date(),sep='')))))%>%
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
  
  # Output Colored Risk datatable
  output$riskTableInfo <- DT::renderDataTable({
    datatable(risk,colnames=c('Risk Category'),rownames=FALSE) %>% 
      formatStyle('Risk_Category',backgroundColor=styleEqual(brksrisk,clrsrisk))
  })
  
  ### pH Table Tab
  callModule(parameterSummaryTab,'pH_Summary',userData[["percentiles"]][["pH"]],'unitless',pHRiskTable)
  callModule(cdfSubpopPlot,"pH_CDFplot","pH", "pH", userData[["percentiles"]][["pH"]],pHRiskTable, pHsettingsCDF)
  
  ### DO Table Tab
  callModule(parameterSummaryTab,'DO_Summary',userData[["percentiles"]][["DO"]],'mg/L', DORiskTable)
  callModule(cdfSubpopPlot,"DO_CDFplot","Dissolved Oxygen","DO",userData[["percentiles"]][["DO"]],DORiskTable,DOsettingsCDF)
  
  ### TN Table Tab
  callModule(parameterSummaryTab,'TN_Summary',userData[["percentiles"]][["TN"]],'mg/L', TNRiskTable)
  callModule(cdfSubpopPlot,"TN_CDFplot","Total Nitrogen","TN",userData[["percentiles"]][["TN"]],TNRiskTable,TNsettingsCDF)
  
  ### TP Table Tab
  callModule(parameterSummaryTab,'TP_Summary',userData[["percentiles"]][["TP"]],'mg/L', TPRiskTable)
  callModule(cdfSubpopPlot,"TP_CDFplot","Total Phosphorus","TP",userData[["percentiles"]][["TP"]],TPRiskTable,TPsettingsCDF)
  
  ### Total Habitat Table Tab
  callModule(parameterSummaryTab,'TotHab_Summary',userData[["percentiles"]][["TotHab"]],'unitless', TotHabRiskTable)
  callModule(cdfSubpopPlot,"TotHab_CDFplot","Total Habitat", "TotalHabitat",userData[["percentiles"]][["TotHab"]],TotHabRiskTable, TotHabsettingsCDF)
  
  ### LRBS Table Tab
  callModule(parameterSummaryTab,'LRBS_Summary',userData[["percentiles"]][["LRBS"]],'unitless', LRBSRiskTable)
  callModule(cdfSubpopPlot,"LRBS_CDFplot","LRBS", "LRBS",userData[["percentiles"]][["LRBS"]],LRBSRiskTable,LRBSsettingsCDF)
  
  ### MetalsCCU Table Tab
  callModule(parameterSummaryTab,'MetalsCCU_Summary',userData[["percentiles"]][["MetalsCCU"]],'unitless', MetalsCCURiskTable)
  callModule(cdfSubpopPlot,"MetalsCCU_CDFplot","Metals CCU", "MetalsCCU",userData[["percentiles"]][["MetalsCCU"]],MetalsCCURiskTable,MetalsCCUsettingsCDF)
  
  ### SpCond Table Tab
  callModule(parameterSummaryTab,'SpCond_Summary',userData[["percentiles"]][["SpCond"]],'uS/cm', SpCondRiskTable)
  callModule(cdfSubpopPlot,"SpCond_CDFplot","Specific Conductivity", "SpCond",userData[["percentiles"]][["SpCond"]],SpCondRiskTable,SpCondsettingsCDF)
  
  ### TDS Table Tab
  callModule(parameterSummaryTab,'TDS_Summary',userData[["percentiles"]][["TDS"]],'mg/L', TDSRiskTable)
  callModule(cdfSubpopPlot,"TDS_CDFplot","Total Dissolved Solids", "TDS",userData[["percentiles"]][["TDS"]],TDSRiskTable,TDSsettingsCDF)
  
  ### DSulfate Table Tab
  callModule(parameterSummaryTab,'DSulfate_Summary',userData[["percentiles"]][["DSulfate"]],'mg/L', DSulfateRiskTable)
  callModule(cdfSubpopPlot,"DSulfate_CDFplot","Dissolved Sulfate", "DSulfate",userData[["percentiles"]][["DSulfate"]],DSulfateRiskTable,DSulfatesettingsCDF)
  
  ### DChloride Table Tab
  callModule(parameterSummaryTab,'DChloride_Summary',userData[["percentiles"]][["DChloride"]],'mg/L', DChlorideRiskTable)
  callModule(cdfSubpopPlot,"DChloride_CDFplot","Dissolved Chloride", "DChloride",userData[["percentiles"]][["DChloride"]],DChlorideRiskTable,DChloridesettingsCDF)
  
  ### DPotassium Table Tab
  callModule(parameterSummaryTab,'DPotassium_Summary',userData[["percentiles"]][["DPotassium"]],'mg/L', DPotassiumRiskTable)
  callModule(cdfSubpopPlot,"DPotassium_CDFplot","Dissolved Potassium", "DPotassium",userData[["percentiles"]][["DPotassium"]],DPotassiumRiskTable,DPotassiumsettingsCDF)
  
  ### DSodium Table Tab
  callModule(parameterSummaryTab,'DSodium_Summary',userData[["percentiles"]][["DSodium"]],'mg/L', DSodiumRiskTable)
  callModule(cdfSubpopPlot,"DSodium_CDFplot","Dissolved Sodium", "DSodium",userData[["percentiles"]][["DSodium"]],DSodiumRiskTable,DSodiumsettingsCDF)
  
  
  ##### Map Tab ------------------------------------------------------------------------------------------------------------
  callModule(statewideMap,'statewideMap',userData$stats, userData$stats_wGIS, supaB, eco)
  
  
  ##### Dissolved Metals Tab -----------------------------------------------------------------------------------------------
  
  ### User Data Tab
  
  # Download data template
  output$downloadTemplate_metals <- downloadHandler(filename=function(){'template_metals.csv'
    },content=function(file){write.csv(template_metals,file,row.names=FALSE)})
  
  # Upload site data
  inputFile_metals <- reactive({inFile2 <- input$siteData_metals
  if(is.null(inFile2))
    return(NULL)
  suppressWarnings(read.csv(inFile2$datapath))}) # suppress the warning here bc missing End Of Line (EOL) character is missing from csv's downloaded from the DT button
  # warning reads "Warning in read.table(file = file, header = header, sep = sep, quote = quote,  :incomplete final line found by readTableHeader on 'C:\Users\wmu43954\AppData\Local\Temp\Rtmp8UbZB0/9c053c0a16c8bc1f80cb857d/0.csv'" 
  output$inputTable_metals <- DT::renderDataTable({datatable(inputFile_metals(),escape=F, rownames = F,options=list(scrollX = TRUE))})
  
  # Save objects to reactive values for better organization, keep inputFile_metals() separate to enable easy req() calls 
  #   for later functions
  observe({
    req(inputFile_metals())
    userData$metals <- metalsCCUcalcDF(inputFile_metals()) %>%
      mutate(StationID=inputFile_metals()$StationID)%>%
      select(StationID,CollectionDateTime,MetalsCCU)})

  # Display Metal CCU calculation 
  output$summary_MetalsCCU <- renderDataTable({
    datatable(userData$metals,extensions = 'Buttons', escape=F, rownames = F,
              options=list(pageLength=nrow(userData$metals),
                           dom='Bt', #'Bfrtip',
                           buttons=list('copy',
                                        list(extend='csv',filename=paste('MetalsCCUAnalysis_',Sys.Date(),sep='')),
                                        list(extend='excel',filename=paste('MetalsCCUAnalysis_',Sys.Date(),sep='')))))})
  
  
  
  ### Data Summary Tab
  
  # Give users choices of stations to review from the inputFile_metals
  output$metalsSitesUI <- renderUI({
    req(inputFile_metals())
    selectInput("metalsSites_", "Select Site to Review", inputFile_metals()[,1] )})
  
  # Dissolved Metals Lookup Functions
  percentilesDissolvedMetals <- reactive({
    req(userData$metals, input$metalsSites_)
    Calcium <- percentileTable_metals(inputFile_metals(),'Calcium',input$metalsSites_)
    Magnesium <- percentileTable_metals(inputFile_metals(),'Magnesium',input$metalsSites_)
    Arsenic <- percentileTable_metals(inputFile_metals(),'Arsenic',input$metalsSites_)
    Barium <- percentileTable_metals(inputFile_metals(),'Barium',input$metalsSites_)
    Beryllium <- percentileTable_metals(inputFile_metals(),'Beryllium',input$metalsSites_)
    Cadmium <- percentileTable_metals(inputFile_metals(),'Cadmium',input$metalsSites_)
    Chromium <- percentileTable_metals(inputFile_metals(),'Chromium',input$metalsSites_)
    Copper <- percentileTable_metals(inputFile_metals(),'Copper',input$metalsSites_)
    Iron <- percentileTable_metals(inputFile_metals(),'Iron',input$metalsSites_)
    Lead <- percentileTable_metals(inputFile_metals(),'Lead',input$metalsSites_)
    Manganese <- percentileTable_metals(inputFile_metals(),'Manganese',input$metalsSites_)
    Thallium <- percentileTable_metals(inputFile_metals(),'Thallium',input$metalsSites_)
    Nickel <- percentileTable_metals(inputFile_metals(),'Nickel',input$metalsSites_)
    Silver <- percentileTable_metals(inputFile_metals(),'Silver',input$metalsSites_)
    Zinc <- percentileTable_metals(inputFile_metals(),'Zinc',input$metalsSites_)
    Antimony <- percentileTable_metals(inputFile_metals(),'Antimony',input$metalsSites_)
    Aluminum <- percentileTable_metals(inputFile_metals(),'Aluminum',input$metalsSites_)
    Selenium <- percentileTable_metals(inputFile_metals(),'Selenium',input$metalsSites_)
    Hardness <- percentileTable_metals(inputFile_metals(),'Hardness',input$metalsSites_)
    final <- rbind(Calcium,Magnesium,Arsenic,Barium,Beryllium,Cadmium,Chromium,Copper,Iron,
                   Lead,Manganese,Thallium,Nickel,Silver,Zinc,Antimony,Aluminum,Selenium,Hardness)
    return(final)})
  
  percentilesDissolvedMetals2 <- reactive({
    req(percentilesDissolvedMetals())
    H <- as.numeric(percentilesDissolvedMetals()[19,2])
    criteria <- metalsCriteriaAssessment(H)
    criteria[,2:4] <- suppressWarnings(apply(criteria[,2:4], 2, function(x) as.numeric(as.character(x))))
    percentilesDissolvedMetals2 <- suppressWarnings(left_join(percentilesDissolvedMetals(),criteria,by='Dissolved_Metal')%>%
                                                      mutate(ChronicAssessment=ifelse(Measure>Chronic,'Exceedance','No Exceedance'),
                                                             AcuteAssessment=ifelse(Measure>Acute,'Exceedance','No Exceedance'),
                                                             PWSAssessment=ifelse(Measure>PWS,'Exceedance','No Exceedance')))
    return(percentilesDissolvedMetals2)})
  
  
  output$colors_metals <- DT::renderDataTable({
    if(is.null(percentilesDissolvedMetals2()))
      return(NULL)
    DT::datatable(percentilesDissolvedMetals2(),extensions = 'Buttons', escape=F, rownames = F,
                  colnames = c('Dissolved Metal','Measure','Statewide Percentile','Acute Criteria',
                               'Chronic Criteria',"PWS Criteria",'Chronic Assessment','Acute Assessment','PWS Assessment'),
                  options=list(scrollX = TRUE,scrollY = "600px",pageLength=20,
                    dom='Bt', #'Bfrtip',
                    buttons=list('copy',
                                 list(extend='csv',filename=paste(input$metalsSites_,'StatewideDissolvedMetalsAnalysis_',Sys.Date(),sep='')),
                                 list(extend='excel',filename=paste(input$metalsSites_,'StatewideDissolvedMetalsAnalysis_',Sys.Date(),sep='')))))%>%
      formatStyle('ChronicAssessment',backgroundColor=styleEqual(c('No Exceedance','Exceedance'),c('white','red'))) %>%
      formatStyle('AcuteAssessment',backgroundColor=styleEqual(c('No Exceedance','Exceedance'),c('white','red'))) %>%
      formatStyle('PWSAssessment',backgroundColor=styleEqual(c('No Exceedance','Exceedance'),c('white','red')))
    })
  
  callModule(dissolvedMetals,'dMetalsPlots',inputFile_metals,
             percentilesDissolvedMetals, percentilesDissolvedMetals2)
  
  
  ####--------------------------------------- RMARKDOWN SECTION--------------------------------------------------
  
  
  ##### CDF REPORT -----------------------------------------------------------------------------------------------
  
  # Send input data and CDF data to html report
  output$report <- downloadHandler(
    paste(unique(userData[["stats_wTemp"]][,1])[1],"_CDFresults.html",sep=""),
    content= function(file){
      tempReport <- file.path(tempdir(),"reportHTML.Rmd")
      file.copy("reportHTML.Rmd",tempReport,overwrite = T)
      params <- list(userDataRMD=userData)#table_compositestats=userData$stats)
      
      rmarkdown::render(tempReport,output_file = file,
                        params=params,envir=new.env(parent = globalenv()))})
  
  ##### Word Document REPORT -----------------------------------------------------------------------------------------------
  callModule(finalReport,'report', userData, stats)
  
  #output$verbatim <- renderPrint({ unique(userData[["stats_wTemp"]][,1])[1]})
  #output$verbatim <- renderPrint({ userData[["percentiles"]][["pH"]][1,]})
})