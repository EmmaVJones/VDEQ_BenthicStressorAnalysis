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

siteData <- removeUnits_envDataDF(read.csv('C:/HardDriveBackup/R/GitHub/WQMdataQueryTool/data/BSAmessAround/BSAtemplateData4ADEE000.062021-04-23 (3).csv')) 
  #read.csv('C:/HardDriveBackup/R/GitHub/BenthicStressorAnalysiswithLogi/testData/4ADEE000.06/LOGImanipulatedData_2020-10-294ADEE000.06.csv')) 
#removeUnits_envDataDF(siteData)


# Deal with columns of only NA coming in as logical
dat <- siteData %>% select(-(Temp))
dat <- japply( dat, which(sapply(dat, class)=="logical"), as.numeric )

datamean <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
  summarise_all(funs(format(mean(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Average")
datamean[datamean %in% c("NaN","NA")] <- NA
datamedian <- select(dat,-c(StationID,CollectionDateTime,Longitude,Latitude))%>%
  summarise_all(funs(format(median(., na.rm = TRUE),digits=4)))%>%mutate(Statistic="Median")
datamedian[datamedian %in% c("NaN","NA")] <- NA
stats <- rbind(datamean,datamedian)%>%select(Statistic,everything())
 

stats_wGIS <- reshape2::melt(stats,c("Statistic"))%>%
  dcast(variable~Statistic)%>%
  mutate(StationID=unique(siteData$StationID),
         Longitude=unique(siteData$Longitude),
         Latitude=unique(siteData$Latitude))



Basin <- 'Roanoke Basin'
Ecoregion <- 'Blue Ridge Mountains'
StreamOrder <- 'Third Order'

percentiles <- list(pH = percentileTable(stats,"pH",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    DO = percentileTable(stats,"DO",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    TN = percentileTable(stats,"TN",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    TP = percentileTable(stats,"TP",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    TotHab = percentileTable(stats,"TotalHabitat",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    LRBS = percentileTable(stats,"LRBS",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    MetalsCCU = percentileTable(stats,"MetalsCCU",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    SpCond = percentileTable(stats,"SpCond",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    TDS = percentileTable(stats,"TDS",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    DSulfate = percentileTable(stats,"DSulfate",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    DChloride = percentileTable(stats,"DChloride",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    DPotassium = percentileTable(stats,"DPotassium",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)),
                    DSodium = percentileTable(stats,"DSodium",Basin,Ecoregion,StreamOrder,unique(siteData$StationID)))





## Metals Testing
inputFile_metals1 <- suppressWarnings(read.csv('C:/Users/wmu43954/Downloads/BSAtemplateData2-JKS023.612021-04-23.csv'))
# warning reads "Warning in read.table(file = file, header = header, sep = sep, quote = quote,  :incomplete final line found by readTableHeader on 'C:\Users\wmu43954\AppData\Local\Temp\Rtmp8UbZB0/9c053c0a16c8bc1f80cb857d/0.csv'" 
datatable(inputFile_metals1,escape=F, rownames = F,options=list(scrollX = TRUE))
metals <- metalsCCUcalcDF(inputFile_metals1) %>%
  mutate(StationID=inputFile_metals1$StationID)%>%
  select(StationID,CollectionDateTime,MetalsCCU)
metalsSites_ <- unique(inputFile_metals1$StationID)[1]


  Calcium <- percentileTable_metals(inputFile_metals1,'Calcium',metalsSites_)
  Magnesium <- percentileTable_metals(inputFile_metals1,'Magnesium',metalsSites_)
  Arsenic <- percentileTable_metals(inputFile_metals1,'Arsenic',metalsSites_)
  Barium <- percentileTable_metals(inputFile_metals1,'Barium',metalsSites_)
  Beryllium <- percentileTable_metals(inputFile_metals1,'Beryllium',metalsSites_)
  Cadmium <- percentileTable_metals(inputFile_metals1,'Cadmium',metalsSites_)
  Chromium <- percentileTable_metals(inputFile_metals1,'Chromium',metalsSites_)
  Copper <- percentileTable_metals(inputFile_metals1,'Copper',metalsSites_)
  Iron <- percentileTable_metals(inputFile_metals1,'Iron',metalsSites_)
  Lead <- percentileTable_metals(inputFile_metals1,'Lead',metalsSites_)
  Manganese <- percentileTable_metals(inputFile_metals1,'Manganese',metalsSites_)
  Thallium <- percentileTable_metals(inputFile_metals1,'Thallium',metalsSites_)
  Nickel <- percentileTable_metals(inputFile_metals1,'Nickel',metalsSites_)
  Silver <- percentileTable_metals(inputFile_metals1,'Silver',metalsSites_)
  Zinc <- percentileTable_metals(inputFile_metals1,'Zinc',metalsSites_)
  Antimony <- percentileTable_metals(inputFile_metals1,'Antimony',metalsSites_)
  Aluminum <- percentileTable_metals(inputFile_metals1,'Aluminum',metalsSites_)
  Selenium <- percentileTable_metals(inputFile_metals1,'Selenium',metalsSites_)
  Hardness <- percentileTable_metals(inputFile_metals1,'Hardness',metalsSites_)
  percentilesDissolvedMetals_ <- rbind(Calcium,Magnesium,Arsenic,Barium,Beryllium,Cadmium,Chromium,Copper,Iron,
                 Lead,Manganese,Thallium,Nickel,Silver,Zinc,Antimony,Aluminum,Selenium,Hardness)
  

  H <- as.numeric(percentilesDissolvedMetals_[19,2])
  criteria <- metalsCriteriaAssessment(H)
  criteria[,2:4] <- suppressWarnings(apply(criteria[,2:4], 2, function(x) as.numeric(as.character(x))))
  percentilesDissolvedMetals2_ <- suppressWarnings(left_join(percentilesDissolvedMetals_,criteria,by='Dissolved_Metal')%>%
                                                    mutate(ChronicAssessment=ifelse(Measure>Chronic,'Exceedance','No Exceedance'),
                                                           AcuteAssessment=ifelse(Measure>Acute,'Exceedance','No Exceedance'),
                                                           PWSAssessment=ifelse(Measure>PWS,'Exceedance','No Exceedance')))
  names(inputFile_metals1[5:length(inputFile_metals1)])

  
  
  
  
  
  
  
  dissolvedMetalsUI <- function(id){
    ns <- NS(id)
    tagList(
      br(),br(),
      #verbatimTextOutput('test'),
      uiOutput(ns('dMetal')),
      checkboxInput(ns('addstd'),strong('Add Acute Criteria to CDF Plot.')),
      uiOutput(ns('dMetalplot_'))
    )
  }
  
  dissolvedMetals <- function(input,output,session,inputFile_metals,
                              percentilesDissolvedMetals,percentilesDissolvedMetals2){
    
    output$test <- renderPrint({names(inputFile_metals()[5:length(inputFile_metals())])})
    # Choose Dissolved Metal to display
    output$dMetal <- renderUI({
      selectInput(session$ns("dMetal_"),"Select Dissolved Metal to Plot",names(inputFile_metals()[5:length(inputFile_metals())]))
    })
    output$dMetalplot_ <- renderUI({
      plotOutput(session$ns("p_dMetal"))})
    # Updating dissolved metals cdf plot
    output$p_dMetal <- renderPlot({
      req(inputFile_metals())
      parametercap <- toupper(input$dMetal_)
      cdfsubset <- subFunction(cdfdata,parametercap,"Virginia")
      pct1 <- cbind(percentilesDissolvedMetals(),metal=sub(" .*","",percentilesDissolvedMetals()$Dissolved_Metal))%>%
        filter(metal==input$dMetal_)
      pct <- cbind(cdfsubset[1,1:3],Value=pct1$Measure,Estimate.P=as.numeric(as.character(pct1$Statewide_Percentile)),
                   cdfsubset[1,6:8])
      m <- max(cdfsubset$NResp)
      p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + geom_point() + labs(x=as.character(pct1[1,1]),y="Percentile") +
        ggtitle(paste("Virginia",input$dMetal_,"\nPercentile Graph( n=",m,")",sep=" ")) + 
        theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
        theme(axis.title = element_text(face='bold',size=12))+
        geom_point(data=pct,color='orange',size=4)
      
      std <- cbind(percentilesDissolvedMetals2()[,c(1,4)],metal=sub(" .*","",percentilesDissolvedMetals()$Dissolved_Metal))%>%
        filter(metal==input$dMetal_)
      
      if(input$addstd==F){return(p1)}else{
        if(is.na(std[1,2])){
          xloc <- 0.75*max(cdfsubset$Value)
          p1+annotate('text',x=xloc,y=50,label='No Criteria',color='red', fontface =2)}else{
            p1+geom_vline(xintercept=as.numeric(as.character(std[1,2])),color='red',linetype='dashed')}
      }},height = 250,width=325)
  }
  
  
  
  ui <- fluidPage(
    
    dissolvedMetalsUI('dMetalsPlots')
  )
  
  server <- function(input,output,session){
    
    inputFile_metals <- reactive({inputFile_metals1})
    percentilesDissolvedMetals <- reactive({percentilesDissolvedMetals_})
    percentilesDissolvedMetals2  <- reactive({percentilesDissolvedMetals2_})
    
    callModule(dissolvedMetals,'dMetalsPlots',inputFile_metals,
               percentilesDissolvedMetals, percentilesDissolvedMetals2)
    
  }
  
  shinyApp(ui,server)
  
  
  
  
  
