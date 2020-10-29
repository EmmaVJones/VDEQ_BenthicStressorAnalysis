
metalCCUsingleSiteUI <- function(id){
  ns <- NS(id)
  tagList(
    h3("User Input Metals Data"),
    helpText("Please use this form to calculate MetalsCCU data for the User Data tab if you
                                                          have not done so already. Input StationID, Date, and associated metals data then
                                                          scroll down to view MetalsCCU result.",
             span(strong("If you have more than one site to analyze for MetalsCCU, use the 
                                                                      Dissolved Metals Tab at the top of the navigation pane."))),
    column(6,textInput(ns("StationID"),h5("StationID"), value = "")),
    column(6,textInput(ns("SampleDate"),h5("Sample Date (MM-DD-YYYY)"), value = "")),
    column(6,
           numericInput(ns("Hardness"),h5("Hardness"), value = ""),
           numericInput(ns("Aluminum"),h5("Aluminum"), value = ""),
           numericInput(ns("Arsenic"),h5("Arsenic"), value = ""),
           numericInput(ns("Cadmium"),h5("Cadmium"), value = ""),
           numericInput(ns("Chromium"),h5("Chromium"), value = "")),
    column(6,
           numericInput(ns("Copper"),h5("Copper"), value = ""),
           numericInput(ns("Lead"),h5("Lead"), value = ""),
           numericInput(ns("Nickel"),h5("Nickel"), value = ""),
           numericInput(ns("Selenium"),h5("Selemium"), value = ""),
           numericInput(ns("Zinc"),h5("Zinc"), value = "")),
    column(12,h3('MetalsCCU Result'),DT::dataTableOutput(ns('metalsTable')),
           helpText('You can copy/paste this value into your spreadsheet for upload
                                                                 back to this app or use it for additional analyses.')) )
}

metalCCUsingleSite <- function(input,output,session){
  mCCU <- reactive({
    if(is.null(input$Hardness))
      return(NULL)
    return(format(metalsCCUcalc(input$Hardness,input$Aluminum,input$Arsenic,input$Cadmium,input$Chromium,input$Copper,
                                input$Lead,input$Nickel,input$Selenium,input$Zinc),digits=4))
  })
  
  output$metalsTable <- renderDataTable({
    if(is.null(mCCU))#|is.null(input$StationID&input$SampleDate))
      return(data.frame(StationID=input$StationID,SampleDate=input$SampleDate,MetalsCCU=NA))
    datatable(data.frame(StationID=input$StationID,SampleDate=input$SampleDate,MetalsCCU=mCCU()),
              extensions = 'Buttons', escape=F, rownames = F,
              options= list(dom='Bt',buttons=list('copy')))})
  
}


parameterSummaryTabUI <- function(id){
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns('table_Site')),
    DT::dataTableOutput(ns('table')),br()
  )
}

parameterSummaryTab <- function(input,output,session, percentileData, unit, colorList){
  output$table_Site <- DT::renderDataTable({
    datatable(percentileData[1,],colnames = c('StationID',paste('Average (',unit,')',sep=""),paste('Median (',unit,')',sep="")),rownames = F)%>%
      formatStyle(c("Average","Median"), backgroundColor = styleInterval(colorList$brks, colorList$clrs))})
  output$table <- DT::renderDataTable({datatable(percentileData[2:5,],colnames=c('Subpopulation','Average Percentile','Median Percentile'),rowname=F)})
}



cdfSubpopPlotUI <- function(id){
  ns <- NS(id)
  tagList(
    column(6,DT::dataTableOutput(ns('riskTable'))),
    column(6,uiOutput(ns("dataset")),plotOutput(ns('plot')))
  )
}


cdfSubpopPlot <- function(input,output,session, prettyParameterName, parameter, percentileData, parameterRiskTable, CDFsettings){
  output$riskTable <- DT::renderDataTable({
    datatable(parameterRiskTable$Data,
              colnames=parameterRiskTable$ColNames,rownames = F) %>%
      formatStyle('Risk_Category',target='row',backgroundColor=styleEqual(parameterRiskTable$StyleEqual1,parameterRiskTable$StyleEqual2))})
  
  output$dataset <- renderUI({
    selectInput(session$ns("dataset_"),"Select Dataset to Plot",percentileData$Statistic[2:5])
  })
  output$plot <- renderPlot({
    if(is.null(input$dataset_))
      return(NULL)
    cdfplot(prettyParameterName, parameter,input$dataset_,percentileData, CDFsettings)
  })
}
  

statewideMapUI <- function(id){
  ns <- NS(id)
  tagList(
    bootstrapPage(div(class="outer",tags$style(type ="text/css",".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                      leafletOutput(ns('VAmap'),width = '100%', height='100%'),
                      absolutePanel(top=60, right=50, class="panel panel-default",fixed=T,style="z-index:500;",
                                    draggable = T, left = 50,bottom="auto",height="auto",width=900,
                                    p("The statewide map demonstrates water quality parameters as measured at 735 stations across Virginia. Users may display 
                                           basin and ecoregion layers to give additional context to parameter variation. Risk categories for each parameter are consistent 
                                           with breaks and color formatting utilized throughout the app.",span("(draggable)",style="color:red"))),
                      absolutePanel(top=140, right=10,class="panel panel-default",fixed=T,style="z-index:500;",
                                    draggable = T, left = "auto",bottom="auto",height=550,width=325,style = "overflow-x:scroll; max-width: 300px",
                                    h6(span("(draggable)"),style="color:red"),
                                    selectInput(ns("parameterToPlot"),label = 'Choose a water quality parameter to display',
                                                choices=c('','VSCI/VCPMI','pH','Dissolved Oxygen','Total Nitrogen','Total Phosphorus',
                                                          'Total Habitat','LRBS','Metals CCU','Specific Conductivity','Total Dissolved Solids',
                                                          'Dissolved Sulfate','Dissolved Chloride','Dissolved Potassium','Dissolved Sodium')
                                                , selected=''),
                                    plotOutput(ns("Statewidecdf"), height = 200,width=300),
                                    h5(strong("User Input Data")),
                                    h6(span("(Requires user to upload data)"),style="color:red"),
                                    checkboxInput(ns('showUserSite'),'Show User Site',value=F),
                                    DT::dataTableOutput(ns("colors2"))))))
}

statewideMap <- function(input,output,session, summaryStats, stats_wGIS, supaB, eco){
  colOptions <- data.frame(stressLevels=as.factor(c("No Probability of Stress to Aquatic Life","Low Probability of Stress to Aquatic Life","Medium Probability of Stress to Aquatic Life","High Probability of Stress to Aquatic Life")))
  pal <- colorFactor(c("#0072B2","#009E73","#F0E442","firebrick"),levels=colOptions$stressLevels, ordered=T)
  
  # First interpret user selection to enable filtering)
  dataSelect <- reactive({switch(input$parameterToPlot
                                 ,'VSCI/VCPMI'='VSCIfactor','Dissolved Oxygen'='DOfactor'
                                 ,'pH'='pHfactor','Specific Conductivity'='SpCondfactor'
                                 ,'Total Phosphorus'='TPfactor','Total Nitrogen'='TNfactor'
                                 ,'Total Habitat'='TotHabfactor','Total Dissolved Solids'='TDSfactor'
                                 ,'Metals CCU'='MetalCCUfactor','LRBS'='LRBSfactor','Dissolved Sodium'='Nafactor'
                                 ,'Dissolved Potassium'='Kfactor','Dissolved Chloride'='Clfactor','Dissolved Sulfate'='Sffactor')})
  dataSelectcdf <- reactive({switch(input$parameterToPlot
                                    ,'VSCI/VCPMI'='VSCIVCPMI','Dissolved Oxygen'='DO','pH'='pH','Specific Conductivity'='SpCond'
                                    ,'Total Phosphorus'='TP','Total Nitrogen'='TN','Total Habitat'='TotalHabitat'
                                    ,'LRBS'='LRBS','Metals CCU'='MetalsCCU','Total Dissolved Solids'='TDS'
                                    ,'Dissolved Sulfate'='DSulfate','Dissolved Chloride'='DChloride'
                                    ,'Dissolved Potassium'='DPotassium','Dissolved Sodium'='DSodium')})
  filteredData <- reactive({
    df2 <- subset(virginiaStations,ParameterFactor==dataSelect())
  })
  
  output$VAmap <- renderLeaflet({
    leaflet(VAstationselect) %>% 
      setView(-80, 37, zoom = 8)%>%
      addProviderTiles(providers$Esri.NatGeoWorldMap,group='Nat Geo World Map') %>%
      addProviderTiles(providers$Esri.WorldImagery,group='Esri World Imagery') %>%
      addProviderTiles(providers$OpenStreetMap,group='Open Street Map') %>%
      fitBounds(~min(Longitude),~min(Latitude), ~max(Longitude),~max(Latitude)) %>%
      addPolygons(data=supaB,color='navy',fill=0.9,stroke=0.1,group="Basins",
                  popup=paste("Basin: ",supaB$SUPERBASIN,sep="")) %>% hideGroup('Basins') %>%
      addPolygons(data=eco,color='grey',fill=0.9,stroke=0.1,group="Ecoregions",
                  popup=paste("Ecoregion: ",eco$US_L3NAME,sep="")) %>% hideGroup('Ecoregions') %>%
      addLayersControl(baseGroups=c('Nat Geo World Map','Esri World Imagery','Open Street Map'),
                       overlayGroups = c('Basins','Ecoregions'),
                       options=layersControlOptions(collapsed=T),
                       position='topleft')
  })
  
  observe({if(input$showUserSite==TRUE){
    leafletProxy('VAmap') %>%
      addMarkers(data=stats_wGIS[1,],~Longitude,~Latitude,
                 popup=paste(sep= "<br/>",strong('User Input Site'),
                             paste(strong("StationID: "),stats_wGIS$StationID)))}
  })
  
  output$colors2 <- DT::renderDataTable({
    req(summaryStats)
    datatable(summaryStats) %>% formatStyle("pH", backgroundColor = styleInterval(pHRiskTable$brks, pHRiskTable$clrs))%>%
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
  })
  
  # Update map markers when user changes parameter
  observe({
    leafletProxy('VAmap',data=filteredData()) %>% clearMarkers() %>%
      addCircleMarkers(color=~pal(ParameterFactorLevel),fillOpacity=1,stroke=FALSE, radius=5,
                       popup=paste(sep = "<br/>",paste(strong("StationID: "),filteredData()$StationID,sep="")
                                    ,paste(strong("VSCI/VCPMI Score: "),prettyNum(filteredData()$VSCIVCPMI,digits=3),sep="")
                                    ,paste(strong(filteredData()$Parameter[1]),": ",
                                           prettyNum(filteredData()$ParameterMeasure,digits=4)," ",
                                           filteredData()$units,sep="")))})
  
  # Separate observe() to recreate legend as needed
  observe({
    proxy <- leafletProxy('VAmap',data=filteredData())
    proxy %>% clearControls() %>%
      addLegend("bottomright",pal=pal, values=levels(as.factor(filteredData()$ParameterFactorLevel)),title="Benthic TMDL Stressor Thresholds")})
  
  filteredDatacdf <- reactive({
    df <- subset(cdfdata,Indicator==dataSelectcdf() & Subpopulation=="Virginia")})
  
  output$Statewidecdf <- renderPlot({
    req(dataSelectcdf())
    
    print(dataSelectcdf())
    
    
    if(dataSelectcdf()== 'DO'){CDFsettings <- DOsettingsCDF}
    if(dataSelectcdf()=='pH'){CDFsettings <- pHsettingsCDF}
    if(dataSelectcdf()=='SpCond'){CDFsettings <- SpCondsettingsCDF}
    if(dataSelectcdf()=='TDS'){CDFsettings <- TDSsettingsCDF}
    if(dataSelectcdf()=='DSulfate'){CDFsettings <- DSulfatesettingsCDF}
    if(dataSelectcdf()=='DChloride'){CDFsettings <- DChloridesettingsCDF}
    if(dataSelectcdf()=='DPotassium'){CDFsettings <- DPotassiumsettingsCDF}
    if(dataSelectcdf()=='DSodium'){CDFsettings <- DSodiumsettingsCDF}
    if(dataSelectcdf()=='TN'){CDFsettings <- TNsettingsCDF}
    if(dataSelectcdf()=='TP'){CDFsettings <- TPsettingsCDF}
    if(dataSelectcdf()=='TotalHabitat'){CDFsettings <- TotHabsettingsCDF}
    if(dataSelectcdf()=='LRBS'){CDFsettings <- LRBSsettingsCDF}
    if(dataSelectcdf()=='MetalsCCU'){CDFsettings <- MetalsCCUsettingsCDF}
    if(dataSelectcdf()=='VSCIVCPMI'){CDFsettings <- VSCIsettingsCDF}
    
    print(CDFsettings)
    
    ggplot(filteredDatacdf(), aes(x=Value,y=Estimate.P)) + 
      labs(list(title=paste("Statewide",input$parameterToPlot,"\nPercentile Graph")
                ,x=paste(input$parameterToPlot,filteredDatacdf()$units,sep=" ")
                ,y='Statewide Percentile')) +
      CDFsettings  +
      geom_point()+geom_line(aes(y=LCB95Pct.P),colour='gray')+
      geom_line(aes(y=UCB95Pct.P),colour='gray')
    # original plot without color background
    #p <- ggplot(filteredDatacdf(), aes(Value,Estimate.P))+
    #  labs(list(title=paste("Statewide",input$parameterToPlot,"\nPercentile Graph")
    #            ,x=paste(input$parameterToPlot,filteredDatacdf()$units,sep=" ")
    #            ,y='Statewide Percentile'))
    #p+geom_point()+geom_line(aes(y=LCB95Pct.P),colour='gray')+
    #  geom_line(aes(y=UCB95Pct.P),colour='gray')
  })
  
  
}



dissolvedMetalsUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),br(),
    uiOutput(ns('dMetal')),
    checkboxInput(ns('addstd'),strong('Add Acute Criteria to CDF Plot.')),
    uiOutput(ns('dMetalplot_'))
  )
}

dissolvedMetals <- function(input,output,session,inputFile_metals,
                            percentilesDissolvedMetals,percentilesDissolvedMetals2){
  # Choose Dissolved Metal to display
  output$dMetal <- renderUI({
    selectInput(session$ns("dMetal_"),"Select Dissolved Metal to Plot",names(inputFile_metals()[5:23]))
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
