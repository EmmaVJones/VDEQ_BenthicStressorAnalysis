source('global.R')
#source('global_appModules.R')
source('global_appModules_new.R')
source('global_logiManipulationModule.R')
source('global_finalReportModule.R')

shinyUI(fluidPage(theme="yeti.css",
                  navbarPage("VDEQ Benthic Stressor Analysis Tool",
                             tabPanel("About",fluidRow(column(10,
                                                              h4("This app was created to assist in the identification of benthic stressors 
                                                                 to aquatic communities."),
                                                              h5("By uploading field parameters and chemistry data from a given sample site, 
                                                                 users may compare their dataset to 16 years of Virginia Probabilistic Monitoring
                                                                 data by major river basin, stream order, and ecoregion."),
                                                              br(),
                                                              h5(strong("Authors:")),
                                                              h5("Jason Hill, Water Quality Assessment Team Leader"),h5("Chip Sparks, Regional Biologist"),
                                                              h5("Mary Dail, Water Quality Assessment Scientist"),h5("Emma Jones, Water Quality Assessment Data Analyst"),
                                                              h5("Lucy Baker, Regional TMDL Coordinator"),
                                                              br(),
                                                              h5(strong("Background:")),
                                                              p("The DEQ assesses aquatic invertebrate communities in order to evaluate whether or 
                                                                not Virginia’s aquatic life use standard is being met. To meet both state and federal 
                                                                requirements, DEQ assesses water quality data, including chemical and biological data, 
                                                                every two years and subsequently lists those waterbodies not meeting Virginia’s water 
                                                                quality standards on the 303(d) list. The root cause of an aquatic community shift is 
                                                                rarely obvious even though changes in the composition and/or abundance of the benthic 
                                                                macroinvertebrate community are distinct. DEQ utilizes EPA’s recommended method, stressor 
                                                                analysis, to systematically characterize the cause of an aquatic community shift. The goal
                                                                of the stressor analysis process is to apply a weight-of-evidence approach to define a/the 
                                                                most probable stressor(s) that explain(s) the shift in the benthic macroinvertebrate community. 
                                                                Aquatic community stressors encompass a wide array of parameters that have varying degrees of 
                                                                synergistic interactions, further complicating the stressor analysis process. Recognizing these
                                                                challenges, stressor thresholds were developed utilizing ten years of data collected through 
                                                                DEQ’s Freshwater Probabilistic Monitoring Program.  Stressor thresholds are concentration/measured 
                                                                ranges linked to varying levels of stress to aquatic life that present context for stressor 
                                                                analyses reviewers and developers to evaluate water quality datasets and relate them to aquatic 
                                                                community outcomes. Statewide, ecoregion-, basin-, and stream order-specific context is presented 
                                                                in relation to the following common aquatic stressors: dissolved oxygen, pH, total phosphorus, 
                                                                total nitrogen, ionic strength (specific conductivity, TDS, and dissolved sulfate, chloride, 
                                                                sodium, and potassium), dissolved metals cumulative criterion unit, total habitat and relative 
                                                                bed stability.  Specifically, thresholds ranging from no stress to aquatic life to high probability 
                                                                of stress aquatic life were developed and integrated into a web-based application for better 
                                                                understanding stressors."),
                                                              hr(),
                                                              h4(strong("To begin analysis, simply navigate to the 'Upload Data' tab and follow the on screen 
                                                                        instructions.")),
                                                              h4(strong("See the 'How To' tab for additional instructions on pulling data
                                                                        necessary to run the tool and report."))))),
                             tabPanel("How To",
                                      htmlOutput("TMDLBenthicStressorToolHowTo")),
                             tabPanel("Data Organization",
                                      logiManipulationUI('logi')),
                             tabPanel("Data Upload",
                                      column(4,wellPanel(
                                        h4("Instructions:"),
                                        p("Please upload site chemistry and field parameter data as a flat file (.csv). All data uploaded 
                                          to the app must be formatted correctly. If you are unsure whether your data is in the correct 
                                          format, please download the 'template.csv' file first to check your data structure."),
                                        downloadButton('downloadTemplate',"Download template.csv"),
                                        fileInput('siteData','Upload Site (flat file)',accept='.csv',width='100%'))),
                                      column(8,tabsetPanel(
                                        tabPanel("User Data",
                                                 h3("User Uploaded Data"),
                                                 h5("Please fill in the Basin, Ecoregion, and Stream Order fields appropriately."),
                                                 column(3,selectInput("Basin",label=h4("Basin"),
                                                                      c(" "="NA","James Basin"="James Basin",
                                                                        "Roanoke Basin"="Roanoke Basin",
                                                                        "Potomac-Shenandoah"="Potomac-Shenandoah",
                                                                        "Rappahannock-York"="Rappahannock-York",
                                                                        "Chowan Basin"="Chowan",
                                                                        "Tennessee Basin"="Tennessee",
                                                                        "New Basin"="New"),selected=1)),
                                                 column(4,selectInput("Ecoregion",label=h4("Ecoregion"),
                                                                      c(" "="NA","Piedmont"="Piedmont",
                                                                        "Northern Piedmont"="Northern Piedmont",
                                                                        "Central Appalachian Ridges and Valleys"="Central Appalachian Ridges and Valleys",
                                                                        "Southeastern Plains"="Southeastern Plains",
                                                                        "Blue Ridge Mountains"="Blue Ridge Mountains",
                                                                        "Central Appalachians"="Central Appalachians"),selected=1)),
                                                 column(5,selectInput("StreamOrder",label=h4("Stream Order"),
                                                                      c(" "="NA","First Order"="First Order",
                                                                        "Second Order"="Second Order","Third Order"="Third Order",
                                                                        "Fourth Order"="Fourth Order","Fifth Order"="Fifth Order"),selected=1)),
                                                 DT::dataTableOutput('inputTable'),
                                                 hr(),
                                                 h3("Summary Statistics"),
                                                 div(style = 'overflow-x: scroll',tableOutput("summaryStats")))#,
                                        #tabPanel("Metals CCU Analysis, Single Site",
                                        #         metalCCUsingleSiteUI('metalCCUsingleSite')) # REMOVED 10/29/2020 TO AVOID CONFUSION WITH LARRY MCCU CALCULATION
                                                 ))),
                             tabPanel("Data Summary",
                                      column(12,tabsetPanel(
                                        tabPanel("Composite Table",br(),br(),
                                                 h4('Composite Table'),helpText('You can export the table below as a .csv, .xlsx, or .pdf by clicking the corresponding
                                                                                button below. The Copy button copies all table data for you to put into any spreadsheet 
                                                                                program. If you want the color background formatting, you need to manually select the 
                                                                                table with your cursor to copy all associated formatting to a spreadsheet program.'),
                                                 DT::dataTableOutput('colors'),
                                                 column(3,DT::dataTableOutput('riskTableInfo')),br(),br(),
                                                 column(4,
                                                        wellPanel(h4('Report Output:'),
                                                                  helpText('Click below to save a .HTML version of all the tables and graphics associated with 
                                                                           the input station. You can save this to a .pdf after initial HTML conversion 
                                                                           (File -> Print -> Save as PDF).'),
                                                                  downloadButton('report','Generate CDF Report')))),
                                        # pH Summary
                                        tabPanel("pH Summary",br(),br(),h4('pH Summary'),
                                                 parameterSummaryTabUI('pH_Summary'),
                                                 cdfSubpopPlotUI('pH_CDFplot')),
                                        # DO Summary
                                        tabPanel("DO Summary",br(),br(),h4('DO Summary'),
                                                 parameterSummaryTabUI('DO_Summary'),
                                                 cdfSubpopPlotUI('DO_CDFplot')),
                                        # TN Summary
                                        tabPanel("TN Summary",br(),br(),h4('TN Summary'),
                                                 parameterSummaryTabUI('TN_Summary'),
                                                 cdfSubpopPlotUI('TN_CDFplot')),
                                        navbarMenu("More",
                                                   # TP Summary
                                                   tabPanel("TP Summary",br(),br(),h4('TP Summary'),
                                                            parameterSummaryTabUI('TP_Summary'),
                                                            cdfSubpopPlotUI('TP_CDFplot')),
                                                   # Total Habitat Summary
                                                   tabPanel("Total Habitat Summary",br(),br(),h4('Total Habitat Summary'),
                                                            parameterSummaryTabUI('TotHab_Summary'),
                                                            cdfSubpopPlotUI('TotHab_CDFplot')),
                                                   # LRBS Summary
                                                   tabPanel("LRBS Summary",br(),br(),h4('LRBS Summary'),
                                                            parameterSummaryTabUI('LRBS_Summary'),
                                                            cdfSubpopPlotUI('LRBS_CDFplot')),
                                                   # Metals CCU Summary
                                                   tabPanel("Metals CCU Summary",br(),br(),h4('Metals CCU Summary'),
                                                            parameterSummaryTabUI('MetalsCCU_Summary'),
                                                            cdfSubpopPlotUI('MetalsCCU_CDFplot')),
                                                   # Specific Conductivity Summary
                                                   tabPanel("Specific Conductivity Summary",br(),br(),h4('Specific Conductivity Summary'),
                                                            parameterSummaryTabUI('SpCond_Summary'),
                                                            cdfSubpopPlotUI('SpCond_CDFplot')),
                                                   # TDS Summary
                                                   tabPanel("TDS Summary",br(),br(),h4('Total Dissolved Solids Summary'),
                                                            parameterSummaryTabUI('TDS_Summary'),
                                                            cdfSubpopPlotUI('TDS_CDFplot')),
                                                   # Dissolved Sulfate Summary
                                                   tabPanel("Dissolved Sulfate Summary",br(),br(),h4('Dissolved Sulfate Summary'),
                                                            parameterSummaryTabUI('DSulfate_Summary'),
                                                            cdfSubpopPlotUI('DSulfate_CDFplot')),
                                                   # Dissolved Chloride Summary
                                                   tabPanel("Dissolved Chloride Summary",br(),br(),h4('Dissolved Chloride Summary'),
                                                            parameterSummaryTabUI('DChloride_Summary'),
                                                            cdfSubpopPlotUI('DChloride_CDFplot')),
                                                   # Dissolved Potassium Summary
                                                   tabPanel("Dissolved Potassium Summary",br(),br(),h4('Dissolved Potassium Summary'),
                                                            parameterSummaryTabUI('DPotassium_Summary'),
                                                            cdfSubpopPlotUI('DPotassium_CDFplot')),
                                                   # Dissolved Sodium Summary
                                                   tabPanel("Dissolved Sodium Summary",br(),br(),h4('Dissolved Sodium Summary'),
                                                            parameterSummaryTabUI('DSodium_Summary'),
                                                            cdfSubpopPlotUI('DSodium_CDFplot')))))),
                             tabPanel("Statewide Map",
                                      statewideMapUI('statewideMap')),
                             tabPanel("Dissolved Metals",
                                      column(3,wellPanel(
                                        h4("Instructions:"),
                                        p("Please upload dissolved metals data as a flat file (.csv). This section of the app allows
                                          for multiple sites to be uploaded. All data uploaded to the app must be formatted correctly. If you are unsure whether 
                                          your data is in the correct format, please download the 'template_metals.csv' file first to 
                                          check your data structure."),
                                        downloadButton('downloadTemplate_metals',"Download metals template"),
                                        fileInput('siteData_metals','Upload Dissolved Metals (flat file)',accept='.csv',width='100%'))),
                                      column(9,tabsetPanel(
                                        tabPanel("User Data",
                                                 h3("User Uploaded Data"),
                                                 h5("Please review data to ensure all fields are correct."),
                                                 DT::dataTableOutput('inputTable_metals'),
                                                 hr(),
                                                 column(6,h3("Metals CCU Analysis"),
                                                        DT::dataTableOutput("summary_MetalsCCU"),
                                                        helpText('You can copy/paste these values into your spreadsheet for upload
                                                                 back to this app or use it for additional analyses.'))),
                                        tabPanel("Dissolved Metals Data Summary",
                                                 fluidPage(
                                                   fluidRow(column(5,
                                                                   h4("Dissolved Metals Statewide"),
                                                                   uiOutput("metalsSitesUI"),
                                                                   helpText("After uploading data from one or more sites on the previous tab ('User Data'), 
                                                                            you will be able to scroll through sites to analyze dissolved metals against
                                                                            statewide percentiles.")),
                                                            column(5,
                                                                   dissolvedMetalsUI('dMetalsPlots'))),
                                                            # helpText("Criteria are adjusted based on Hardness measure, as required."),
                                                            DT::dataTableOutput('colors_metals'),br(),br()))))),
                             tabPanel('Report',
                                      finalReportUI('report'))
                                        
                             
                  )))
