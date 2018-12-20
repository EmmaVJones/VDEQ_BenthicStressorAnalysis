library(shiny)
library(leaflet)
library(mapview)
library(tidyverse)
library(rgdal)
library(DT)
library(reshape2)
library(taRifx)
library(knitr)
library(readxl)

# Local data locations
VAstationselect <- readRDS('data/VAstationselect_May2018.RDS')
virginiaStations <- readRDS('data/VirginiaStations_May2018.RDS')
cdfdata <- readRDS('data/cdfData_May2018.RDS') ### UPDATED CDF DATA AFTER 2018IR REPORT
template <- read.csv('data/templateGIS.csv')
template_metals <- read.csv('data/template_metals.csv')
#metalsCDF <- readRDS('data/metalsCDF_March2017Update.RDS')
bugTemplate <- read.csv('data/bugTemplate.csv')
bugTemplateVCPMI63 <- read.csv('data/bugTemplateVCPMI63.csv')
bugTemplateVCPMI65 <- read.csv('data/bugTemplateVCPMI65.csv')
habitatTemplate <- read.csv('data/habitatTemplate.csv')

# Bring in custom plotting functions for report
source('global_stressorPlotFunctions.R')
#Bring in VLOOKUP-like function written in R
source('vlookup.R')


# Risk table
riskHightoLow <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
risk <- data.frame(Risk_Category=c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life'))
brksrisk <- c('High Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','No Probability of Stress to Aquatic Life')
clrsrisk <- c("firebrick","#F0E442","#009E73","#0072B2")

# Each parameter risk table and colors
pHRiskTable <- list(
  Data = data.frame(Risk_Category=c('Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life','Medium Probability of Stress to Aquatic Life'),pH=c("< 6","6 - 9","> 9")),
  ColNames = c('Risk Category','pH (unitless)'),
  StyleEqual1 = c('Medium Probability of Stress to Aquatic Life','Low Probability of Stress to Aquatic Life'),
  StyleEqual2 = c('#F0E442','#009E73'),
  brks = c(0,6,9),
  clrs = c("gray","#F0E442","#009E73","#F0E442")
)

DORiskTable <- list(
  Data = cbind(risk,DO=c('< 7','> 7, < 8','> 8, < 10','> 10')),
  ColNames = c('Risk Category','DO (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,7,8,10),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2")
)

TNRiskTable <- list(
  Data = cbind(risk,TN=c('> 2','> 1, < 2','> 0.5, < 1','< 0.5')),
  ColNames = c('Risk Category','Total Nitrogen (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.5,1,2),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TPRiskTable <- list(
  Data = cbind(risk,TP=c('> 0.1','> 0.05, < 0.1','> 0.02, < 0.05','< 0.02')),
  ColNames = c('Risk Category','Total Phosphorus (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.02,0.05,0.1),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TotHabRiskTable <- list(
  Data = cbind(risk,TotalHabitat=c('< 100','> 100, < 130','> 130, < 150','> 150')),
  ColNames = c('Risk Category','Total Habitat (unitless)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,100,130,150),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2")
)

LRBSRiskTable <- list(
  Data = cbind(rbind(risk,'Medium Probability of Stress to Aquatic Life'),LRBS=c('< -1.5','> -1.5, < -1.0','> -0.5, < -1.0','> -0.5, < 0.5','> 0.5')),
  ColNames = c('Risk Category','Relative Bed Stability (unitless)'),
  StyleEqual1 = c(riskHightoLow,'Medium Probability of Stress to Aquatic Life'),
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2","#F0E442"),
  brks = c(-3,-1.5,-1,-0.5,0.5),
  clrs = c("gray","firebrick","#F0E442","#009E73","#0072B2","#F0E442")
)

MetalsCCURiskTable <- list(
  Data = cbind(risk,MetalsCCU=c('> 2.0','> 1.5, < 2.0','> 0.75, < 1.5','< 0.75')),
  ColNames = c('Risk Category','Metals CCU (unitless)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,0.75,1.5,2.0),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

SpCondRiskTable <- list(
  Data = cbind(risk,SpCond=c('> 500','> 350, < 500','> 250, < 350','< 250')),
  ColNames = c('Risk Category','Specific Conductivity (uS/cm)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,250,350,500),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

TDSRiskTable <- list(
  Data = cbind(risk,TDS=c('> 350','> 250, < 350','> 100, < 250','< 100')),
  ColNames = c('Risk Category','Total Dissolved Solids (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,100,250,350),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DSulfateRiskTable <- list(
  Data = cbind(risk,DSulfate=c('> 75','> 25, < 75','> 10, < 25','< 10')),
  ColNames = c('Risk Category','Dissolved Sulfate (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,10,25,75),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DChlorideRiskTable <- list(
  Data = cbind(risk,DChloride=c('> 50','> 25, < 50','> 10, < 25','< 10')),
  ColNames = c('Risk Category','Dissolved Chloride (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,10,25,50),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DPotassiumRiskTable <- list(
  Data = cbind(risk,DPotassium=c('> 10','> 2, < 10','> 1, < 2','< 1')),
  ColNames = c('Risk Category','Dissolved Potassium (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks = c(0,1,2,10),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)

DSodiumRiskTable <- list(
  Data = cbind(risk,DSodium=c('> 20','> 10, < 20','> 7, < 10','< 7')),
  ColNames = c('Risk Category','Dissolved Sodium (mg/L)'),
  StyleEqual1 = riskHightoLow,
  StyleEqual2 = c("firebrick","#F0E442","#009E73","#0072B2"),
  brks =  c(0,7,10,20),
  clrs = c("gray","#0072B2","#009E73","#F0E442","firebrick")
)


DOsettingsCDF <- list(
  annotate("rect", xmin=10, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2") ,
  annotate("rect",xmin=8, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73") ,
  annotate("rect",xmin=7, xmax=8, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442") ,
  annotate("rect",xmin=-Inf, xmax=7, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

pHsettingsCDF <- list(
  annotate("rect", xmin=6, xmax=9, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73"),
  annotate("rect",xmin=9, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=-Inf, xmax=6, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442" ))

SpCondsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=250, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=250, xmax=350, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=350, xmax=500, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=500, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TDSsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=100, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=100, xmax=250, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=250, xmax=350, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=350, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DSulfatesettingsCDF <- list(
  annotate("rect",xmin=-Inf, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=10, xmax=25, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=25, xmax=75, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=75, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DChloridesettingsCDF <- list(
  annotate("rect",xmin=-Inf, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=10, xmax=25, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=25, xmax=50, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=50, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DPotassiumsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=1, xmax=2, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=2, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=10, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

DSodiumsettingsCDF <- list(
  annotate("rect",xmin=-Inf, xmax=7, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=7, xmax=10, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=10, xmax=20, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=20, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TNsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=0.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=0.5, xmax=1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=1, xmax=2, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=2, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TPsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=.02, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=.02, xmax=.05, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=.05, xmax=.1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=.1, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

TotHabsettingsCDF <- list(
  annotate("rect", xmin=150, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=130, xmax=150, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=100, xmax=130, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=-Inf, xmax=100, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

LRBSsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=-1.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick"),
  annotate("rect",xmin=-1.5, xmax=-1, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=-1, xmax=-0.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73"),
  annotate("rect",xmin=-0.5, xmax=0.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=0.5, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"))

MetalsCCUsettingsCDF <- list(
  annotate("rect", xmin=-Inf, xmax=0.75, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#0072B2"),
  annotate("rect",xmin=0.75, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#009E73" ),
  annotate("rect",xmin=1.5, xmax=2.0, ymin=-Inf, ymax=Inf, alpha=0.5, fill="#F0E442"),
  annotate("rect",xmin=2.0, xmax=Inf, ymin=-Inf, ymax=Inf, alpha=0.5, fill="firebrick" ))

VSCIsettingsCDF <- list(
  geom_vline(xintercept = 60, color="red", size=1))


# Return percentile 
percentileTable <- function(statsTable,parameter,userBasin,userEco,userOrder,stationName){
  out <- statsTable%>%select_("Statistic",parameter)%>% spread_("Statistic",parameter)%>%
    mutate(Statistic=stationName)%>%select(Statistic,everything())
  va <- filter(cdfdata,Subpopulation=='Virginia',Indicator==parameter)%>%select(Value,Estimate.P)
  basin <- filter(cdfdata,Subpopulation==userBasin,Indicator==parameter)%>%select(Value,Estimate.P)
  eco <- filter(cdfdata,Subpopulation==userEco,Indicator==parameter)%>%select(Value,Estimate.P)
  order <- filter(cdfdata,Subpopulation==userOrder,Indicator==parameter)%>%select(Value,Estimate.P)
  va2 <- data.frame(Statistic='Virginia',Average=vlookup(out$Average,va,2,range=TRUE),Median=vlookup(out$Median,va,2,range=TRUE))
  basin2 <- data.frame(Statistic=userBasin,Average=vlookup(out$Average,basin,2,TRUE),Median=vlookup(out$Median,basin,2,TRUE))
  eco2 <- data.frame(Statistic=userEco,Average=vlookup(out$Average,eco,2,TRUE),Median=vlookup(out$Median,eco,2,TRUE))
  order2 <- data.frame(Statistic=userOrder,Average=vlookup(out$Average,order,2,TRUE),Median=vlookup(out$Median,order,2,TRUE))
  out_final <- rbind(out,va2,basin2,eco2,order2)
  return(out_final)
}




percentileTable_metals <- function(measurementTable,parameter,stationName){
  sub <- select_(measurementTable,"StationID",parameter)%>%filter(StationID%in%as.character(stationName))
  parametercap <- toupper(parameter)
  unit <- filter(cdfdata,Subpopulation=="Virginia",Indicator%in%parametercap)
  va <- filter(cdfdata,Subpopulation=="Virginia",Indicator%in%parametercap)%>%select(Value,Estimate.P)
  final <- data.frame(Dissolved_Metal=paste(parameter," (",unit$units[1],")",sep=""),Measure=sub[1,2],
                      Statewide_Percentile=formatC(vlookup(sub[1,2],va,2,TRUE),digits=3))
  return(final)
}


# CDF plot function
cdfplot <- function(prettyParameterName,parameter,indicator,dataset,CDFsettings){
  cdfsubset <- subFunction(cdfdata,parameter,indicator)
  avg1 <- as.numeric(filter(dataset,Statistic==indicator)[,2])
  avg <- subFunction2(cdfsubset,avg1)
  med1 <- as.numeric(filter(dataset,Statistic==indicator)[,3]) 
  med <- subFunction2(cdfsubset,med1)
  m <- max(cdfsubset$NResp)
  p1 <- ggplot(cdfsubset, aes(x=Value,y=Estimate.P)) + 
    labs(x=paste(prettyParameterName,unique(cdfsubset$units),sep=" "),y="Percentile") +
    ggtitle(paste(indicator,prettyParameterName,"Percentile Graph ( n=",m,")",sep=" ")) + 
    theme(plot.title = element_text(hjust=0.5,face='bold',size=15)) +
    theme(axis.title = element_text(face='bold',size=12))+
    
    CDFsettings  +
    
    geom_point() +
    geom_point(data=avg,color='orange',size=4) + geom_text(data=avg,label='Average',hjust=1.2) +
    geom_point(data=med,color='gray',size=4)+ geom_text(data=med,label='Median',hjust=1.2) 
  return(p1)
}



subFunction <- function(cdftable,parameter,userInput){
  return(filter(cdftable,Subpopulation%in%userInput & Indicator%in%parameter))
}

subFunction2 <- function(cdftable,userValue){
  return(filter(cdftable,Estimate.P%in%userValue))
}

# Metals Criterion Table
metalsCriteria <- function(Hardness){
  criteriaHardness <- ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness))
  x <- data.frame(Calcium=NA,Magnesium=NA,Arsenic=150,Barium=NA,Beryllium=NA,
                  Cadmium=format((exp(0.7852*(log(criteriaHardness))-3.49)),digits=3), 
                  Chromium=format(((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),digits=3),
                  Copper=format(((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),digits=3),
                  Iron=1000,Lead=format(((exp(1.273*(log(criteriaHardness))-3.259))),digits=3),
                  Manganese=50,Thallium=0.24,Nickel=format(((exp(0.846*(log(criteriaHardness))-0.884))*0.997),digits=3),
                  Silver=format(((exp(1.72*(log(criteriaHardness))-6.52))*0.85),digits=3),
                  Zinc=format(((exp(0.8473*(log(criteriaHardness))+0.884))*0.986),digits=3),
                  Antimony=5.6,Aluminum=150,Selenium=5,Hardness=NA)
  return(as.data.frame(t(x)))}


# Metals CCU Calculation
metalsCCUcalc <- function(Hardness,Aluminum,Arsenic,Cadmium,Chromium,Copper,Lead,Nickel,Selenium,Zinc){
  criteriaHardness <- ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness))
  AluminumEPAChronic <- Aluminum/150
  ArsenicChronic <- Arsenic/150
  CadmiumChronic <- Cadmium/(exp(0.7852*(log(criteriaHardness))-3.49))
  ChromiumChronic <- Chromium/((exp(0.819*(log(criteriaHardness))+0.6848))*0.86)
  CopperChronic <- Copper/((exp(0.8545*(log(criteriaHardness))-1.702))*0.96)
  LeadChronic <- Lead/((exp(1.273*(log(criteriaHardness))-3.259)))
  NickelChronic <- Nickel/((exp(0.846*(log(criteriaHardness))-0.884))*0.997)
  SeleniumChronic <- Selenium/5
  ZincChronic <- Zinc/((exp(0.8473*(log(criteriaHardness))+0.884))*0.986)
  return(sum(AluminumEPAChronic,ArsenicChronic,CadmiumChronic,ChromiumChronic,CopperChronic,LeadChronic,
             NickelChronic,SeleniumChronic,ZincChronic))
}

# Metals CCU Calculation for dataframes
metalsCCUcalcDF <- function(df){
  CollectionDateTime <- df$CollectionDateTime
  Hardness <- df$Hardness
  Aluminum <- df$Aluminum
  Arsenic <- df$Arsenic
  Cadmium <- df$Cadmium
  Chromium <- df$Chromium
  Copper <- df$Copper
  Lead <- df$Lead
  Nickel <- df$Nickel 
  Selenium <- df$Selenium
  Zinc <- df$Zinc
  met <- data.frame(CollectionDateTime=NA,MetalsCCU=NA)
  for(i in 1:nrow(df)){
    met[i,] <- cbind(as.character(CollectionDateTime[i]),format(metalsCCUcalc(df$Hardness[i],df$Aluminum[i],df$Arsenic[i],df$Cadmium[i],
                                                                df$Chromium[i],df$Copper[i],df$Lead[i],df$Nickel[i],
                                                                df$Selenium[i],df$Zinc[i]),digits=4))
  }
  return(met)
}


# Assessment Dissolved Metals Table
metalsCriteriaAssessment <- function(Hardness){
  criteriaHardness <- ifelse(Hardness<25,25,ifelse(Hardness>400,400,Hardness))
  x <- data.frame(Calcium_Chronic=NA,Calcium_Acute=NA,Calcium_PWS=NA,
                  Magnesium_Chronic=NA, Magnesium_Acute=NA, Magnesium_PWS=NA,
                  Arsenic_Chronic=150,Arsenic_Acute=340,Arsenic_PWS=10,
                  Barium_Chronic=NA,Barium_Acute=NA,Barium_PWS=2000,
                  Beryllium_Chronic=NA,Beryllium_Acute=NA,Beryllium_PWS=NA,
                  Cadmium_Chronic=format((exp(0.7852*(log(criteriaHardness))-3.49)),digits=3), 
                  Cadmium_Acute= format((exp(1.128*(log(criteriaHardness))-3.828)),digits=3), Cadmium_PWS=5,
                  Chromium_Chronic=format(((exp(0.819*(log(criteriaHardness))+0.6848))*0.86),digits=3),
                  Chromium_Acute=format(((exp(0.819*(log(criteriaHardness))+3.7256))*0.316),digits=3), Chromium_PWS=100,
                  Copper_Chronic=format(((exp(0.8545*(log(criteriaHardness))-1.702))*0.96),digits=3),
                  Copper_Acute=format(((exp(0.9422*(log(criteriaHardness))-1.70))*0.96),digits=3), Copper_PWS=1300,
                  Iron_Chronic=NA, Iron_Acute=NA, Iron_PWS= 300,
                  Lead_Chronic=format(((exp(1.273*(log(criteriaHardness))-3.259))),digits=3),
                  Lead_Acute=format(((exp(1.273*(log(criteriaHardness))-1.084))),digits=3), Lead_PWS=15,
                  Manganese_Chronic=NA, Manganese_Acute=NA, Manganese_PWS=50,
                  Thallium_Chronic=NA, Thallium_Acute=NA, Thallium_PWS=0.24,
                  Nickel_Chronic=format(((exp(0.846*(log(criteriaHardness))-0.884))*0.997),digits=3),
                  Nickel_Acute=format(((exp(0.846*(log(criteriaHardness))+1.312))*0.998),digits=3),Nickel_PWS=610,
                  Silver_Chronic=NA, Silver_Acute=format(((exp(1.72*(log(criteriaHardness))-6.52))*0.85),digits=3), Silver_PWS=NA,
                  Zinc_Chronic=format(((exp(0.8473*(log(criteriaHardness))+0.884))*0.986),digits=3),
                  Zinc_Acute=format(((exp(0.8473*(log(criteriaHardness))+0.884))*0.978),digits=3), Zinc_PWS=7400,
                  Antimony_Chronic=NA, Antimony_Acute=NA, Antimony_PWS=5.6,
                  Aluminum_Chronic=NA,Aluminum_Acute=NA,Aluminum_PWS=NA,
                  Selenium_Chronic=5, Selenium_Acute= 20, Selenium_PWS=170,
                  Hardness_Chronic=NA,Hardness_Acute=NA,Hardness_PWS=NA)
  x1 <- as.data.frame(t(x)) %>%rownames_to_column('original')
  return(suppressWarnings(x1 %>%rowwise()%>%  
                            mutate(Dissolved_Metal=gsub("_.*","\\1",original),Criteria=gsub(".*_","\\1",original)) %>%
                            group_by(Criteria) %>%
                            mutate(id = row_number()) %>%
                            select(-original) %>%
                            spread(Criteria, V1) %>%
                            select(-id) %>%
                            mutate(Dissolved_Metal=dplyr::recode(Dissolved_Metal,Calcium="Calcium (mg/L)",Magnesium ='Magnesium (mg/L)',
                                                                 Arsenic ='Arsenic (ug/L)',Barium ='Barium (ug/L)',Barium ='Barium (ug/L)',
                                                                 Beryllium ='Beryllium (ug/L)',Cadmium ='Cadmium (ug/L)',Chromium ='Chromium (ug/L)',
                                                                 Copper='Copper (ug/L)', Iron = 'Iron (ug/L)',Lead ='Lead (ug/L)',Manganese ='Manganese (ug/L)',
                                                                 Thallium ='Thallium (ug/L)',Nickel ='Nickel (ug/L)',Silver ='Silver (ug/L)',Zinc ='Zinc (ug/L)',
                                                                 Antimony ='Antimony (ug/L)', Aluminum ='Aluminum (ug/L)',Selenium ='Selenium (ug/L)',Hardness ='Hardness (mg/L)'))))
}


addUnits_envDataDF <- function(envData){
  return(rename(envData, `pH (unitless)`= pH, `DO (mg/L)` = DO, `TN (mg/L)` = TN, 
                `TP (mg/L)` = TP, `Total Habitat (unitless)`= TotalHabitat,
                `LRBS (unitless)`= LRBS, `MetalsCCU (unitless)`= MetalsCCU,
                `SpCond (uS/cm)` = SpCond,  `TDS (mg/L)` = TDS,  
                `DSulfate (mg/L)` = DSulfate, `DChloride (mg/L)` = DChloride, 
                `DPotassium (mg/L)` = DPotassium, `DSodium (mg/L)` = DSodium,
                `Temperature (C)` = Temp))
}

removeUnits_envDataDF <- function(envData){
  return(rename(envData,  pH = "pH..unitless.", DO = "DO..mg.L." , TN =  "TN..mg.L.", 
                TP = "TP..mg.L.", TotalHabitat = "Total.Habitat..unitless.",
                LRBS =  "LRBS..unitless." , MetalsCCU = "MetalsCCU..unitless.",
                SpCond = "SpCond..uS.cm.",  TDS = "TDS..mg.L." ,  
                DSulfate = "DSulfate..mg.L.", DChloride= "DChloride..mg.L.", 
                DPotassium = "DPotassium..mg.L.", DSodium = "DSodium..mg.L.",
                Temp = "Temperature..C."))
}