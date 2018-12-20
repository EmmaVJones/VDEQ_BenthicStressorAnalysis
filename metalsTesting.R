source("global.R")

template_metals <- read.csv('data/template_metals.csv')
inputFile_metals <- template_metals

percentilesDissolvedMetals <- list(
  Calcium = percentileTable_metals(inputFile_metals,'Calcium',"2CXAF000.46"),
  Magnesium = percentileTable_metals(inputFile_metals,'Magnesium',"2CXAF000.46"),
  Arsenic = percentileTable_metals(inputFile_metals,'Arsenic',"2CXAF000.46"),
  Barium = percentileTable_metals(inputFile_metals,'Barium',"2CXAF000.46"),
  Beryllium = percentileTable_metals(inputFile_metals,'Beryllium',"2CXAF000.46"),
  Cadmium = percentileTable_metals(inputFile_metals,'Cadmium',"2CXAF000.46"),
  Chromium = percentileTable_metals(inputFile_metals,'Chromium',"2CXAF000.46"),
  Copper = percentileTable_metals(inputFile_metals,'Copper',"2CXAF000.46"),
  Iron = percentileTable_metals(inputFile_metals,'Iron',"2CXAF000.46"),
  Lead = percentileTable_metals(inputFile_metals,'Lead',"2CXAF000.46"),
  Manganese = percentileTable_metals(inputFile_metals,'Manganese',"2CXAF000.46"),
  Thallium = percentileTable_metals(inputFile_metals,'Thallium',"2CXAF000.46"),
  Nickel = percentileTable_metals(inputFile_metals,'Nickel',"2CXAF000.46"),
  Silver = percentileTable_metals(inputFile_metals,'Silver',"2CXAF000.46"),
  Zinc = percentileTable_metals(inputFile_metals,'Zinc',"2CXAF000.46"),
  Antimony = percentileTable_metals(inputFile_metals,'Antimony',"2CXAF000.46"),
  Aluminum = percentileTable_metals(inputFile_metals,'Aluminum',"2CXAF000.46"),
  Selenium = percentileTable_metals(inputFile_metals,'Selenium',"2CXAF000.46"),
  Hardness = percentileTable_metals(inputFile_metals,'Hardness',"2CXAF000.46"),
  final = rbind(percentilesDissolvedMetals$Calcium,percentilesDissolvedMetals$Magnesium,
              percentilesDissolvedMetals$Arsenic,percentilesDissolvedMetals$Barium,
              percentilesDissolvedMetals$Beryllium,percentilesDissolvedMetals$Cadmium,
              percentilesDissolvedMetals$Chromium,percentilesDissolvedMetals$Copper,percentilesDissolvedMetals$Iron,
              percentilesDissolvedMetals$Lead,percentilesDissolvedMetals$Manganese,
              percentilesDissolvedMetals$Thallium,percentilesDissolvedMetals$Nickel,
              percentilesDissolvedMetals$Silver,percentilesDissolvedMetals$Zinc,
              percentilesDissolvedMetals$Antimony,percentilesDissolvedMetals$Aluminum,
              percentilesDissolvedMetals$Selenium,percentilesDissolvedMetals$Hardness))




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
                         
                     
 
#percentilesDissolvedMetals2 <- list(
  H <- as.numeric(percentilesDissolvedMetals$final[19,2])
  criteria <- metalsCriteriaAssessment(H)
  criteria[,2:4] <- suppressWarnings(apply(criteria[,2:4], 2, function(x) as.numeric(as.character(x))))
  percentilesDissolvedMetals2 <- suppressWarnings(left_join(percentilesDissolvedMetals$final,criteria,by='Dissolved_Metal')%>%
    mutate(ChronicAssessment=ifelse(Measure>Chronic,'Not Supporting','Fully Supporting'),
           AcuteAssessment=ifelse(Measure>Acute,'Not Supporting','Fully Supporting'),
           PWSAssessment=ifelse(Measure>PWS,'Not Supporting','Fully Supporting')))
  
  criteria1 <- metalsCriteria1(H)
  criteria1$V1 <- as.numeric(as.character(criteria$V1))
  percentilesDissolvedMetals2 <- cbind(percentilesDissolvedMetals$final,criteria)%>%
    mutate(ChronicAssessment=ifelse(Measure>V1,'Not Supporting','Fully Supporting'))
  names(percentilesDissolvedMetals2)[4] <- paste('Dissolved Metal Chronic Criteria (Hardness=',H,')',sep='')


  DT::datatable(percentilesDissolvedMetals2,extensions = 'Buttons', escape=F, rownames = F,
                colnames = c('Dissolved Metal','Measure','Statewide Percentile','Acute Criteria',
                             'Chronic Criteria',"PWS Criteria",'Chronic Assessment','Acute Assessment','PWS Assessment'),
                options=list(scrollX = TRUE,scrollY = "600px",pageLength=20,
                             dom='Bt'))%>%
    formatStyle('ChronicAssessment',backgroundColor=styleEqual(c('Fully Supporting','Not Supporting'),c('white','red'))) %>%
    formatStyle('AcuteAssessment',backgroundColor=styleEqual(c('Fully Supporting','Not Supporting'),c('white','red'))) %>%
    formatStyle('PWSAssessment',backgroundColor=styleEqual(c('Fully Supporting','Not Supporting'),c('white','red')))
    
  