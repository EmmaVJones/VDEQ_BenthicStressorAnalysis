# to get these I queried 2-CNE000.96 from 1-1-2015:1-1-2017 in personal logi queries, using copy of each bc
# I deleted out header matter from those queries

field <- read_excel('testData/7-PAR003.09/Logi-EDASraw/201516_7-PAR003.09field_copy.xls') %>%
  mutate(StationID=unique(StationID)[1], # Fill StationID with value
         DO = ifelse(is.na(`DO Optical`),`DO Probe`,`DO Optical`),
         Temp = `Temp Celsius`) %>% # combine DO Optical and DO Probe fields
  select(-c(`DO Optical`,`DO Probe`,`Temp Celsius`)) # Now remove those columns bc no longer needed
field <- field[complete.cases(field[ , 3:4]),] # remove rows without real data
field$CollectionDateTime <- as.Date(field$CollectionDateTime) # change to just date format bc field query will not bring in times to match benstress pull


benstress <- read_excel('testData/7-PAR003.09/Logi-EDASraw/201516_7-PAR003.09fbenstress_copy.xls') %>%
  mutate(`Parm Name` = recode(`Parm Name`, 
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


# Bring in RBP
totHab <- read_excel('testData/2-CNE000.96/Logi-EDASraw/Habitat parameters Query LB_2-CNE000.96.xlsx') %>%
  dplyr::distinct(StationID,CollDate,TotHabSc) %>%
  mutate(CollectionDateTime = CollDate, TotalHabitat = TotHabSc,
         Latitude = unique(benstress$Latitude), Longitude = unique(benstress$Longitude)[1]) %>%
  select(StationID,CollectionDateTime,Longitude, Latitude, TotalHabitat )
totHab$CollectionDateTime <- as.Date(totHab$CollectionDateTime)

# Old method
#totHab <- read_excel('testData/Total HabValues Score_2-CNE000.96.xlsx') %>%
#  mutate(CollectionDateTime = CollDate, TotalHabitat = TotHabSc,
#         Latitude = unique(benstress$Latitude), Longitude = unique(benstress$Longitude)[1]) %>%
#  select(StationID,CollectionDateTime,Longitude, Latitude, TotalHabitat )
#totHab$CollectionDateTime <- as.Date(totHab$CollectionDateTime)


# combine field and benstress 
allTogether <- full_join(field,benstress,by=c('StationID','CollectionDateTime','Longitude','Latitude')) %>%
  full_join(totHab, by = c('StationID','CollectionDateTime','Longitude','Latitude')) %>%
  mutate(LRBS = NA, MetalsCCU = NA) %>% # add spot for parameters to be entered by hand
  select(StationID, CollectionDateTime, Longitude, Latitude, pH, DO, TN, TP, TotalHabitat, LRBS, MetalsCCU, 
         SpCond, TDS, DSulfate, DChloride, DPotassium, DSodium, Temp) %>% # rearrange data according to template
  arrange(CollectionDateTime) # sort oldest to newest

                    







### Metals
metals <- read_excel('multipleStationExampleData/2-CAT026.29/Logi-EDASraw/201516_2-CAT026.29metals_copy.xls') %>%
#metals <- read_excel('testData/201517_2-CNE000.96metals.xls') %>%
  mutate(StationID = `Station ID`, CollectionDateTime = `Date Time`,
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
  select(StationID, CollectionDateTime, Longitude, Latitude, `Parm Name`, Value)
metals <- metals[complete.cases(metals[, 3:4]),] # get rid of extra empty rows 
metals <- spread(metals,"Parm Name","Value") # go from Long to Wide format
metals$CollectionDateTime <- as.Date(metals$CollectionDateTime) # change to just date format bc field query will not bring in times to match benstress pull



calc <- metalsCCUcalcDF(data.frame(metals))%>%
  mutate(StationID=metals$StationID)%>%
  select(StationID,MetalsCCU)
