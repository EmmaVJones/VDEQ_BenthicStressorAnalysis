source('global.R')


metals <- read_csv('oldTemplates/Metals_CCU_Calculator - Sand Branch_Oct 2020.csv')

metals <- mutate(metals, StationID = `Station ID`, 
                 CollectionDateTime = as.Date(`Date Time`, format = '%m/%d/%Y'),
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


calc <- metalsCCUcalcDF(data.frame(metals))%>%
  mutate(StationID=metals$StationID)%>%
  select(StationID,CollectionDateTime,MetalsCCU)
