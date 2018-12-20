# Dummy min/maxDate object so you can build these objects outside the function
minDate <- as.Date('2000-05-01')
maxDate <- as.Date('2020-05-01')

# Build specifics for each plot by parameter
DOsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=10, ymax=Inf, alpha=1, fill="#0072B2") ,
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=8, ymax=10, alpha=1, fill="#009E73") ,
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=7, ymax=8, alpha=1, fill="#F0E442") ,
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=7, alpha=1, fill="firebrick" ) ,
  geom_hline(yintercept = 4, color="black", size=1.5),
  scale_y_continuous(name="Dissolved Oxygen mg/L", breaks=c(4:15), limits=c(4,15)) ,
  labs(x="Sample Month-Year", y="Dissolved Oxygen mg/L"))

pHsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=6, ymax=9, alpha=1, fill="#009E73"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=9, ymax=Inf, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=6, alpha=1, fill="#F0E442" ),
  geom_hline(yintercept = 6, color="black", size=1.5),
  geom_hline(yintercept = 9, color="black", size=1.5),
  scale_y_continuous(name="pH", breaks=seq(5,10,.5), limits=c(5,10)),
  labs(x="Sample Month-Year", y="pH"))

spCondsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=250, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=250, ymax=350, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=350, ymax=500, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=500, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Specific Conductivity", breaks=seq(30, 550,50),limits=c(30,550)),
  labs(x="Sample Month-Year", y="Specific Conductivity"))

TDSsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=250, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=250, ymax=350, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=350, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Dissolved Solids mg/L", breaks=seq(50, 400,50), limits=c(50,400)),
  labs(x="Sample Month-Year", y="Total Dissolved Solids"))

Sulfatesettings <- list(
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=10, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=25, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=25, ymax=75, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=75, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Sulfate mg/L", breaks=seq(0, 80,10), limits=c(0,80)),
  labs(x="Sample Month-Year", y="Dissolved Sulfate mg/L"))

Chloridesettings <- list(
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=10, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=25, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=25, ymax=50, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=50, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Chloride mg/L", breaks=seq(0, 55,5), limits=c(0,55)),
  labs(x="Sample Month-Year", y="Dissolved Chloride mg/L"))

Potassiumsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=1, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=1, ymax=2, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=2, ymax=10, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Potassium mg/L", breaks=seq(0, 12,2),limits=c(0,12)),
  labs(x="Sample Month-Year", y="Dissolved Potassium mg/L"))

Sodiumsettings <- list(
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=7, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=7, ymax=10, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=10, ymax=20, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=20, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Dissolved Sodium mg/L", breaks=seq(0, 55,5), limits=c(0,55)),
  labs(x="Sample Month-Year", y="Dissolved Sodium mg/L"))

TNsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=.5, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.5, ymax=1, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=1, ymax=2, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=2, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Nitrogen mg/L", breaks=seq(0, 5.5,.5), limits=c(0,5.5)),
  labs(x="Sample Month-Year", y="Total Nitrogen mg/L"))

TPsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=.02, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.02, ymax=.05, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.05, ymax=.1, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=.1, ymax=Inf, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Phosphorus mg/L", breaks=seq(0, .5,.03),limits=c(0,0.3)),
  labs(x="Sample Month-Year", y="Total Phosphorus mg/L"))

TotHabsettings <- list(
  annotate("rect", xmin=minDate, xmax=maxDate, ymin=150, ymax=Inf, alpha=1, fill="#0072B2"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=130, ymax=150, alpha=1, fill="#009E73" ),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=100, ymax=130, alpha=1, fill="#F0E442"),
  annotate("rect",xmin=minDate, xmax=maxDate, ymin=-Inf, ymax=100, alpha=1, fill="firebrick" ),
  scale_y_continuous(name="Total Habitat", breaks=seq(0, 200, 25),limits=c(0,200)),
  labs(x="Sample Month-Year", y="Total Habitat"))

Tempsettings <- list(
  geom_hline(yintercept =32, color="black", size=1),
  scale_y_continuous(name="Temperature C", breaks=seq(0,35, 5), limits=c(0,35)),
  labs(x="Sample Month-Year", y="Temperature"))



gMasterPlot <- function(dataset,parameter, datebreaks){
  dat <- select(dataset,StationID,date,prettyDate,parameter) %>% # Subset parameter of interest from whole dataset
    na.omit() %>% # Remove samples without parameter measurements
    dplyr::rename(parameter2=!!names(.[4])) # sneaky rename so ggplot will play nicely in function with changeable parameter variable
  
  # output plot if there is data, if not tell user
  if(nrow(dat)==0){
    return(print("No data for that parameter in the input dataset."))
  }else{
    
    # Find min and max dates based on dataset to use for annotation rectangle limits in ggplot
    minDate <- min(dat$date)-15
    maxDate <- max(dat$date)+15
    
    if(parameter== 'DO'){specialSettings <- DOsettings}
    if(parameter=='pH'){specialSettings <- pHsettings}
    if(parameter=='SpCond'){specialSettings <- spCondsettings}
    if(parameter=='TDS'){specialSettings <- TDSsettings}
    if(parameter=='DSulfate'){specialSettings <- Sulfatesettings}
    if(parameter=='DChloride'){specialSettings <- Chloridesettings}
    if(parameter=='DPotassium'){specialSettings <- Potassiumsettings}
    if(parameter=='DSodium'){specialSettings <- Sodiumsettings}
    if(parameter=='TN'){specialSettings <- TNsettings}
    if(parameter=='TP'){specialSettings <- TPsettings}
    if(parameter=='TotalHabitat'){specialSettings <- TotHabsettings}
    if(parameter=='Temp'){specialSettings <- Tempsettings}
    if(parameter=="Fam.SCI"){specialSettings <-VSCIsettings}
    
    # Now make the plot
    p <- ggplot(dat, aes(x=date, y=parameter2, group=StationID)) + 
      theme(axis.text=element_text(size=14, face="bold"), legend.text=element_text(size=14),legend.title =   element_text(size=14, face="bold"),axis.title=element_text(size=14, face="bold")) +
      scale_x_date(date_breaks = datebreaks, date_labels =  "%b-%y") + 
      coord_cartesian(xlim = as.Date(c(maxDate, minDate))) +
      theme(axis.text.x=element_text(angle=45,hjust=1)) +  
      
      specialSettings + 
      
      geom_point(aes(fill=StationID),colour='black',shape=21, size=6) +
      scale_fill_manual(values=gray.colors(length(unique(dat$StationID))))+  # have different point options depending on number of stations
      guides(fill=guide_legend(title="Station ID"))
    
    return(p)
  }
}