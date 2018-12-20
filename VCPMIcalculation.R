# This script walks users through calculating the 
#  Virginia Coastal Plain Macroinvertebrate Index

# Bring in test dataset from EDAS genus. Make sure you update
#  the Station Query prior to trying to run each metric query.


# VCPMI MACP+Chowan Basin (Eco63+Chowan)

Richness Score <- IIf([TotTaxa]/21>1,100,100*([TotTaxa]-6.1)/(21-6.1))
RichnessFinal: IIf([Richness Score]<0,0,[Richness Score])
HBIScore: IIf((7.4-[HBI])/(7.4-4.9)*100>100,100,(7.4-[HBI])/(7.4-4.9)*100)
HBIFinal: IIf([HBIScore]<0,0,[HBIScore])
EPT Score: IIf([EPTTax]>0,IIf([EPTTax]/8>1,100,100*([EPTTax]-0)/(8-0)),0)
EPTFinal: IIf([EPT Score]<0,0,[EPT Score])
EPHEM: IIf([%Ephem]>0,IIf([%Ephem]/25.4*100>100,100,([%Ephem]-0)/(25.4-0)*100),0)
PT-H: IIf([%PT - Hydropsychidae]>0,IIf([%PT - Hydropsychidae]/8.2*100>100,100,([%PT - Hydropsychidae]-0)/(8.2-0)*100),0)
Pct5DOM: IIf((100-[%5Dom])/(100-69.2)*100>100,100,(100-[%5Dom])/(100-69.2)*100)
PctClng-HS: IIf([%ClngP-HS]>0,IIf([%ClngP-HS]/18.7*100>100,100,([%ClngP-HS]-0)/(18.7-0)*100),0)
CPMI63+CHOWAN: ([RichnessFinal]+[HBIFinal]+[EPTFinal]+[EPHEM]+[PT-H]+[Pct5DOM]+[PctClng-HS])/7