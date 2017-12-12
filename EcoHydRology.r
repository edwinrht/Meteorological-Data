write.csv(vlpd, file="vlpd.csv")

library(EcoHydRology)
vlpd$clud=EstCloudiness(vlpd$t_max,vlpd$t_min,opt="Black")
# First get rain and snow-melt input:
rsm <- SnowMelt(Date=vlpd$date, precip_mm=vlpd$precip, Tmax_C=vlpd$t_max, Tmin_C=vlpd$t_min, lat_deg=10.30707)
# Calculate streamflow based on watershed characteristics:
Results <- Lumped_VSA_model(dateSeries = vlpd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=vlpd$t_max, Tmin=vlpd$t_min, latitudeDegrees=10.30707, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
#  View results:
hydrograph(streamflow=ConvertFlowUnits(cms=Results$modeled_flow, WA=100, AREAunits="mi2"), timeSeries=vlpd$date, streamflow2=Results$modeled_flow, precip=rsm$Rain_mm+rsm$SnowMelt_mm)
#########
EnvirEnergy(10.30707, jday, vlpd$t_max, vlpd$t_min, vlpd$WS, vlpd$precip, (vlpd$rh_830+vlpd$rh_1430)/2, albedo=0.2, cloudiness=vlpd$clud, forest=0,
slope=0, aspect=0,  surftemp=vlpd$t_mean, surfemissivity=0.97)
{if(cloudiness<0){cloudiness<-vlpd$clud}
airtemp<-vlpd$t_mean #average daily air temperature [C]
return(Solar(10.30707,jday,vlpd$t_max,vlpd$t_min,albedo=00.2,forest=0,slope=0,aspect=0)+
Longwave(AtmosphericEmissivity(vlpd$t_mean,vlpd$clud),vlpd$t_mean)-
Longwave(vlpd$AtmEm,vlpd$t_mean),vlpd$t_mean)+SensibleHeat(vlpd$t_mean,vlpd$t_min,vlpd$WS)+
EvapHeat(vlpd$t_mean,vlpd$t_min,(vlpd$rh_830+vlpd$rh_1430)/2,vlpd$t_min,vlpd$WS)+
RainHeat(vlpd$t_min,vlpd$precip)+GroundHeat())}

##CNR
cnrd$clud=EstCloudiness(cnrd$t_max,cnrd$t_min,opt="Black")
cnrd$clud=cnrd$clud*3
rsm <- SnowMelt(Date=cnrd$date, precip_mm=cnrd$precip, Tmax_C=cnrd$t_max, Tmin_C=cnrd$t_min, lat_deg=11.353002)
Results <- Lumped_VSA_model(dateSeries = cnrd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=cnrd$t_max, Tmin=cnrd$t_min, latitudeDegrees=11.353002, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
cnrd$ETo=Results$ET

##GUD
gudd$clud=EstCloudiness(gudd$t_max,gudd$t_min,opt="Black")
gudd$clud=gudd$clud*3
rsm <- SnowMelt(Date=gudd$date, precip_mm=gudd$precip, Tmax_C=gudd$t_max, Tmin_C=gudd$t_min, lat_deg=11.502995)
Results <- Lumped_VSA_model(dateSeries = gudd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=gudd$t_max, Tmin=gudd$t_min, latitudeDegrees=11.502995, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
gudd$ETo=Results$ET

###KOP
kopd$clud=EstCloudiness(kopd$t_max,kopd$t_min,opt="Black")
kopd$clud=kopd$clud*3
rsm <- SnowMelt(Date=kopd$date, precip_mm=kopd$precip, Tmax_C=kopd$t_max, Tmin_C=kopd$t_min, lat_deg=13.528865)
Results <- Lumped_VSA_model(dateSeries = kopd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=kopd$t_max, Tmin=kopd$t_min, latitudeDegrees=13.528865, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
kopd$ETo=Results$ET

###MEP
mepd$clud=EstCloudiness(mepd$t_max,mepd$t_min,opt="Black")
mepd$clud=mepd$clud*3
rsm <- SnowMelt(Date=mepd$date, precip_mm=mepd$precip, Tmax_C=mepd$t_max, Tmin_C=mepd$t_min, lat_deg=11.555)
Results <- Lumped_VSA_model(dateSeries = mepd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=mepd$t_max, Tmin=mepd$t_min, latitudeDegrees=11.555, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
mepd$ETo=Results$ET

##MNR
mnrd$clud=EstCloudiness(mnrd$t_max,mnrd$t_min,opt="Black")
mnrd$clud=mnrd$clud*3
rsm <- SnowMelt(Date=mnrd$date, precip_mm=mnrd$precip, Tmax_C=mnrd$t_max, Tmin_C=mnrd$t_min, lat_deg=10.088933)
Results <- Lumped_VSA_model(dateSeries = mnrd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=mnrd$t_max, Tmin=mnrd$t_min, latitudeDegrees=10.088933, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
mnrd$ETo=Results$ET

##VLP
vlpd$clud=EstCloudiness(vlpd$t_max,vlpd$t_min,opt="Black")
vlpd$clud=vlpd$clud*3
rsm <- SnowMelt(Date=vlpd$date, precip_mm=vlpd$rain, Tmax_C=vlpd$t_max, Tmin_C=vlpd$t_min, lat_deg=10.323591)
Results <- Lumped_VSA_model(dateSeries = vlpd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=vlpd$t_max, Tmin=vlpd$t_min, latitudeDegrees=10.323591, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
vlpd$ETo=Results$ET

##VPR
vprd$clud=EstCloudiness(vprd$t_max,vprd$t_min,opt="Black")
vprd$clud=vprd$clud*3
rsm <- SnowMelt(Date=vprd$date, precip_mm=vprd$precip, Tmax_C=vprd$t_max, Tmin_C=vprd$t_min, lat_deg=9.572112)
Results <- Lumped_VSA_model(dateSeries = vprd$date, P = rsm$Rain_mm+rsm$SnowMelt_mm, Tmax=vprd$t_max, Tmin=vprd$t_min, latitudeDegrees=9.572112, Tp = 5.8, Depth = 2010, SATper = 0.27, AWCper = 0.13, StartCond = "wet")
vprd$ETo=Results$ET
