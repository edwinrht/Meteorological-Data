library(SPEI)
vlpm< - read.table("clipboard", header=TRUE, sep="\t")
vlpm< - ts(vlpm, freq=12,start=c(1981,1))
cnrm$ETo < - penman(cnrm$Tmin,cnrm$Tmax,cnrm$W,tsun=cnrm$S, lat=11.3427, z=1790, na.rm=TRUE, RH=(cnrm$RH830+cnrm$RH830)/2)
gudm$ETo < - penman(gudm$Tmin,gudm$Tmax,gudm$W,tsun=cnrm$S, lat=11.503, z=815, na.rm=TRUE, RH=(gudm$RH830+gudm$RH830)/2)
kopm$ETo < - penman(kopm$Tmin,kopm$Tmax,kopm$W,tsun=kopm$S, lat=13.5289, z=630, na.rm=TRUE, RH=(kopm$RH830+kopm$RH830)/2)
mepm$ETo < - penman(mepm$Tmin,mepm$Tmax,mepm$W,tsun=mepm$S, lat=11.5555, z=850, na.rm=TRUE, RH=(mepm$RH830+mepm$RH830)/2)
mnrm$ETo < - penman(mnrm$Tmin,mnrm$Tmax,mnrm$W,tsun=mnrm$S, lat=10.0889, z=1467, na.rm=TRUE, RH=(mnrm$RH830+mnrm$RH830)/2)
vlpm$ETo < - penman(vlpm$Tmin,vlpm$Tmax,vlpm$W,tsun=vlpm$S, lat=10.2683, z=1064, na.rm=TRUE, RH=(vlpm$RH830+vlpm$RH830)/2)
vprm$ETo < - penman(vprm$Tmin,vprm$Tmax,vprm$W,tsun=vprm$S, lat=9.57211, z=860, na.rm=TRUE, RH=(vprm$RH830+vprm$RH830)/2)

cnrm$clud=(EstCloudiness(cnrm$Tmax,cnrm$Tmin, trans=transmissivity(cnrm$Tmax,cnrm$Tmin), transMin = min(transmissivity(cnrm$Tmax,cnrm$Tmin)), transMax =max(transmissivity(cnrm$Tmax,cnrm$Tmin)), opt = "Black"))*5
gudm$clud=(EstCloudiness(gudm$Tmax,gudm$Tmin, trans=transmissivity(gudm$Tmax,gudm$Tmin), transMin = min(transmissivity(gudm$Tmax,gudm$Tmin)), transMax =max(transmissivity(gudm$Tmax,gudm$Tmin)), opt = "Black"))*5
kopm$clud=(EstCloudiness(kopm$Tmax,kopm$Tmin, trans=transmissivity(kopm$Tmax,kopm$Tmin), transMin = min(transmissivity(kopm$Tmax,kopm$Tmin)), transMax =max(transmissivity(kopm$Tmax,kopm$Tmin)), opt = "Black"))*5
mepm$clud=(EstCloudiness(mepm$Tmax,mepm$Tmin, trans=transmissivity(mepm$Tmax,mepm$Tmin), transMin = min(transmissivity(mepm$Tmax,mepm$Tmin)), transMax =max(transmissivity(mepm$Tmax,mepm$Tmin)), opt = "Black"))*5
mnrm$clud=(EstCloudiness(mnrm$Tmax,mnrm$Tmin, trans=transmissivity(mnrm$Tmax,mnrm$Tmin), transMin = min(transmissivity(mnrm$Tmax,mnrm$Tmin)), transMax =max(transmissivity(mnrm$Tmax,mnrm$Tmin)), opt = "Black"))*5
vlpm$clud=(EstCloudiness(vlpm$Tmax,vlpm$Tmin, trans=transmissivity(vlpm$Tmax,vlpm$Tmin), transMin = min(transmissivity(vlpm$Tmax,vlpm$Tmin)), transMax =max(transmissivity(vlpm$Tmax,vlpm$Tmin)), opt = "Black"))*5
vprm$clud=(EstCloudiness(vprm$Tmax,vprm$Tmin, trans=transmissivity(vprm$Tmax,vprm$Tmin), transMin = min(transmissivity(vprm$Tmax,vprm$Tmin)), transMax =max(transmissivity(vprm$Tmax,vprm$Tmin)), opt = "Black"))*5

par(mfrow=c(3,1))
plot(spei(ts(cnrm$RF-cnrm$ETo, freq=12, start=c(1981,1)),3))
plot(spei(ts(cnrm$RF-cnrm$ETo, freq=12, start=c(1981,1)),6))
plot(spei(ts(cnrm$RF-cnrm$ETo, freq=12, start=c(1981,1)),12))

par(mfrow=c(3,1))
plot(spei(ts(vlpm$precip,freq=12,start=c(1981,1)),3))
plot(spei(ts(vlpm$precip,freq=12,start=c(1981,1)),6))
plot(spei(ts(vlpm$precip,freq=12,start=c(1981,1)),12))

par(mfrow=c(3,1))
plot(spei(ts(vlpm$precip-vlpm$pen,freq=12,start=c(1981,1)),3,kernel=list(type='gaussian',shift=0)))
plot(spei(ts(vlpm$precip-vlpm$pen,freq=12,start=c(1981,1)),6,kernel=list(type='gaussian',shift=0)))
plot(spei(ts(vlpm$precip-vlpm$pen,freq=12,start=c(1981,1)),12,kernel=list(type='gaussian',shift=0)))
