library(seas)
vlpd< - read.table("clipboard", header=TRUE, sep="\t")
vlpd$date < - as.Date(vlpd$date)
year.plot(mksub(vlpd))
attr(vlpd$t_min, "long.name") = "Temperature Minimum"
attr(vlpd$t_min, "units") = "°C"
attr(vlpd$t_max, "long.name") = "Temperature Minimum"
attr(vlpd$t_max, "units") = "°C"
attr(vlpd$t_mean, "long.name") = "Temperature Mean"
attr(vlpd$t_mean, "units") = "°C"
attr(vlpd$rh_830, "long.name") = "Relative Humudity @ 830 h"
attr(vlpd$rh_830, "units") = "%"
attr(vlpd$rh_1430, "long.name") = "Relative Humudity @ 1430 h"
attr(vlpd$rh_1430, "units") = "%"
attr(vlpd$rain, "long.name") = "Precipitation intensity"
attr(vlpd$rain, "units") = "mm"
attr(vlpd$s, "long.name") = "Sunshine"
attr(vlpd$s, "units") = "h"
attr(vlpd$WS, "long.name") = "Wind Speed"
attr(vlpd$WS, "units") = "km/h"
#attr(vlpd$RAD, "long.name") < - "Global Solar Radiation"
#attr(vlpd$RAD, "units") < - "MJ/m2"
#attr(vlpd$PET, "long.name") < - "Potential Evapotranspiration"
#attr(vlpd$PET, "units") < - "mm"
#attr(vlpd$gdd,"long.name") < - "Growing degree days"
vlpd$t_min[vlpd$t_min  == 0 | vlpd$t_min 30] < - NA
vlpd$t_mean[vlpd$t_mean == 0 | vlpd$t_mean 40] < - NA
vlpd$t_max[vlpd$t_max == 0 | vlpd$t_max 40] < - NA
vlpd$rh_830[vlpd$rh_830 == 0 | vlpd$rh_830  <1] < - NA
vlpd$rh_1430[vlpd$rh_1430 == 0 | vlpd$rh_1430  <1] < - NA
vlpd< - mksub(vlpd)
seas.temp.plot(vlpd)
seas.temp.plot(vlpd, wid="mon")
seas.var.plot(vlpd, "t_min", col="azure", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_min", wid="mon", col="azure", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_min", wid="DJF", col="azure", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_max", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_max", wid="mon", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_max", wid="DJF", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_mean", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_mean", wid="mon", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "t_mean", wid="DJF", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
seas.var.plot(vlpd, "rh_830")
seas.var.plot(vlpd, "rh_830", wid="mon")
seas.var.plot(vlpd, "rh_830", wid="DJF")
seas.var.plot(vlpd, "rh_1430")
seas.var.plot(vlpd, "rh_1430", wid="mon")
seas.var.plot(vlpd, "rh_1430", wid="DJF")
seas.var.plot(vlpd, "s")
seas.var.plot(vlpd, "s", wid="mon")
seas.var.plot(vlpd, "s", wid="DJF")
seas.var.plot(vlpd, "WS")
seas.var.plot(vlpd, "WS", wid="mon")
seas.var.plot(vlpd, "WS", wid="DJF")
seas.var.plot(vlpd, "RAD")
seas.var.plot(vlpd, "RAD", wid="mon")
seas.var.plot(vlpd, "RAD", wid="mon")
seas.var.plot(vlpd, "PET")
seas.var.plot(vlpd, "PET", wid="mon")
seas.var.plot(vlpd, "PET", wid="DJF")
seas.var.plot(vlpd, "gdd")
vlpd.ss < - seas.sum(vlpd)
image(vlpd.ss)
image(vlpd.ss,norm="active")
vlpd.dep < - precip.dep(vlpd,precip.norm(vlpd.ss, fun="mean"))
plot(dep ~ date, vlpd.dep, type="l", main="CPD from mean normals")
vlpd.ss < - seas.sum(vlpd, var="PET")
image(vlpd.ss)
vlpd.ss < - seas.sum(vlpd, var="RAD")
image(vlpd.ss)
vlpd.int < - interarrival(mksub(vlpd))
plot(vlpd.int, ylog=FALSE, maxy=31, rep=24)
plot(vlpd.int, width="mon", ylog=FALSE, maxy=31, rep=12)
####read LARS###########
stfile < - system.file("extdata", "summerland.st", package="seas")
print(stfile)
summ < - read.lars(stfile, year.offset=1960)
head(summ)
str(summ)
# plot temperature
summ$t_mean < - rowMeans(summ[, c("t_min", "t_max")])
seas.temp.plot(summ)

# plot solar radiation
seas.var.plot(summ, "solar")

# plot precipitation
summ.ss < - seas.sum(summ)
image(summ.ss)
plot(seas.norm(summ.ss))

####################
library(seas)
seas.temp.plot(dat)
dat$t_min[dat$t_min  == 0 | dat$t_mean 30] < - NA
dat$t_mean[dat$t_mean == 0 | dat$t_mean 40] < - NA
dat$t_max[dat$t_max == 0 | dat$t_max 40] < - NA

plot(density(dat$t_max[is.finite(dat$t_max)])); rug(dat$t_max)
plot(density(dat$t_min[is.finite(dat$t_min)])); rug(dat$t_min)
plot(density(dat$t_mean[is.finite(dat$t_mean)])); rug(dat$t_mean)
plot(density(dat$RF[is.finite(dat$RF)])); rug(dat$RF)
plot(density(dat$s[is.finite(dat$s)])); rug(dat$s)
plot(density(dat$rh_830[is.finite(dat$rh_830)])); rug(dat$rh_830)
plot(density(dat$rh_1430[is.finite(dat$rh_1430)])); rug(dat$rh_1430)

that's a better univariate distribution. Now, back to seas:

seas.temp.plot(dat)

a pretty odd cluster of high temperature anomalies in the later months, but oh well. Other data:

seas.var.plot(dat, "rh_830")
seas.var.plot(dat, "rh_1430")
seas.var.plot(dat, "s")

dat.ss < - seas.sum(dat)
image(dat.ss)
plot(seas.norm(dat.ss))

dat < - read.table("clipboard", header=TRUE, sep="\t")
dat$date < - as.Date(dat$date, format="%d-%m-%Y", origin="01-01-1991")
dat < - mksub(dat)
seas.temp.plot(dat)


##########dry and wet day classification#########
dat.int < - interarrival(mksub(dat))
plot(dat.int, width="mon")
plot(dat.int, ylog=FALSE, maxy=31)

##########Plots a “normal” of a seasonal variable, including a precipitation normal###########
dat < - mksub(dat)
d.ss < - seas.sum(dat)
plot(seas.norm(d.ss))

##########precipitation intensity############

pdat < - dat[dat$precip 0,]
attr(pdat$precip, "long.name") < - "precipitation intensity"
attr(pdat$precip, "units") < - "mm/day"

par(ylog=TRUE)
seas.var.plot(pdat, var="precip", col="azure")

seas.var.plot(dat, var="t_max", col="tomato", add.alt=c(5/9, 32), alt.ylab="F")
abline(h=0)

##############Graphically display a seasonal sum object##########
dat.ss < - seas.sum(dat, width="mon")
image(dat.ss)
image(dat.ss, contour=FALSE)
image(dat.ss, norm="active", start=6, rep=5)

dat2.ss < - seas.sum(dat, start.day=as.Date("1999-01-01"))
image(dat2.ss)
