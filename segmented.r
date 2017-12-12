library(segmented)
library(xts)
enso=read.table("clipboard",T)
x<-1:365
x<-xts(x,as.Date(0:364,origin="2017-01-01"))

Clod=enso$Clod
out=glm(Clod~x)
cold<-segmented.lm(out,seg.Z=~x,psi=list(x=c(30,60)),control=seg.control(stop.if.error=TRUE,n.boot=100, it.max=20))
out=glm(Clod~x,family=quasipoisson)
cold1<-segmented(out,seg.Z=~x,psi=list(x=c(30,60)),control=seg.control(stop.if.error=TRUE,n.boot=100, it.max=20))

Warm=enso$Warm
out=glm(Warm~x)
warm<-segmented.lm(out,seg.Z=~x,psi=list(x=c(30,60)),control=seg.control(stop.if.error=TRUE,n.boot=100, it.max=20))
out=glm(Warm~x,family=quasipoisson)
warm1<-segmented(out,seg.Z=~x,psi=list(x=c(30,60)),control=seg.control(stop.if.error=TRUE,n.boot=100, it.max=20))

Neutral=enso$Neutral
out=glm(Neutral~x)
neut<-segmented.lm(out,seg.Z=~x,psi=list(x=c(30,60)),control=seg.control(stop.if.error=TRUE,n.boot=100, it.max=20))
out=glm(Neutral~x,family=quasipoisson)
neut1<-segmented(out,seg.Z=~x,psi=list(x=c(30,60)),control=seg.control(stop.if.error=TRUE,n.boot=100, it.max=20))

pdf("Monsoon.pdf", paper="a4r",useKerning=TRUE,width = 0, height = 0)
par(mfrow=c(3,2))
plot(cold, conf.level=0.95, shade=TRUE,xlab="Julian day",ylab="Rainfall (mm)",main="Cold ENSO-Phase")
plot(cold,add=TRUE,link=TRUE,lwd=2,col=2:3, lty=c(1,3))
lines(cold,col=4,pch=25,bottom=FALSE,lwd=1)
points(cold,col=6)
plot(cold1, conf.level=0.95, shade=TRUE,xlab="Julian day",ylab="Rainfall (mm)",main="Cold ENSO-Phase")
plot(cold1,add=TRUE,link=TRUE,lwd=2,col=2:3, lty=c(1,3))
lines(cold1,col=4,pch=25,bottom=FALSE,lwd=1)
points(cold1,col=6)

plot(warm, conf.level=0.99, shade=TRUE,xlab="Julian day",ylab="Rainfall (mm)",main="Warm ENSO-Phase")
plot(warm,add=TRUE,link=TRUE,lwd=2,col=2:3, lty=c(1,3))
lines(warm,col=4,pch=25,bottom=FALSE,lwd=1)
points(warm,col=6)
plot(warm1, conf.level=0.99, shade=TRUE,xlab="Julian day",ylab="Rainfall (mm)",main="Warm ENSO-Phase")
plot(warm1,add=TRUE,link=TRUE,lwd=2,col=2:3, lty=c(1,3))
lines(warm1,col=4,pch=25,bottom=FALSE,lwd=1)
points(warm1,col=6)

plot(neut, conf.level=0.95, shade=TRUE,xlab="Julian day",ylab="Rainfall (mm)",main="Neutral ENSO-Phase")
plot(neut,add=TRUE,link=TRUE,lwd=2,col=2:3, lty=c(1,3))
lines(neut,col=4,pch=25,bottom=FALSE,lwd=1)
points(neut,col=6)
plot(neut1, conf.level=0.95, shade=TRUE,xlab="Days",ylab="Rainfall (mm)",main="Neutral ENSO-Phase")
plot(neut1,add=TRUE,link=TRUE,lwd=2,col=2:3, lty=c(1,3))
lines(neut1,col=4,pch=25,bottom=FALSE,lwd=1)
points(neut1,col=6)

dev.off()
