library(forecast)
a=ts(cnrm$YPH,start=1981, frequency=12)
b=ts(gudm$YPH,start=1981, frequency=12)
c=ts(kopm$YPH,start=1981, frequency=12)
d=ts(mepm$YPH,start=1981, frequency=12)
e=ts(mnrm$YPH,start=1981, frequency=12)
f=ts(vlpm$YPH,start=1981, frequency=12)
g=ts(vprm$YPH,start=1981, frequency=12)
fita=auto.arima(a)
fitb=auto.arima(b)
fitc=auto.arima(c)
fitd=auto.arima(d)
fite=auto.arima(e)
fitf=auto.arima(f)
fitg=auto.arima(g)
summary(fita)
Box.test(fita$residuals,type="Ljung-Box")
shapiro.test(fita$residuals)
summary(fitb)
Box.test(fitb$residuals,type="Ljung-Box")
shapiro.test(fitb$residuals)
summary(fitc)
Box.test(fitc$residuals,type="Ljung-Box")
shapiro.test(fitc$residuals)
summary(fitd)
Box.test(fitd$residuals,type="Ljung-Box")
shapiro.test(fitd$residuals)
summary(fite)
Box.test(fite$residuals,type="Ljung-Box")
shapiro.test(fite$residuals)
summary(fitf)
Box.test(fitf$residuals,type="Ljung-Box")
shapiro.test(fitf$residuals)
summary(fitg)
Box.test(fitg$residuals,type="Ljung-Box")
shapiro.test(fitg$residuals)
a=a-fita$residuals
b=b-fitb$residuals
c=c-fitc$residuals
d=d-fitd$residuals
e=e-fite$residuals
f=f-fitf$residuals
g=f-fitg$residuals

#fit.pred=predict(fit,n.ahead=48)
#acf(fit$residuals); pacf(fit$residuals)

pdf("ARIMA.pdf", paper="a4r",useKerning=TRUE,width = 0, height = 0)
plot(forecast(fita,h=60))
tsdiag(fita)
plot(forecast(fita,h=60))
tsdiag(fita)
dev.off()
##ARIMA @ SEASON###
#CNRs
fit=arima(ts(cnrs$YPH,start=1981, frequency=4), c(1,0,2), seasonal=list(order=c(1,0,2),period=4))
plot(forecast(fit,h=20))
tsdiag(fit)
accuracy(fit)
shapiro.test(fit$residuals)
##MNRs
fit=arima(ts(mnrs$YPH,start=1981, frequency=4), c(2,0,2), seasonal=list(order=c(2,1,2),period=4))
plot(forecast(fit,h=20))
tsdiag(fit)
accuracy(fit)
shapiro.test(fit$residuals)
#VLPs
fit=arima(ts(vlps$YPH,start=1981, frequency=4), c(1,0,2), seasonal=list(order=c(1,1,2),period=4))
plot(forecast(fit,h=20))
tsdiag(fit)
accuracy(fit)
shapiro.test(fit$residuals)
#VPRs
fit=arima(ts(vprs$YPH,start=1981, frequency=4), c(2,1,2), seasonal=list(order=c(1,1,2),period=4))
plot(forecast(fit,h=20))
tsdiag(fit)
accuracy(fit)
shapiro.test(fit$residuals)
###TSA#######
library(TSA)
x=cnrm$YPH
y=cnrm[c("Tmax","Tmin","RH830","RH1430","RF","Rday","S","Sday","W","S830","S1430","Sm","ETo","clud")]
fit=arima(x, order = c(4, 0, 1), seasonal = list(order = c(2, 0, 0), period = 12), xreg = y, method = "ML")#CNRM
fit=arimax(x, order = c(4, 0, 1), seasonal = list(order = c(2, 0, 0), period = 12), xreg = y, method = "ML")#CNRM
LB.test(fit)

#######Diebold-Mariano Test
Obs=t$Obs-t$VAR
sim=t$Obs-t$ANN
dm.test(obs,sim,h=1)
