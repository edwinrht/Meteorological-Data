library(tgp)
seed < - 0; set.seed(seed)
data=cnra
data$Tmax=(data$Tmax-mean(data$Tmax))/sd(data$Tmax)
data$Tmin=(data$Tmin-mean(data$Tmin))/sd(data$Tmin)
data$RH830=(data$RH830-mean(data$RH830))/sd(data$RH830)
data$RH1430=(data$RH1430-mean(data$RH1430))/sd(data$RH1430)
data$RF=(data$RF-mean(data$RF))/sd(data$RF)
data$Rday=(data$Rday-mean(data$Rday))/sd(data$Rday)
data$S=(data$S-mean(data$S))/sd(data$S)
data$Sday=(data$Sday-mean(data$Sday))/sd(data$Sday)
data$W=(data$W-mean(data$W))/sd(data$W)
data$S830=(data$S830-mean(data$S830))/sd(data$S830)
data$S1430=(data$S1430-mean(data$S1430))/sd(data$S1430)
data$Sm=(data$Sm-mean(data$Sm))/sd(data$Sm)
data$ETo=(data$ETo-mean(data$ETo))/sd(data$ETo)
data$clud=(data$clud-mean(data$clud))/sd(data$clud)
Xf=data[c("Tmax","Tmin","RH830","RH1430","RF","Rday","S","Sday","W","S830","S1430","Sm","ETo","clud")]
Zf=data[c("YPH")]
Xf=data[c("Tmax","Tmin","RF")]

sf < - sens(X=Xf, Z=Zf, nn.lhs=1000, model=blm, verb=1, bprior="b0", R=2,trace=TRUE)
sf < - sens(X=Xf, Z=Zf, nn.lhs=10, model=bgp, pred.n=FALSE, verb=1, bprior="b0", R=2, basemax = 10)
sf < - sens(X=Xf, Z=Zf, nn.lhs=1000, model=blm, verb=1, pred.n=TRUE, basemax = 10, bprior="b0")
sg < - sens(X=Xf, Z=Zf, nn.lhs=1000, model=blm, verb=1, pred.n=TRUE, basemax = 10, bprior="bmzt")

sf < - sens(X=Xf, Z=Zf, nn.lhs=1000, model=btgp, verb=1, pred.n=TRUE, basemax = 10,bprior="bmznot",corr="exp")
sg < - sens(X=Xf, Z=Zf, nn.lhs=1000, model=bgpllm, verb=1, pred.n=TRUE, basemax = 10,bprior="bmznot",corr="exp")#low RMSE
sh < - sens(X=Xf, Z=Zf, nn.lhs=1000, model=blm, verb=1, pred.n=TRUE, basemax = 10,bprior="bmznot",corr="exp")
sqrt(mean((sg$Zp.mean  - Zf).2))
sqrt(mean((sh$Zp.mean  - Zf).2))
sqrt(mean((sf$Zp.mean  - Zf).2))

names(sf$sens)
plot(sf, layout="sens", legendloc="topleft")
graphics.off()
par(mar=c(4,2,4,2), mfrow=c(2,7))
plot(sf, layout="sens", maineff=t(1:7))
tgp.trees(sf, "map")



rect < - t(apply(Xf, 2, range, na.rm=TRUE))
mode < - apply(Xf, 2, mean, na.rm=TRUE)
shape < - rep(2,14)
Udraw< - lhs(300, rect=rect, mode=mode, shape=shape)
par(mfrow=c(1,7), mar=c(4,2,4,2))
for(i in 1:7){
hist(Udraw[,i], breaks=10,xlab=names(Xf)[i],
main="",ylab="", border=grey(.9), col=8)
}

par(mfrow=c(2,7), mar=c(4,2,4,2))
for(i in 1:14){
hist(Udraw[,i], breaks=10,xlab=names(Xf)[i],
main="",ylab="", border=grey(.9), col=8)
}

sp < - suppressWarnings(sens(X=Xf,Z=Zf,nn.lhs=300, model=NULL, ngrid=100, rect=rect, shape=shape, mode=mode))
sw < - predict(sg, BTE=c(1,1000,1), sens.p=sp, verb=1)
sx < - predict(sh, BTE=c(1,1000,1), sens.p=sp, verb=1)
plot(sw, layout="sens", ylab="YPH", main="main effects", maineff=t(1:7))
plot(sx, layout="sens", ylab="YPH", main="main effects", maineff=t(1:7))



#blm ++LM ++Linear Model
#btlm ++T, LM ++Treed Linear Model
#bcart ++T ++Treed Constant Model (do not use bcz to will give large RMSE)
#bgp ++GP ++Gaussian Process Regression
#bgpllm +GP, LLM +GP with jumps to the LLM 
#btgp ++T, GP ++treed GP Regression
#btgpllm +T, GP, LLM +treed GP with jumps to the LLM
