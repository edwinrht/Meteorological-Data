library(hydroTSM)
x=vlpd$precip
xts.ts=xts(x,as.Date(1:12775,origin="1981-01-01"))
hydroplot(xts.ts,FUN=mean,ylab="Q",var.unit="m3/s")
library(lattice)
m=daily2monthly(xts.ts,FUN=sum,na.rm=TRUE)
M=matrix(m,ncol=12,byrow=TRUE)
colnames(M)=month.abb
rownames(M)=unique(time(m),"%Y")
print(matrixplot(M,ColorRamp="Precipitation",main="Monthly precipitation at San Martino st., [mm/month]"))
hydroplot(xts.ts, pfreq="seasonal", FUN=sum, stype="default")
