library(zoo)
aggregate(x0, as.yearmon, sum)

library(xts)
Achoor<-read.table("Achoor  - Copy.txt",T,sep="\t")
Achoor < - xts(Achoor,as.Date(0:12052,origin="1983-01-01"))
Achoorm <-apply.monthly(Achoor,mean,na.rm=TRUE)
write.csv(Achoorm,"Achoorm.csv")

t< - read.table("clipboard", header=TRUE, sep="\t")

apply.monthly(xts.ts,mean,na.rm=TRUE)
