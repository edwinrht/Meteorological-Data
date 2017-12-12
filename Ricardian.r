library(plm)
t< - read.table("clipboard", header=TRUE, sep="\t")
Ric<-plm.data(ric, index = c("Region", "Year"))
form=YPH~Ptea+Worker+Pworker+Mange+pH+OM+Sand+slit+clay+Bluk+Gravel+Tx_D+Tx.2_D+Tn_D+Tn.2_D+RF_D+RF.2_D+Tx_M+Tx.2_M+Tn_M+Tn.2_M+RF_M+RF.2_M+Tx_J+Tx.2_J+Tn_J+Tn.2_J+RF_J+RF.2_J+Tx_S+Tx.2_S+Tn_S+Tn.2_S+RF_S+RF.2_S
form=YPH~CO+Workers+pWorker+Fertilizer+pFertilizer+Tx_D+Tx.2_D+Tn_D+Tn.2_D+RF_D+RF.2_D+Rd_D+Rd.2_D+S_D+S.2_D+Sd_D+Sd.2_D+ET_D+ET.2_D+Tx_M+Tx.2_M+Tn_M+Tn.2_M+RF_M+RF.2_M+Rd_M+Rd.2_M+S_M+S.2_M+Sd_M+Sd.2_M+ET_M+ET.2_M+Tx_J+Tx.2_J+Tn_J+Tn.2_J+RF_J+RF.2_J+Rd_J+Rd.2_J+S_J+S.2_J+Sd_J+Sd.2_J+ET_J+ET.2_J+Tx_S+Tx.2_S+Tn_S+Tn.2_S+RF_S+RF.2_S+Rd_S+Rd.2_S+S_S+S.2_S+Sd_S+Sd.2_S+ET_S+ET.2_S
fit=lm(YPH~Ptea+Worker+Mange+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=Ric)
summary(fit)

#fitp=plm(form,data=Ric,model = "pooling",na.action=na.omit)
fitw=plm(form,data=Ric,model = "within",na.action=na.omit)# Fixed effects model
#fitr=plm(form,data=Ric,model = "random",na.action=na.omit)

## Pooled model (regular OLS)

fitlm< - lm(YPH~CO+Workers+pWorker+Fertilizer+pFertilizer+Tx_D+Tx.2_D+Tn_D+Tn.2_D+RF_D+RF.2_D+Rd_D+Rd.2_D+S_D+S.2_D+Sd_D+Sd.2_D+ET_D+ET.2_D+Tx_M+Tx.2_M+Tn_M+Tn.2_M+RF_M+RF.2_M+Rd_M+Rd.2_M+S_M+S.2_M+Sd_M+Sd.2_M+ET_M+ET.2_M+Tx_J+Tx.2_J+Tn_J+Tn.2_J+RF_J+RF.2_J+Rd_J+Rd.2_J+S_J+S.2_J+Sd_J+Sd.2_J+ET_J+ET.2_J+Tx_S+Tx.2_S+Tn_S+Tn.2_S+RF_S+RF.2_S+Rd_S+Rd.2_S+S_S+S.2_S+Sd_S+Sd.2_S+ET_S+ET.2_S,data=Ric, na.action=na.omit)
summary(fitlm)

## LSDV model
lsdvr< - lm(YPH~CO+Workers+pWorker+Fertilizer+pFertilizer+Tx_D+Tx.2_D+Tn_D+Tn.2_D+RF_D+RF.2_D+Rd_D+Rd.2_D+S_D+S.2_D+Sd_D+Sd.2_D+ET_D+ET.2_D+Tx_M+Tx.2_M+Tn_M+Tn.2_M+RF_M+RF.2_M+Rd_M+Rd.2_M+S_M+S.2_M+Sd_M+Sd.2_M+ET_M+ET.2_M+Tx_J+Tx.2_J+Tn_J+Tn.2_J+RF_J+RF.2_J+Rd_J+Rd.2_J+S_J+S.2_J+Sd_J+Sd.2_J+ET_J+ET.2_J+Tx_S+Tx.2_S+Tn_S+Tn.2_S+RF_S+RF.2_S+Rd_S+Rd.2_S+S_S+S.2_S+Sd_S+Sd.2_S+ET_S+ET.2_S+factor(Region),data=Ric, na.action=na.omit)
lsdvs< - lm(YPH~CO+Workers+pWorker+Fertilizer+pFertilizer+Tx_D+Tx.2_D+Tn_D+Tn.2_D+RF_D+RF.2_D+Rd_D+Rd.2_D+S_D+S.2_D+Sd_D+Sd.2_D+ET_D+ET.2_D+Tx_M+Tx.2_M+Tn_M+Tn.2_M+RF_M+RF.2_M+Rd_M+Rd.2_M+S_M+S.2_M+Sd_M+Sd.2_M+ET_M+ET.2_M+Tx_J+Tx.2_J+Tn_J+Tn.2_J+RF_J+RF.2_J+Rd_J+Rd.2_J+S_J+S.2_J+Sd_J+Sd.2_J+ET_J+ET.2_J+Tx_S+Tx.2_S+Tn_S+Tn.2_S+RF_S+RF.2_S+Rd_S+Rd.2_S+S_S+S.2_S+Sd_S+Sd.2_S+ET_S+ET.2_S+factor(Soil),data=Ric, na.action=na.omit)
summary(lsdvr)
summary(lsdvs)

##Linear model with bootstrap
library(simpleboot)
lboot <- lm.bootfit, R = 1000)
summary(lboot)

## With weighting
w <- runif(nrow(model.frame(fit)))
fit=lm(NR~Ptea+Worker+Pworker+Mange+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=Ric,weight=w)
or

fit=lm(NR~Ptea+Worker+Mange+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=Ric,weight=(2*seq(nrow(Ric))))
lbootw <- lm.boot(fit, R = 1000, weights = w)
summary(lbootw)

## Resample residuals
lboot2 <- lm.boot(lmodel, R = 1000, rows = FALSE)
summary(lboot2)


## First difference model - - not often used in political science panel data
fitfd < - plm(form, data=Ric, index=c("Region", "Year"), na.action=na.omit, model="fd")
summary(fitfd)

## F test for fixed effects

anova(fitlm, lsdvr)

## Using the plm package ##

fitp< - plm(form, data=Ric, index=c("Region", "Year"), na.action=na.omit, model="pooling")
summary(fitp)

## Within model

fitw < - plm(form, data=Ric, index=c("Region", "Year"), na.action=na.omit, model="within")
summary(fitw)
summary(fixef(fitw))
fitwt < - plm(form, data=Ric, index=c("Region", "Year"), na.action=na.omit, model="within",effect = "twoways")
summary(fixef(fitwt,effect = "time"))

# an example of the formula method
ercomp(form,data=Ric,method="walhus",effect="time")
ercomp(form,data=Ric,method="walhus",effect="twoway")


## random
fitrr=plm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Region),data=ric,model="random",effect="time",na.omit=TRUE)
fitrs=plm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=ric,model="random",effect="time",na.omit=TRUE)
fitr=plm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=ric,model="random",effect="time",na.omit=TRUE)
fitrr1=plm(log(YPH)~Ptea+Pworker+Nworker+Eworker+Fertilizer+factor(Region),data=ric,model="random",effect="time",na.omit=TRUE)
fitrs1=plm(log(YPH)~Ptea+Pworker+Nworker+Eworker+Fertilizer+factor(Soil),data=ric,model="random",effect="time",na.omit=TRUE)
fitr1=plm(log(YPH)~Ptea+Pworker+Nworker+Eworker+Fertilizer,data=ric,model="random",effect="time",na.omit=TRUE)
fitrr2=plm(log(YPH)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel+factor(Region),data=ric,model="random",effect="time",na.omit=TRUE)
fitrs2=plm(log(YPH)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel+factor(Soil),data=ric,model="random",effect="time",na.omit=TRUE)
fitr2=plm(log(YPH)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel,data=ric,model="random",effect="time",na.omit=TRUE)
fitrr3=plm(log(YPH)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Region),data=ric,model="random",effect="time",na.omit=TRUE)
fitrs3=plm(log(YPH)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=ric,model="random",effect="time",na.omit=TRUE)
fitr3=plm(log(YPH)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=ric,model="random",effect="time",na.omit=TRUE)

fitwr=plm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Region),data=ric,model="within",effect="time",na.omit=TRUE)
fitws=plm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=ric,model="within",effect="time",na.omit=TRUE)
fitw=plm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=ric,model="within",effect="time",na.omit=TRUE)
fitwr1=plm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+factor(Region),data=ric,model="within",effect="time",na.omit=TRUE)
fitws1=plm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+factor(Soil),data=ric,model="within",effect="time",na.omit=TRUE)
fitw1=plm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer,data=ric,model="within",effect="time",na.omit=TRUE)
fitwr2=plm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel+factor(Region),data=ric,model="within",effect="time",na.omit=TRUE)
fitws2=plm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel+factor(Soil),data=ric,model="within",effect="time",na.omit=TRUE)
fitw2=plm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel,data=ric,model="within",effect="time",na.omit=TRUE)
fitwr3=plm(log(NR)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Region),data=ric,model="within",effect="time",na.omit=TRUE)
fitws3=plm(log(NR)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=ric,model="within",effect="time",na.omit=TRUE)
fitw3=plm(log(NR)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=ric,model="within",effect="time",na.omit=TRUE)

fitpr=lm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Region),data=ric,weight=w)
fitps=lm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=ric,weight=w)
fitp=lm(YPH~log(Ptea)+log(Pworker)+log(Worker)+log(Mange)+log(NR)+pH+Sand+slit+clay+Bluk+Gravel+TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=ric,weight=w)
fitpr1=lm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+factor(Region),data=ric,model="pooling",effect="time",na.omit=TRUE)
fitps1=lm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+factor(Soil),data=ric,model="pooling",effect="time",na.omit=TRUE)
fitp1=lm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer,data=ric,model="pooling",effect="time",na.omit=TRUE)
fitpr2=lm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel+factor(Region),data=ric,model="pooling",effect="time",na.omit=TRUE)
fitps2=lm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel+factor(Soil),data=ric,model="pooling",effect="time",na.omit=TRUE)
fitp2=lm(log(NR)~Ptea+Pworker+Nworker+Eworker+Fertilizer+pH+Sand+slit+clay+Bluk+Gravel,data=ric,model="pooling",effect="time",na.omit=TRUE)
fitpr3=lm(log(NR)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Region),data=ric,model="pooling",effect="time",na.omit=TRUE)
fitps3=lm(log(NR)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2+factor(Soil),data=ric,model="pooling",effect="time",na.omit=TRUE)
fitp3=lm(log(NR)~TxD+TxD2+TnD+TnD2+RD+RD2+TxM+TxM2+TnM+TnM2+RM+RM2+TxJ+TxJ2+TnJ+TnJ2+RJ+RJ2+TxS+TxS2+TnS+TnS2+RS+RS2,data=ric,model="pooling",effect="time",na.omit=TRUE)


pooltest(fitw,fitp)
plmtest(fitw,effect="twoways",type="ghm")
plmtest(fitw,effect="individual",type="bp")
pFtest(form,data=Ric,effect="twoway")
pwtest(form,data=Ric)
pbsytest(form,data=Ric,test="j")
pbsytest(form,data=Ric)
pbsytest(form,data=Ric,test="re")
pbltest(form,data=Ric,alternative="onesided")
pbgtest(fitw, order=2)
pwartest(form,data=Ric)
pwfdtest(form,data=Ric)
pwfdtest(form,data=Ric,h0="fe")
pcdtest(form,data=Ric)
pcdtest(form,data=Ric,model="within")

#Unit root tests for panel data
y < - data.frame(split(Ric$YPH, Ric$Region))
purtest(y, pmax = 4, exo = "intercept", test = "madwu")

## Hausman test

phtest(pmodel2, pmodel3)
