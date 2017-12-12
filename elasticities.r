elasticities <- function(linmod){
Ncoef <- nrow(data.frame(linmod$coefficients))
for(i in 2:Ncoef){
  el <- as.numeric(linmod$coefficients[i] * colMeans(model.matrix(linmod))[i]/colMeans(model.matrix(linmod))[1])
  ifelse (i== 2, elasticity <- el, elasticity <- rbind(elasticity,el))
}
rownames(elasticity) <- names(coef(linmod)[-1])
colnames(elasticity) <- 'elasticities'

return(data.frame(elasticity))
}


fit=glm(YPH~RD+I(RD^2)+Region,data=ric)
a <- elasticities(fit)

Elasticity
PE<-as.numeric(fit$coefficients["Tx"] * mean(ric$Tx)/mean(ric$YPH))

Marginal
fit$coefficients["Tx"] * ric$YPH
