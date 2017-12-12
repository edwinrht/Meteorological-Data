library(wq)
pdf("WQ.pdf", paper="a4r",useKerning=TRUE,width = 0, height = 0)
M=ts(cnrm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 12, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(y2, type = 'slope.pct', plot = TRUE)

m=ts(cnrm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010, 2015), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'The Nilgiris', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)
plotSeason(m, "by.month", ylab ="TMax °C")

m=ts(cnrm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'The Nilgiris', legend.title ='Tmin °C', four = FALSE, loganom = TRUE, square = FALSE)

m=ts(cnrm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'The Nilgiris', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(cnrm$YPH, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Yield kg/ha')
plotTsTile(m, plot.title = 'The Nilgiris', legend.title ='Yield kg/ha', four = FALSE, loganom = FALSE, square = FALSE)

M=ts(gudm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(M, type = 'slope.pct', plot = TRUE)

m=ts(gudm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'Gudalur', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(gudm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'Gudalur', legend.title ='Tmin °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(gudm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'Gudalur', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)

M=ts(kopm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(M, type = 'slope.pct', plot = TRUE)

m=ts(kopm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'Koppa', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(kopm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'Koppa', legend.title ='Tmin °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(kopm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'Koppa', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)


M=ts(mepm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(M, type = 'slope.pct', plot = TRUE)

m=ts(mepm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'Meppadi', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(mepm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'Meppadi', legend.title ='Tmin °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(mepm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'Meppadi', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)



M=ts(mnrm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(M, type = 'slope.pct', plot = TRUE)

m=ts(mnrm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'Munnar', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(mnrm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'Munnar', legend.title ='Tmin °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(mnrm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'Munnar', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(mnrm$YPH, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Yield kg/ha')
plotTsTile(m, plot.title = 'Munnar', legend.title ='Yield kg/ha', four = FALSE, loganom = FALSE, square = FALSE)

M=ts(vlpm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(M, type = 'slope.pct', plot = TRUE)

m=ts(vlpm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'The Anamalais', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(vlpm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'The Anamalais', legend.title ='Tmin °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(vlpm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'The Anamalais', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(vlpm$YPH, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Yield kg/ha')
plotTsTile(m, plot.title = 'The Anamalais', legend.title ='Yield kg/ha', four = FALSE, loganom = FALSE, square = FALSE)

M=ts(vprm, start=1981, frequency=12)
mannKen(M)
y1 < - interpTs(M)
y2 < - aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2, plot=TRUE, type='pct', order=TRUE)
seasonTrend(M, type = 'slope.pct', plot = TRUE)

m=ts(vprm$Tmax, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'TMax °C')
plotTsTile(m, plot.title = 'Vandiperiyar', legend.title ='Tmax °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(vprm$Tmin, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Tmin °C')
plotTsTile(m, plot.title = 'Vandiperiyar', legend.title ='Tmin °C', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(vprm$RF, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'RF mm')
plotTsTile(m, plot.title = 'Vandiperiyar', legend.title ='RF mm', four = FALSE, loganom = FALSE, square = FALSE)

m=ts(vprm$YPH, start=1981, frequency=12)
plotSeason(m, num.era = c(1981, 1990, 2000, 2010), ylab = 'Yield kg/ha')
plotTsTile(m, plot.title = 'Vandiperiyar', legend.title ='Yield kg/ha', four = FALSE, loganom = FALSE, square = FALSE)

dev.off()


jpeg(file = "myplot.jpeg", quality = 100)
plotTsTile(m, plot.title = 'The Nilgiris', legend.title ='Yield kg/ha', four = FALSE, loganom = FALSE, square = FALSE)
dev.off()
