## Codes used for wavelet transforms and coherence plots

install.packages("biwavelet")
library(biwavelet)
# Import your data

DataPath<-read.table(file="C:\\Users\\iiml\\Desktop\\workshop\\foodindex.csv", header=TRUE, sep=",", stringsAsFactors=FALSE)
head(DataPath)
dim(DataPath)
names(DataPath)
attach(DataPath)

#Our variables are Food, Dairy, Cereals.Oils,Suger.RBRTE
# Define two sets of variables with time stamps
t1 = cbind(Date, Food)
t2 = cbind(Date, Dairy)
t3 = cbind(Date, Cereals)
t4 = cbind(Date, Oils)
t5 = cbind(Date, Sugar)
t6 = cbind(Date, RBRTE)

# Continuous wavelet transform
wt.t1=wt(t1,do.sig=TRUE)
wt.t2=wt(t2,do.sig=TRUE)
wt.t3=wt(t3,do.sig=TRUE)
wt.t4=wt(t4,do.sig=TRUE)
wt.t5=wt(t5,do.sig=TRUE)
wt.t6=wt(t6,do.sig=TRUE)

# Plotting Wavelet power spectrum of series used in the study
par(oma=c(0, 0, 0, 0), mar=c(5, 4, 4, 5) + 0.1)
plot(wt.t1, plot.cb=TRUE, plot.phase=FALSE)
plot(wt.t2, plot.cb=TRUE, plot.phase=FALSE)
plot(wt.t3, plot.cb=TRUE, plot.phase=FALSE)
plot(wt.t4, plot.cb=TRUE, plot.phase=FALSE)
plot(wt.t5, plot.cb=TRUE, plot.phase=FALSE)
plot(wt.t6, plot.cb=TRUE, plot.phase=FALSE)


# Estimation of Wavelet Coherence for pairs
# Specify the number of iterations. For the purpose of this study, we set it = 1000
nrands = 1000
wtc.61 = wtc(t6, t1, nrands = nrands)
wtc.62 = wtc(t6, t2, nrands = nrands)
wtc.63 = wtc(t6, t3, nrands = nrands)
wtc.64 = wtc(t6, t4, nrands = nrands)
wtc.65 = wtc(t6, t5, nrands = nrands)

# Plotting Wavelet power spectrum of series used in the study
plot(wtc.61, ncol=64,plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE)
plot(wtc.62, ncol=64,plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE)
plot(wtc.63, ncol=64,plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE)
plot(wtc.64, ncol=64,plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE)
plot(wtc.65, ncol=64,plot.phase = TRUE, lty.coi = 1, col.coi = "grey", lwd.coi = 2, 
     lwd.sig = 2, arrow.lwd = 0.03, arrow.len = 0.12, ylab = "Scale", xlab = "Period", 
     plot.cb = TRUE)



