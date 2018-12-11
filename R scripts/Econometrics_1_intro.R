## Econometrics Introduction 


library(AER)
set.seed(1071)

###################################################
### code chunk number 2: Linear Regression
###################################################

data("Journals", package = "AER")
plot(log(subs) ~ log(price/citations), data = Journals,
     main = "Demand for economics journals")
journals <- Journals[, c("subs", "price")]
journals$citeprice <- Journals$price/Journals$citations
jour_lm <- lm(log(subs) ~ log(citeprice), data = journals)
lciteprice <- seq(from = -6, to = 4, by = 0.25)
jour_pred <- predict(jour_lm, interval = "prediction",
                     newdata = data.frame(citeprice = exp(lciteprice)))

lines(jour_pred[, 1] ~ lciteprice, col = 2)    
lines(jour_pred[, 2] ~ lciteprice, col = 2, lty = 2)
lines(jour_pred[, 3] ~ lciteprice, col = 2, lty = 2)


###################################################
### code chunk number 3: quantreg (Quantile Regression)
###################################################
data("CPS1985", package = "AER")
install.packages("quantreg")
cps <- CPS1985
library("quantreg")
cps_lm <- lm(log(wage) ~ experience + I(experience^2) +
               education, data = cps)
cps_rq <- rq(log(wage) ~ experience + I(experience^2) +
               education, data = cps, tau = seq(0.2, 0.8, by = 0.15))
cps2 <- data.frame(education = mean(cps$education),
                   experience = min(cps$experience):max(cps$experience))
cps2 <- cbind(cps2, predict(cps_lm, newdata = cps2,
                            interval = "prediction"))
cps2 <- cbind(cps2,
              predict(cps_rq, newdata = cps2, type = ""))  
plot(log(wage) ~ experience, data = cps,
     main = "Quantile regression for a wage equation")
for(i in 6:10) lines(cps2[,i] ~ experience,
                     data = cps2, col = "red")


#########################################################
### code chunk number 4: dynlm (Time series regression)
#########################################################
data("USMacroG", package = "AER")
install.packages("dynlm")
library("dynlm")
cons_lm1 <- dynlm(consumption ~ dpi + L(dpi), data = USMacroG)
cons_lm2 <- dynlm(consumption ~ dpi + L(consumption), 
                  data = USMacroG)
plot(merge(as.zoo(USMacroG[,"consumption"]), fitted(cons_lm1),
           fitted(cons_lm2), 0, residuals(cons_lm1),
           residuals(cons_lm2)), screens = rep(1:2, c(3, 3)),
     col = rep(c(1, 2, 4), 2), ylab = c("Fitted values", "Residuals"),
     xlab = "Time", main = "US consumption functions")
legend(0.05, 0.95, c("Observed", "Distributed lag", "Autoregressive distributed lag"), 
       col = c(1, 2, 4), lty = 1, bty = "n")


###################################################################
### code chunk number 5: strucchange (Structural Change Analysis)
###################################################################
install.packages("strucchange")
library("strucchange")
data("UKDriverDeaths")
dd <- log(UKDriverDeaths)
dd_dat <- ts.intersect(dd, dd1 = lag(dd, k = -1),
                       dd12 = lag(dd, k = -12))
dd_bp <- breakpoints(dd ~ dd1 + dd12, data = dd_dat, h = 0.1)
plot(dd, ylab = "UK driver deaths", main = "Change in seatbelt legislation in the UK")
lines(fitted(dd_bp, breaks = 2), col = 4)
lines(confint(dd_bp, breaks = 2))


###################################################
### code chunk number 6: urca (Cointegration)
###################################################
data("PepperPrice", package = "AER")
plot(PepperPrice, plot.type = "single", col = c("black", "slategray"),
     ylab = "Average monthly spot price",
     main = "European pepper prices")
legend("topleft", c("white", "black"), bty = "n", 
       col = c("slategray", "black"), lty = rep(1,2), lwd = 2)


####################################################################
### code chunk number 7: countreg (Regression model for count data)
####################################################################
data("RecreationDemand", package = "AER")
plot(table(RecreationDemand$trips),
     main = "Recreational trips to Lake Somerville",
     xlab = "Number of trips", ylab = "Frequency")



