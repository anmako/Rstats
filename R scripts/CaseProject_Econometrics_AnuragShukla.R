
## Econometrics - Case Project

setwd("~/Desktop/github/Rstats/data")

library(readxl)
dataCase <- read_xls("CaseProject.xls")
str(dataCase)
summary(dataCase)

# Adding log variables of sell and lot

dataCase$sell_Log <- log(dataCase$sell)
dataCase$lot_Log <- log(dataCase$lot)

summary(dataCase)

# Estimating a linear model (a)

modelLM <- lm(sell ~ lot + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg, data = dataCase)
summary(modelLM)

## Model Linearity Testing (Ramsey's RESET testing)

install.packages("lmtest")
library(lmtest)

modelLM.RESET <- resettest(modelLM, power = 2, type = "fitted", data = dataCase)
print(modelLM.RESET)

## Model Jarque-Bera testing

install.packages("normtest")
library(normtest)
modelLM_JB <- jb.norm.test(modelLM$residuals)
print(modelLM_JB)

## plotting the fitted.value diagram

ModelLM.fitted <- fitted.values(modelLM)
library(ggplot2)
ggplot(dataCase, aes(x = ModelLM.fitted, y = sell)) + geom_point(shape = 16) + geom_smooth()


## Second Model Estimation (b)

modelLM1 <- lm(sell_Log ~ lot + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg, data = dataCase)
summary(modelLM1)


## Model linearity testing (RESET test)

modelLM1.RESET <- resettest(modelLM1, power = 2, type = "fitted", data = dataCase)
print(modelLM1.RESET)

## Jarque-Bera test for normality

modelLM_JB1<- jb.norm.test(modelLM1$residuals)
print(modelLM_JB1)

## plotting the fitted.value diagram for the second model

ModelLM.fitted1 <- fitted.values(modelLM1)
library(ggplot2)
ggplot(dataCase, aes(x = ModelLM.fitted1, y = sell_Log)) + geom_point(shape = 16) + geom_smooth()


## Estimating the third model 

modelLM2 <- lm(sell_Log ~ lot + lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg, data = dataCase)
summary(modelLM2)

## Model Linearity Testing (RESET test)

modelLM2.RESET <- resettest(modelLM2, power = 2, type = "fitted", data = dataCase)
print(modelLM2.RESET)

## Jarque-Bera test for normality

modelLM_JB2<- jb.norm.test(modelLM2$residuals)
print(modelLM_JB2)

## plotting the fitted.value diagram for the third model

ModelLM.fitted2 <- fitted.values(modelLM2)
library(ggplot2)
ggplot(dataCase, aes(x = ModelLM.fitted2, y = sell_Log)) + geom_point(shape = 16) + geom_smooth()


## Interaction effects and variables 

modelInteraction <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg +
               lot_Log * bdms + lot_Log * fb + lot_Log * sty + lot_Log * drv + lot_Log * rec + 
               lot_Log * ffin + lot_Log * ghw + lot_Log * ca + lot_Log * gar + lot_Log * reg, 
             data = dataCase)

summary(modelInteraction)

## Model Linearity Testing (RESET Test)

modelInteraction.RESET <- resettest(modelInteraction, power = 2, type = "fitted", data = dataCase)
print(modelInteraction.RESET)

## Jarque-Bera test for normality

modelInteraction_JB<- jb.norm.test(modelInteraction$residuals)
print(modelInteraction_JB)

## plotting the fitted.value diagram for the fourth model

ModelInteraction.fitted <- fitted.values(modelInteraction)
library(ggplot2)
ggplot(dataCase, aes(x = ModelInteraction.fitted, y = sell_Log)) + geom_point(shape = 16) + geom_smooth()


## Interaction effects joint significance

modelE <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg +
                         lot_Log * drv + lot_Log * rec, data = dataCase)

summary(modelE)

## Model Linearity Testing (RESET Test)

modelE.RESET <- resettest(modelE, power = 2, type = "fitted", data = dataCase)
print(modelE.RESET)

## Jarque-Bera test for normality

modelE_JB <- jb.norm.test(modelE$residuals)
print(modelE_JB)

## plotting the fitted.value diagram for the fifth model


ModelE.fitted <- fitted.values(modelE)
library(ggplot2)
ggplot(dataCase, aes(x = ModelE.fitted, y = sell_Log)) + geom_point(shape = 16) + geom_smooth()

## Performing F-test on interaction model 

SSR_u <- sum(modelInteraction$residuals ^ 2)
SSR_r <- sum(modelE$residuals ^ 2)

SSR_nom <- (SSR_r - SSR_u)/10
SSR_den <- SSR_u / 524

SSR_ <- SSR_nom / SSR_den

SSR_p <- pf(SSR_, 10, 524)


## General to specific approach to regression 

model1 <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg +
              lot_Log * bdms + lot_Log * fb + lot_Log * sty + lot_Log * drv + lot_Log * rec + 
              lot_Log * ffin + lot_Log * ghw + lot_Log * ca + lot_Log * gar + lot_Log * reg, 
            data = dataCase)

summary(model1)

model2 <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg + lot_Log * bdms + lot_Log * fb + lot_Log * sty + lot_Log * drv + lot_Log * rec + lot_Log * ffin + lot_Log * ghw + lot_Log * ca + lot_Log * gar , data = dataCase)

summary(model2)

model3 <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg + lot_Log * fb + lot_Log * sty + lot_Log * drv + lot_Log * rec + lot_Log * ffin + lot_Log * ghw + lot_Log * ca + lot_Log * gar , data = dataCase)

summary(model3)

model4 <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg + lot_Log * fb + lot_Log * sty + lot_Log * drv + lot_Log * rec + lot_Log * ghw + lot_Log * ca + lot_Log * gar , data = dataCase)

summary(model4)

#after 10 iteration, all the variables are found significant

finalModel <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg + lot_Log * rec, data = dataCase)

summary(finalModel)

## Model Linearity Testing (RESET Test)

library(lmtest)
modelFinal.RESET <- resettest(finalModel, power = 2, type = "fitted", data = dataCase)
print(modelFinal.RESET)

## Jarque-Bera test for normality

library(normtest)
modelFinal_JB <- jb.norm.test(finalModel$residuals)
print(modelFinal_JB)

## plotting the fitted.value diagram for the fifth model


ModelFinal.fitted <- fitted.values(finalModel)
library(ggplot2)
ggplot(dataCase, aes(x = ModelFinal.fitted, y = sell_Log)) + geom_point(shape = 16) + geom_smooth()


## Separating the data sample in two groups

dataCase1 <- dataCase[which(dataCase$obs <= 400), ]
str(dataCase1)

dataCase2 <- dataCase[which(dataCase$obs > 400), ]
str(dataCase2)

## Seventh model estimation 

modelSeventh <- lm(sell_Log ~ lot_Log + bdms + fb + sty + drv + rec + ffin + ghw + ca + gar + reg, 
             data = dataCase1)
summary(modelSeventh)

## Model Linearity Testing (RESET Test)

library(lmtest)
modelSeventh.RESET <- resettest(modelSeventh, power = 2, type = "fitted", data = dataCase1)
print(modelSeventh.RESET)

## Jarque-Bera test for normality

library(normtest)
modelSeventh_JB <- jb.norm.test(modelSeventh$residuals)
print(modelSeventh_JB)

## plotting the fitted.value diagram for the fifth model


ModelSeventh.fitted <- fitted.values(modelSeventh)
library(ggplot2)
ggplot(dataCase1, aes(x = ModelSeventh.fitted, y = sell_Log)) + geom_point(shape = 16) + geom_smooth()

## Calculating sell_Log variance and MAE (Mean Absolute Error) index calculation

sell_Log_mean <- mean(dataCase$sell_Log)
sell_Log_sd <- sd(dataCase$sell_Log)

sell_Log_mean = 11.05896
sell_Log_sd =  0.3719849

digits <- 3
n <- nrow(dataCase2)
resid_sum <- sum(abs(dataCase2$residuals))

MAE = 0.128
