## Evaluating a 2SLS model 

setwd("~/Desktop/github/Rstats/data")

library(readxl)
dataexercise <- read_xls("TrainExer45.xls")

# Looking at the structure of the data 

str(dataexercise)

# Getting the OLS results 

model1 <- lm(GPA ~ GENDER + PARTICIPATION, data = dataexercise)
summary(model1)

# 1st stage regression 

model2 <- lm(PARTICIPATION ~ GENDER + EMAIL, data = dataexercise)
summary(model2)

# 2nd stage regression 

dataexercise$instrumented.participation <- model2$fitted.values
str(dataexercise)
modelfinal <- lm(GPA ~ GENDER + instrumented.participation, data = dataexercise)
summary(modelfinal)



