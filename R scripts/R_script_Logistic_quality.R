## Logistic Regression ##

setwd("/Users/anuragshukla/Desktop/github/Rstats/data")

quality <- read.csv("quality.csv")

str(quality)

## Looking at the plots 

plot(quality$OfficeVisits, quality$Narcotics)

## Building a table 

table(quality$PoorCare)

## Baseline prediction 

98/131

## setting the seed 

library(caTools)
set.seed(88)
split <- sample.split(quality$PoorCare, SplitRatio = 0.75)
qualitytrain <- subset(quality, split == TRUE)
qualitytest <- subset(quality, split == FALSE)
nrow(qualitytrain)
nrow(qualitytest)
QualityLog <- glm(PoorCare ~ OfficeVisits + Narcotics, data=qualitytrain, family=binomial)
summary(QualityLog)

# Make predictions on training set

predictTrain = predict(QualityLog, type="response")

# Analyze predictions

summary(predictTrain)
tapply(predictTrain, qualitytrain$PoorCare, mean)


## Preparing confusion matrix

table(qualitytrain$PoorCare, predictTrain > 0.5)

# Install and load ROCR package

install.packages("ROCR")

library(ROCR)

ROCRpred <- prediction(predictTrain, qualitytrain$PoorCare)

ROCRperf <- performance(ROCRpred, "tpr", "fpr")

plot(ROCRperf)

plot(ROCRperf, colorize=TRUE)

plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
