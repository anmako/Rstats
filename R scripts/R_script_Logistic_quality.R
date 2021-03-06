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

## Preparing confusion matrix

table(qualitytrain$)
