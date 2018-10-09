## loading the dataset 

setwd("/Users/anuragshukla/Desktop/github/Rstats/data")
framingham <- read.csv("framingham.csv")

## Looking at the strcture of the data

str(framingham)

## Loading the caTools 

library(caTools)

## setting the seed 

set.seed(1000)

split <- sample.split(framingham$TenYearCHD, SplitRatio = 0.65)

train <- subset(framingham, split == TRUE)

test <- subset(framingham, split== FALSE)


## Analyzing the logistic regression 

framinghamLog <- glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

## Predict the test set 

PredictTest <- predict(framinghamLog, type = "response", newdata = test)

## Making confusion matrix 

table(test$TenYearCHD, PredictTest > 0.5)

Accuracy <- (1069+11)/(1069+6+11+187)
Baseline <- (1069+6)/(1069+6+11+187)

## Finding AUC value 

library(ROCR)

ROCRpred <- prediction(PredictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)


