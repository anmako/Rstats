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

framinghamLog <- glm(TenYearCHD ~ ., data = framingham, family = binomial)
summary(framinghamLog)

## Predict the test set 

PredictTest <- predict(framinghamLog, type = "response", newdata = test)
