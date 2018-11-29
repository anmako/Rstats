## Predicting Customer Churn

setwd("~/Desktop/github/Rstats/data")

customers <- read.csv("telco-churn.csv")
table(customers$Churn)


##What is the mean monthly charges amongst customers with month-to-month contracts?

tapply(customers$MonthlyCharges, customers$Contract =="Month-to-month", mean)

## Simple Logistic Regression

set.seed(1)
library(caTools)
split <- sample.split(customers$Churn, SplitRatio = 0.7)
train <- subset(customers, split ==TRUE)
test <- subset(customers, split == FALSE)

##What is the (test) accuracy of this baseline model?

z <- table(test$Churn)
z[1]/sum(z)

##What is the (test) true positive rate of this baseline model?

z <- table(test$Churn)
z[2]/sum(z)

##Simple Logistic Regression

ChurnLog <- glm(Churn ~ tenure, data = train, family = "binomial")
summary(ChurnLog)

##Using your logistic regression model, obtain predictions on the test set. Then, using a probability threshold of 0.5, create a confusion matrix for the test set. What is the accuracy of your logistic regression model?

PredictTrain <- predict(ChurnLog, newdata = test, type = "response")
#confusion matrix
table(test$Churn, PredictTrain > 0.5)

accuracy <- (1478+119)/(1478+119+442+71)
accuracy

## Adding more variables

ChurnLog2 = glm(Churn ~ .,  data=train, family=binomial)
summary(ChurnLog2)


PredictTrain2 <- predict(ChurnLog2, newdata = test, type = "response")
table(test$Churn, PredictTrain2 > 0.5)

accuracy <- (310+1398)/(1398+310+151+251)
accuracy

library(ROCR)
pred <- prediction(PredictTrain2, test$Churn)
as.numeric(performance(pred, "auc")@y.values)
perf <- performance(pred,"tpr","fpr")
plot(perf,col="black",lty=3, lwd=3)


## CART
set.seed(2)
train$Churn <- as.factor(train$Churn)
library(caret)
library(e1071)
library(rpart)
library(rpart.plot)

# Define cross-validation experiment

# Perform the cross validation
cv = train(y = train$Churn, x = subset(train, select=-c(Churn)), method = "rpart", trControl = numFolds, tuneGrid = cpGrid)

# Create a new CART model
ChurnTreeCV = rpart(Churn ~ ., data = train, method="class", cp = 0.005)
prp(ChurnTreeCV)

# Make predictions
PredictCV <- predict(ChurnTreeCV, newdata = test, type = "class")
z <- table(test$Churn, PredictCV)
# Compute Accuracy
table(test$Churn, PredictCV)
Accuracy = 0.7905213




