##Objective
###Our objective is to build ML/ANN model to predict which transactions could be 
###fraudulent, with high accuracy.

##DataSet
#The datasets contains transactions made by credit cards.
#This dataset presents transactions that occurred in two days, where we have 492 frauds out of 
#284,807 transactions. The dataset is highly unbalanced, the positive class (frauds) account for 
#0.172% of all transactions.

#It contains only numerical input variables which are the result of a PCA transformation. 
#Unfortunately, due to confidentiality issues, the original features are not given.
#Features V1, V2, . V28 are the principal components obtained with PCA, the only features 
#which have not been transformed with PCA are 'Time' and 'Amount'. 
#Feature 'Time' contains the seconds elapsed between each transaction and the first transaction in 
#the dataset. The feature 'Amount' is the transaction Amount, this feature can be used for 
#example-dependant cost-senstive learning. Feature 'Class' is the response variable and it takes 
#value 1 in case of fraud and 0 otherwise.

#Given the class imbalance ratio, we recommend measuring the accuracy using 
#the Area Under the Precision-Recall Curve (AUPRC). 
#Confusion matrix accuracy may not be meaningful for unbalanced classification.

###Need to install few packages

install.packages("rpart")
install.packages("ROSE")
install.packages("rattle")
install.packages("caret")
install.packages("mlbench")
install.packages("stringi")
install.packages("caretEnsemble")
install.packages("tidyverse")

library(rpart)
library(ROSE)
library(rattle)
library(caret)
library(mlbench)
library(stringi)
library(caretEnsemble)
library(tidyverse)


#Data Preparation

DataPath<-"C:\\Sandip\\Training\\Workshop\\Hands On\\Data\\creditcard.csv"

mydata <- read.csv(DataPath)
#mydata$Class[mydata$Class==1]<- "Yes"
#mydata$Class[mydata$Class==0]<- "No"
#mydata$Class <- as.factor(mydata$Class)
training_size <- floor(0.80 * nrow(mydata))
train_ind <- sample(seq_len(nrow(mydata)), size = training_size)
training <- mydata[train_ind, ]
testing <- mydata[-train_ind, ]

##Model Building

#CART Model Performance on imbalanced data
fit1 <- rpart(Class~., data = training)
pred.fit1 <- predict(fit1, newdata = testing)
accuracy.meas(testing$Class, pred.fit1[,2])

#We therefore scale and split the data:(Optional)
maxs <- apply(mydata[,-31], 2, max) 
mins <- apply(mydata[,-31], 2, min)
scaled <- as.data.frame(scale(mydata[,-31], center = mins, scale = maxs - mins))
scaled$Class<-as.vector(mydata$Class)
train_ <- scaled[index,]
test_ <- scaled[-index,]


## ANN Model

library(neuralnet)
n <- names(training)
f <- as.formula(paste("Class ~", paste(n[!n %in% "Class"], collapse = " + ")))
nn_fraud <- neuralnet(f,data=training,hidden=c(5,3),act.fct = "logistic",linear.output=F)

plot(nn_fraud)

#Prediction

pr.nn <- compute(nn_fraud,testing[,1:30])
pred<-pr.nn$net.result

##Undersample

data_balanced_under <- ovun.sample(Class ~ ., data = training, method = "under", N = 806, seed = 1)$data
table(data_balanced_under$Class)
nn_fraud_under <- neuralnet(f,data=data_balanced_under,hidden=c(5,3),act.fct = "logistic",linear.output=F)
plot(nn_fraud_under)

#Prediction

pr.nn_under <- compute(nn_fraud_under,testing[,1:30])
pred_under<-pr.nn$net.result

###Both
data_balanced_both <- ovun.sample(Class ~ ., data = training, method = "both", p=0.5, N=10000, seed = 1)$data
table(data_balanced_both$Class)
nn_fraud_both <- neuralnet(f,data=data_balanced_both,hidden=c(5,3),act.fct = "logistic",linear.output=F)
plot(nn_fraud_both)
hist(unlist(nn_fraud_both$net.result))


###Exercise for Student#####

###Build NN with 3 hidden layers with 2 hidden units ### 

