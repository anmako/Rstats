## Linear Regression 

Y=??1+??2X+??

cars 

head(cars)

## Looking at the structure of the data 

str(cars)
summary(cars)

##Visualizing the dataset 

## Scatter plot

scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")

## Box plot

par(mfrow=c(1,2))
boxplot(cars$speed, main="Speed", sub=paste("Outlier rows:", boxplot.stats(cars$speed)$out))
boxplot(cars$dist, main="Distance", sub=paste("Outlier rows:", boxplot.stats(cars$dist)$out))

## Density plot

library(e1071)
par(mfrow=c(1,2))
plot(density(cars$speed), main = "Density Plot : Speed", ylab = "Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$speed),2)))
polygon(density(cars$speed), col = "red")

plot(density(cars$dist), main = "Density Plot : Distance", ylab = "Frequency", sub=paste("Skewness:", round(e1071::skewness(cars$dist),2)))
polygon(density(cars$dist), col = "red")


## Correlation 

cor(cars$speed, cars$dist)
cor(cars)

## Build Linear Model 

linearMod <- lm(dist ~ speed, data=cars)
print(linearMod)
summary(linearMod)

##The p-Values are very important because, We can consider a linear model to be statistically significant only when both these p-Values are less that the pre-determined statistical significance level, which is ideally 0.05. This is visually interpreted by the significance stars at the end of the row.

## t-value: We can interpret the t-value something like this. A larger t-value indicates that it is less likely that the coefficient is not equal to zero purely by chance. So, higher the t-value, the better.

## Pr(>|t|) or p-value: is the probability that you get a t-value as high or higher than the observed value when the Null Hypothesis (the ?? coefficient is equal to zero or that there is no relationship) is true. So if the Pr(>|t|) is low, the coefficients are significant (significantly different from zero). If the Pr(>|t|) is high, the coefficients are not significant.

##How to calculate the t Statistic and p-Values?

modelSummary <- summary(linearMod)
modelCoeffs <- modelSummary$coefficients
beta.estimate <- modelCoeffs["speed", "Estimate"]
std.error <- modelCoeffs["speed", "Std. Error"] 
t_value <- beta.estimate/std.error
p_value <- 2*pt(-abs(t_value), df=nrow(cars)-ncol(cars))
f_statistic <- linearMod$fstatistic[1]
f <- summary(linearMod)$fstatistic
model_p <- pf(f[1], f[2], f[3], lower=FALSE)

## AIC and BIC

AIC(linearMod)
BIC(linearMod)

## Predicting Linear Models
set.seed(100)
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))
trainingData <- cars[trainingRowIndex, ] 
testData  <- cars[-trainingRowIndex, ]

# Build the model on training data
lmMod <- lm(dist ~ speed, data=trainingData)
distPred <- predict(lmMod, testData)

summary (lmMod) 
AIC (lmMod)


## Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds)
head(actuals_preds)

## Now lets calculate the Min Max accuracy and MAPE

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals) 

## k-Fold Cross validation 

install.packages("DAAG")
library(DAAG)
cvResults <- suppressWarnings(CVlm(cars, form.lm=dist ~ speed, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))
attr(cvResults, 'ms')






