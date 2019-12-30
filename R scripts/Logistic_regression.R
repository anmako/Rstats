install.packages("ISLR")
require(ISLR)
library(ISLR)
names(Smarket)
?Smarket
pairs(Smarket, col=Smarket$Direction)
summary(Smarket)
# Logistic Regression
glm.fit <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit)
glm.probs <- predict(glm.fit,type = "response")
glm.probs[1:5]
glm.pred <- ifelse(glm.probs > 0.5, "Up", "Down")
attach(Smarket)
table(glm.pred, Direction)
mean(glm.pred== Direction)

#Make training and test set

train <- Year< 2005
glm.fit1 <- glm(Direction ~ Lag1+ Lag2+ Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial, subset = train)
glm.probs1 <- predict(glm.fit1, newdata = Smarket[!train,], type = "response")
glm.pred1 <- ifelse(glm.probs1 > 0.5, "Up", "Down")
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred1, Direction.2005)
mean(glm.pred1==Direction.2005)

# Fit smaller model 
glm.fit2 <- glm(Direction ~ Lag1+ Lag2, data = Smarket, family = binomial, subset = train)
glm.probs2 <- predict(glm.fit2, newdata = Smarket[!train,], type = "response")
glm.pred2 <- ifelse(glm.probs2 > 0.5, "Up", "Down")
Direction.2005 <- Smarket$Direction[!train]
table(glm.pred2, Direction.2005)
mean(glm.pred2==Direction.2005)

summary(glm.fit2)

#Linear Discriminant Analysis 

library(ISLR)
require(ISLR)
require(MASS)
library(MASS)


lda.fit <- lda(Direction ~ Lag1+Lag2, data = Smarket, subset = Year< 2005)
lda.fit
plot(lda.fit)
Smarket.2005 <- subset(Smarket, Year == 2005)
lda.pred <- predict(lda.fit, Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class==Smarket.2005$Direction)

## K-nearest Neighbour 

library(class)
?knn
attach(Smarket)
Xlag <- cbind(Lag1, Lag2)
train <- Year< 2005
knn.pred <- knn(Xlag[train,],Xlag[!train,], Direction[train],k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])



