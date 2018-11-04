## Election Forecasting Revisited

# Drawing a Map of the US

setwd("~/Desktop/github/Rstats/data")
library(ggplot2)
library(ggmap)
library(RgoogleMaps)
library(maps)
state <- read.csv("statedata.csv")

statesMap <- map_data("state")
str(statesMap)

length(table(statesMap$group))

# Drawing a Map of the US

ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

# Coloring the States by Predictions

polling <- read.csv("PollingImputed.csv")
str(polling)
Test <- subset(polling, Year > 2008)
Train <- subset(polling, Year <= 2008)
mod2 <- glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction <- predict(mod2, newdata=Test, type="response")
TestPredictionBinary <- as.numeric(TestPrediction > 0.5)


predictionDataFrame <- data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(TestPredictionBinary)
mean(TestPrediction)


## Coloring the States by Predictions

predictionDataFrame$region <- tolower(predictionDataFrame$Test.State)

predictionMap <- merge(statesMap, predictionDataFrame, by = "region")

predictionMap <- predictionMap[order(predictionMap$order),]

# Coloring the States by Predictions

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary))+ geom_polygon(color = "black") + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", linetype=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", size=3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction))+ geom_polygon(color = "black", alpha=0.3) + scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

