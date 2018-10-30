## Airlines Cluster 

## loading the data in R

setwd("~/Desktop/github/Rstats/data")
airlines <- read.csv("AirlinesCluster.csv")
str(airlines)

## Looking at the summary of the data

summary(airlines)

## Normalizing the data 

library(caret)
preproc <- preProcess(airlines)
airlinesNorm <- predict(preproc, airlines)
summary(airlinesNorm)

## Hierarchical Clustering

distanceAirlines <- dist(airlinesNorm, method = "euclidean")
AirlinesClust <- hclust(distanceAirlines, method = "ward.D")
plot(AirlinesClust)
clusterGroups <- cutree(AirlinesClust, k=5)
table(clusterGroups)

tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)

## K-Means Clustering 

set.seed(88)
kmeansClust <- kmeans(airlinesNorm, centers=5, iter.max=1000)
table(kmeansClust$cluster)







