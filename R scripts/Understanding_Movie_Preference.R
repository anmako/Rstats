## Understanding Movie Preferences

setwd("~/Desktop/github/Rstats/data")
ratings <- read.csv("movieleens-user-genre-ratings.csv")

nrow(ratings)
str(ratings)
summary(ratings)

#Exploratory Data Analysis

cor(ratings)

##Clustering
#Remove the first column of the table using the following line of code.

points <- ratings[,2:ncol(ratings)]

# Normalize the data
library(caret)
preproc <- preProcess(points)
pointsnorm <- predict(preproc, points)

# Maximum value of adventure
max(pointsnorm$adventure)
## [1] 7.013803

# Creating a dendogram
distances <- dist(pointsnorm, method = "euclidean")
dend <- hclust(distances, method = "ward.D")
plot(dend, labels = FALSE)

set.seed(200)
# K-means clustering
set.seed(200)
kmc = kmeans(pointsnorm, centers = 5)
# Divides the dataset into 5 different subsets for each cluster
KmeansCluster1 <- subset(pointsnorm, kmc$cluster == 1)
KmeansCluster2 <- subset(pointsnorm, kmc$cluster == 2)
KmeansCluster3 <- subset(pointsnorm, kmc$cluster == 3)
KmeansCluster4 <- subset(pointsnorm, kmc$cluster == 4)
KmeansCluster5 <- subset(pointsnorm, kmc$cluster == 5)

# Output the number of observations in each cluster
nrow(KmeansCluster1)
## [1] 2084
nrow(KmeansCluster2)
## [1] 942
nrow(KmeansCluster3)
## [1] 3968
nrow(KmeansCluster4)
## [1] 748
nrow(KmeansCluster5)
## [1] 1402


##Understanding the Clusters

# Understanding the Clusters
tail(sort(colMeans(KmeansCluster1)))
tail(sort(colMeans(KmeansCluster2)))
tail(sort(colMeans(KmeansCluster3)))
tail(sort(colMeans(KmeansCluster4)))
tail(sort(colMeans(KmeansCluster5)))



