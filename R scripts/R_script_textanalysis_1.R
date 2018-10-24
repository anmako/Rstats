## Exercise 

# Load the dataset

setwd("~/Desktop/github/Rstats/data")

wiki <- read.csv("wiki.csv", stringsAsFactors = FALSE)

str(wiki)

# Converting Vandal to a factor 

wiki$Vandal <- as.factor(wiki$Vandal)

# Look at wiki

wiki$X[1]
wiki$Minor[1]
wiki$Added[1]
wiki$Removed[1]

# Load tm package

library(tm)

# Create corpus

corpusAdded <- VCorpus(VectorSource(wiki$Added))

corpusAdded[[1]]$content

# Pre-process data

corpusAdded <- tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded <- tm_map(corpusAdded, removeWords, sw)
corpusAdded <- tm_map(corpusAdded, stemDocument)

# Create matrix

dtmAdded <- DocumentTermMatrix(corpusAdded)
dtmAdded

# Remove sparse terms

sparseAdded <- removeSparseTerms(dtmAdded, 0.997)
sparseAdded

# Create data frame

wordsAdded <- as.data.frame(as.matrix(sparseAdded))

colnames(wordsAdded) = paste("A", colnames(wordsAdded))

## working on word R

corpusRemoved <- VCorpus(VectorSource(wiki$Removed))
corpusRemoved <- tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved <- tm_map(corpusRemoved, stemDocument)
dtmRemoved <- DocumentTermMatrix(corpusRemoved)
sparseRemoved <- removeSparseTerms(dtmRemoved, 0.997)
wordsRemoved <- as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) <- paste("R", colnames(wordsRemoved))



