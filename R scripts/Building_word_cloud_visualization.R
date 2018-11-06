

## Visualizing Text Data Using Word Clouds

setwd("~/Desktop/github/Rstats/data")
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

## Preparing the Data

tweets$Tweet <- as.factor(tweets$Tweet)
tweets$Tweet[1]
tweets$Avg[1]

## Load tm package 

library(tm)

## Create corpus 

corpusadded <- VCorpus(VectorSource(tweets$Tweet))
corpusadded[[1]]$content

## Pre-process the data

corpusadded <- tm_map(corpusadded, content_transformer(tolower))
corpusadded <- tm_map(corpusadded, removePunctuation)
corpusadded <- tm_map(corpusadded, removeWords, stopwords("english"))

## Creating matrix 

dtmTweets <- DocumentTermMatrix(corpusadded)
dtmTweets

## Create data frame 

allTweets <- as.data.frame(as.matrix(dtmTweets))
str(allTweets)
ncol(allTweets)

## Installing word cloud package 

install.packages("wordcloud")
library(wordcloud)
colnames(allTweets)

## Building a wordcloud 

colSums(allTweets)
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, .25))


## Removing most commonly occuring word


library(tm)
library(wordcloud)
tweets <- read.csv("tweets.csv", stringsAsFactors=FALSE)
tweets$Tweet <- as.factor(tweets$Tweet)
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus < tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english")))
frequencies <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets), colSums(allTweets), scale=c(2, 0.25))

