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

## Combine the two data frames into a data frame called wikiWords 

wikiWords <- cbind(wordsAdded, wordsRemoved)

# Add in the outcome variable
wikiWords$Vandal <- wiki$Vandal
str(wikiWords)

# Split the data

library(caTools)
set.seed(123)
spl <- sample.split(wikiWords$Vandal, SplitRatio = 0.7)
wikiTrain <- subset(wikiWords, spl==TRUE)
wikiTest <- subset(wikiWords, spl==FALSE)

# Build a CART model

library(rpart)
library(rpart.plot)

wikiCart <- rpart(Vandal ~., data = wikiTrain, method="class")
testPredictCart <- predict(wikiCart, newdata = wikiTest, type="class")
table(wikiTest$Vandal, testPredictCart)
(618+12)/(618+12+533+0)

prp(wikiCart)

## create a copy of dataframe from the previous question

wikiWords2 <- wikiWords
wikiWords2$HTTP <- ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

## creating training and testing sets

wikiTrain2 <- subset(wikiWords2, spl==TRUE)
wikiTest2 <- subset(wikiWords2, spl==FALSE)

wikiCart2 <- rpart(Vandal ~., data = wikiTrain2, method = "class")
testPredictCart2 <- predict(wikiCart2, newdata = wikiTest2, type ="class")
table(wikiTest2$Vandal, testPredictCart2)

## Further improving on the model

wikiWords2$NumWordsAdded <- rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved <- rowSums(as.matrix(dtmRemoved))

wikitrain1 <- subset(wikiWords2, spl== TRUE)

wikitest1 <- subset(wikiWords2, spl == FALSE)

wikicart3 <- rpart(Vandal ~ ., data = wikitrain1, method = "class")

wikitestpredict3 <- predict(wikicart3, newdata = wikitest1, type = "class")
table(wikitest1$Vandal, wikitestpredict3)

## Using non-textual data

wikiWords3 <- wikiWords2
wikiWords3$Minor <- wiki$Minor
wikiWords3$Loggedin <- wiki$Loggedin

wikiWords3train <- subset(wikiWords3, spl == TRUE)
wikiWord3test <- subset(wikiWords3, spl == FALSE)

wikiword3cart <- rpart(Vandal ~., data= wikiWords3train, method = "class")
wikiword3predict <- predict(wikiword3cart, newdata = wikiWord3test, type="class")
table(wikiWord3test$Vandal, wikiword3predict)




