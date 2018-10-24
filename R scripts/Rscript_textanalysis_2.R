## Automating Reviews in Medicine

# Loading the Data

trials <- read.csv("clinical_trial.csv", stringsAsFactors=FALSE)

summary(nchar(trials$abstract)) 
max(nchar(trials$abstract))

table(nchar(trials$abstract) == 0) 
which.min(nchar(trials$title))
trials$title[1258]

#Preparing the Corpus

library(tm)

corpusTitle <- VCorpus(VectorSource(trials$title))
corpusAbstract <- VCorpus(VectorSource(trials$abstract))

# Pre-process the data

corpusTitle <- tm_map(corpusTitle, content_transformer(tolower))
corpusAbstract <- tm_map(corpusAbstract, content_transformer(tolower))
corpusTitle <- tm_map(corpusTitle, removePunctuation)
corpusAbstract<- tm_map(corpusAbstract, removePunctuation)
corpusTitle <- tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract <- tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle <- tm_map(corpusTitle, stemDocument)
corpusAbstract <- tm_map(corpusAbstract, stemDocument)

## Look at the first Review

corpusTitle[[1]]$content

# Create matrix 

dtmTitle <- DocumentTermMatrix(corpusTitle)
dtmAbstract <- DocumentTermMatrix(corpusAbstract)
dtmTitle
dtmAbstract

# Remove Sparse Terms

dtmTitle <- removeSparseTerms(dtmTitle, 0.95)
dtmAbstract <- removeSparseTerms(dtmAbstract, 0.95)

# Create data frame 

dtmTitle <- as.data.frame(as.matrix(dtmTitle))
dtmAbstract <- as.data.frame(as.matrix(dtmAbstract))

# What is the most frequent word stem across all the abstracts?

sort(colSums(dtmAbstract))

# Building a model 

colnames(dtmTitle) <- paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <- paste0("A", colnames(dtmAbstract))

dtm <- cbind(dtmTitle, dtmAbstract)

# Add in the outcome variable

dtm$trial <- trials$trial

str(dtm)

# Split the data

library(caTools)
set.seed(144)
spl <- sample.split(dtm$trial, SplitRatio = 0.7)
dtmtrain <- subset(dtm, spl == TRUE)
dtmtest <- subset(dtm, spl== FALSE)

table(dtmtrain$trial)

# Building a model 

library(rpart)
library(rpart.plot)

trialCART <- rpart(trial ~., data = dtmtrain, method = "class")
predictTrialCART <- predict(trialCART, newdata = dtmtest, type = "class")

prp(trialCART)

predTrain <- predict(trialCart)[,2]
summary(predTrain)

table(dtmtest$trial, predictTrialCART)

# Using ROCR package

library(ROCR)









