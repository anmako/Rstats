library("topicmodels")
library(tm)
data("AssociatedPress")

#model 1
lda_model = LDA(AssociatedPress, k=4, control=list(seed=30))
lda_topics=topics(lda_model,1)
print(as.data.frame(terms(lda_model,10)), quote=FALSE)

#model 2 - remove sparse terms
dtm = AssociatedPress 
removeSparseTerms(dtm, 0.95)
rowTotals = apply(dtm, 1, sum)
dtm = dtm[rowTotals>0,]

lda_model_2 = LDA(dtm, k=4, control=list(seed=30))
lda_topics=topics(lda_model_2,1)
print(as.data.frame(terms(lda_model_2,10)), quote=FALSE)

topicProbabilities <- as.data.frame(lda_model_2@gamma)
termProbabilities <- as.data.frame(lda_model_2@beta)

topicProbabilities[1:30,]
termProbabilities[,1:15]

#evaluate model
library(caTools)
set.seed(123)
split = sample.split(dtm, SplitRatio = 0.8)
trainData = dtm[split==TRUE,]
testData = dtm[split==FALSE,]

#model 3 - Gibbs sampling
lda_model_3 = LDA(trainData, k=4, control=list(seed=30), method='Gibbs')
perplexityVal = perplexity(lda_model_3, testData)
logLikelihood = lda_model_3@loglikelihood



