Sys.setlocale("LC_ALL", "C")
rm(list=ls())

tweets = read.csv("tweets.csv", stringsAsFactors = FALSE)
str(tweets)

tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)

install.packages("tm")
install.packages("SnowballC")

library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]$content

corpus = tm_map(corpus,tolower)
content(corpus[[1]])
corpus = tm_map(corpus,removePunctuation)
content(corpus[[1]])
stopwords("english")[1:10]
corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
content(corpus[[1]])
corpus = tm_map(corpus,stemDocument)
corpus[[1]]$content

frequencies = DocumentTermMatrix(corpus)
frequencies
inspect(frequencies[1000:1005,505:515])

findFreqTerms(frequencies, lowfreq = 20)
sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)

trainSparse = subset(tweetsSparse, split == T)
testSparse = subset(tweetsSparse, split == F)
findFreqTerms(frequencies, lowfreq = 100)

# Building a CART Model 
library(rpart)
library(rpart.plot)

tweetCART = rpart(Negative ~ ., data = trainSparse, method = "class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata = testSparse, type = "class")
table(testSparse$Negative, predictCART)
(298+15)/nrow(testSparse)

library(randomForest)
set.seed(123)
ForestTweet = randomForest(Negative ~ ., data = trainSparse)
ForestPredict = predict(ForestTweet, newdata = testSparse)
table(testSparse$Negative, ForestPredict)
(295+21)/nrow(testSparse)

LogModel = glm(Negative ~ ., data = trainSparse, family = binomial)
LogPredict = predict(LogModel, newdata = testSparse, type = "response")

table(testSparse$Negative, LogPredict > 0.5)
(233+26)/nrow(testSparse)
