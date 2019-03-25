rm(list=ls())
trial = read.csv("clinical_trial.csv", stringsAsFactors = FALSE)
str(trial)
summary(trial)

which.max(nchar(trial$abstract))
nchar(trial$abstract[664])
sum(nchar(trial$abstract)==0)
which.min(nchar(trial$title))
trial$title[1258]

install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)
corpusTitle = Corpus(VectorSource(trial$title))
corpusAbstract = Corpus(VectorSource(trial$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

sparseTitle = removeSparseTerms(dtmTitle,0.95)
sparseAbstract = removeSparseTerms(dtmAbstract,0.95)

sparseTitle
sparseAbstract

Title = as.data.frame(as.matrix(sparseTitle))
Abstract = as.data.frame(as.matrix(sparseAbstract))

colSums(Title)
sort(colSums(Abstract))
colnames(Title) = paste0("T", colnames(Title))
colnames(Abstract) = paste0("A", colnames(Abstract))

dtm = cbind(Title,Abstract)
dtm$trial = trial$trial
str(dtm)

library(caTools)
set.seed(144)
split = sample.split(dtm$trial, SplitRatio = 0.7)
train = subset(dtm, split == T)
test = subset(dtm, split == F)

table(train$trial)
730/nrow(train)

library(rpart)
library(rpart.plot)

trialCART = rpart(trial ~ ., data = train, method = "class")
prp(trialCART)
trainpred = predict(trialCART, data = train)
trainpred
pred.new = trainpred[,2]
max(pred.new)

table(train$trial, pred.new>0.5)
(631+441)/nrow(train)
441/(441+131)
631/(99+631)

pred.test = predict(trialCART, newdata = test)[,2]
table(test$trial, pred.test > 0.1)
(261+162)/nrow(test)

library(ROCR)
ROCRpred = prediction(pred.test, test$trial)
as.numeric(performance(ROCRpred, "auc")@y.values)
