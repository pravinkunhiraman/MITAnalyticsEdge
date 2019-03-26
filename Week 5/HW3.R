emails = read.csv("emails.csv",stringsAsFactors = FALSE)
str(emails)
table(emails$spam)

which.max(nchar(emails$text))
nchar(emails$text[2651])
which.min(nchar(emails$text))
nchar(emails$text[1992])

library(tm)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
dtm

spdtm = removeSparseTerms(dtm, 0.95)
spdtm

emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))
emailsSparse$spam = emails$spam
table(colSums(emailsSparse)>=5000)
table(emailsSparse$spam)
table(colSums(subset(emailsSparse, spam == 0))>=5000)
sort(colSums(subset(emailsSparse, spam==0)))
sort(colSums(subset(emailsSparse, spam == 1))>=1000)

emailsSparse$spam = as.factor(emailsSparse$spam)

library(caTools)
set.seed(123)
sample = sample.split(emailsSparse$spam, SplitRatio = 0.7)
train = subset(emailsSparse, sample == T)
test = subset(emailsSparse, sample == F)

library(rpart)
library(rpart.plot)
library(randomForest)

spamLog = glm(spam ~ ., data=train, family = binomial)
spamCART = rpart(spam ~ ., data=train, method = "class")
set.seed(123)
spamRF = randomForest(spam ~ ., data=train)

predictLog = predict(spamLog, data = train, type = "response")
predictCART = predict(spamCART, data = train)
predictRF = predict(spamRF, data = train, type = "prob")

table(predictLog<0.00001)
table(predictLog<0.99999 & predictLog>0.00001)
summary(spamLog)
prp(spamCART)

table(train$spam, predictLog>0.5)
(3052+954)/nrow(train)

library(ROCR)
ROCRpred = prediction(predictLog, train$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)

table(train$spam, predictCART[,2]>0.5)
(2885+894)/nrow(train)

ROCRpred = prediction(predictCART[,2], train$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)

pred.RF = predictRF[,2]
table(train$spam, pred.RF>0.5)
(3017+916)/nrow(train)
ROCRpred = prediction(pred.RF, train$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)


predictLog = predict(spamLog, newdata = test, type = "response")
predictCART = predict(spamCART, newdata = test)
predictRF = predict(spamRF, newdata = test, type = "prob")

table(test$spam, predictLog>0.5)
(1257+376)/nrow(test)
ROCRpred = prediction(predictLog, test$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)

table(test$spam, predictCART[,2]>0.5)
(1228+386)/nrow(test)
ROCRpred = prediction(predictCART[,2], test$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)

pred.RF = predictRF[,2]
table(test$spam, pred.RF>0.5)
(1291+385)/nrow(test)
ROCRpred = prediction(pred.RF, test$spam)
as.numeric(performance(ROCRpred,"auc")@y.values)
