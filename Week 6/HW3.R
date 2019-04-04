rm(list=ls())
stocks = read.csv("StocksCluster.csv")
str(stocks)
summary(stocks)
table(stocks$PositiveDec)
6324/nrow(stocks)
cor(stocks)
cor(stocks$ReturnJan, stocks$ReturnFeb) #0.06677458
cor(stocks$ReturnJan, stocks$ReturnMar) #0.0904968
cor(stocks$ReturnJan, stocks$ReturnApr) #0.03767801
cor(stocks$ReturnJan, stocks$ReturnMay) #0.04441142
cor(stocks$ReturnJan, stocks$ReturnJune) #0.09223831
cor(stocks$ReturnJan, stocks$ReturnJuly) #0.08142976
cor(stocks$ReturnJan, stocks$ReturnAug) #0.02279202
cor(stocks$ReturnJan, stocks$ReturnSep) #0.02643715
cor(stocks$ReturnJan, stocks$ReturnOct) #0.1429772
cor(stocks$ReturnJan, stocks$ReturnNov) #0.06763233

cor(stocks$ReturnFeb, stocks$ReturnMar) #0.1559833
cor(stocks$ReturnFeb, stocks$ReturnApr) #0.1913519
cor(stocks$ReturnFeb, stocks$ReturnMay) #0.09552092
cor(stocks$ReturnFeb, stocks$ReturnJune) #0.1699945
cor(stocks$ReturnFeb, stocks$ReturnJuly) #0.06177851
cor(stocks$ReturnFeb, stocks$ReturnAug) #0.1315598
cor(stocks$ReturnFeb, stocks$ReturnSep) #0.04350177
cor(stocks$ReturnFeb, stocks$ReturnOct) #0.08732427
cor(stocks$ReturnFeb, stocks$ReturnNov) #0.1546583

cor(stocks$ReturnMar, stocks$ReturnApr) #0.009726288
cor(stocks$ReturnMar, stocks$ReturnMay) #0.003892789
cor(stocks$ReturnMar, stocks$ReturnJune) #0.08590549
cor(stocks$ReturnMar, stocks$ReturnJuly) #0.00337416
cor(stocks$ReturnMar, stocks$ReturnAug) #0.0220054
cor(stocks$ReturnMar, stocks$ReturnSep) #0.07651833
cor(stocks$ReturnMar, stocks$ReturnOct) #0.01192376
cor(stocks$ReturnMar, stocks$ReturnNov) #0.03732353

library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~ ., data = stocksTrain, family = binomial)
PredTrain = predict(StocksModel, type = "response")
table(stocksTrain$PositiveDec, PredTrain > 0.5)
(990+3640)/nrow(stocksTrain)

PredTest = predict(StocksModel, newdata = stocksTest, type = "response")
table(stocksTest$PositiveDec, PredTest > 0.5)
(417+1553)/nrow(stocksTest)


limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

summary(normTrain)
summary(normTest)

set.seed(144)
km = kmeans(normTrain, centers = 3)
km$cluster
table(km$cluster)

install.packages("flexclust")
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
table(clusterTest)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)
stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)
str(stocksTrain1)

summary(stocksTrain1)
summary(stocksTrain2)
summary(stocksTrain3)

StocksModel1 = glm(PositiveDec ~ ., data = stocksTrain1, family = binomial)
StocksModel2 = glm(PositiveDec ~ ., data = stocksTrain2, family = binomial)
StocksModel3 = glm(PositiveDec ~ ., data = stocksTrain3, family = binomial)

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 = predict(StocksModel1, newdata = stocksTest1, type = "response")
PredictTest2 = predict(StocksModel2, newdata = stocksTest2, type = "response")
PredictTest3 = predict(StocksModel3, newdata = stocksTest3, type = "response")

table(stocksTest1$PositiveDec, PredictTest1 > 0.5)
(30+774)/nrow(stocksTest1)
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757)/nrow(stocksTest2)
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/nrow(stocksTest3)

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
table(AllOutcomes, AllPredictions > 0.5)
(467+1544)/(467+1544+1110+353)
