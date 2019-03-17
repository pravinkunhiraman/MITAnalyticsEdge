# File: stevens.csv

stevens = read.csv("stevens.csv")
str(stevens)
summary(stevens)
table(stevens$LowerCourt, stevens$Reverse)

library(caTools)
set.seed(3000)
split = sample.split(stevens$Reverse, SplitRatio = 0.7)

Train = subset(stevens, split == T)
Test = subset(stevens, split == F)

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

#Build model
StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 25)

prp(StevensTree)

PredictCart = predict(StevensTree, newdata = Test, type ="class")
PredictCart

table(Test$Reverse, PredictCart)

library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC

pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred,"tpr","fpr")

plot(perf)

as.numeric(performance(pred, "auc")@y.values)

StevensTree1 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 5)
prp(StevensTree1)

StevensTree2 = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket = 100)
prp(StevensTree2)

install.packages("randomForest")
library("randomForest")

set.seed(200)
StevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

PredictForest = predict(StevensForest, newdata = Test)
table(Test$Reverse, PredictForest) # Confusion matrix
(46+74)/(46+74+19+31)
(43+75)/(43+75+18+34)
install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

numFolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp = seq(0.01,0.5,0.01))
train(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method="rpart",trControl = numFolds, tuneGrid = cpGrid)

StevensTreeCV = rpart(Reverse~Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp=0.18)
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")

table(Test$Reverse, PredictCV)
prp(StevensTreeCV)
