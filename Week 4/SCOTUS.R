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
