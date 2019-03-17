claims = read.csv("ClaimsData.csv")
str(claims)
table(claims$bucket2009)/nrow(claims)

library(caTools)
set.seed(88)
samples = sample.split(claims$bucket2009, SplitRatio = 0.6)

Train = subset(claims, samples == T)
Test = subset(claims, samples == F)

summary(Train)
table(Train$diabetes)
104672/( 104672+170131)

# Baseline
table(Test$bucket2009, Test$bucket2008)
(110138+10721+2774+ 1539+104)/nrow(Test)

PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
PenaltyMatrix1 = matrix(c(0,1,2,3,4,1,0,1,2,3,2,1,0,1,2,3,2,1,0,1,4,3,2,1,0), byrow=TRUE, nrow=5)

as.matrix(table(Test$bucket2009, Test$bucket2008)) * PenaltyMatrix
as.matrix(table(Test$bucket2009, Test$bucket2008)) * PenaltyMatrix1

sum(as.matrix(table(Test$bucket2009, Test$bucket2008)) * PenaltyMatrix)/nrow(Test)
sum(as.matrix(table(Test$bucket2009, Test$bucket2008)) * PenaltyMatrix1)/nrow(Test)

table(Test$bucket2009)
table(Test$bucket2008)

NewTest = Test
NewTest$Prediction = 1
as.matrix(table(NewTest$bucket2009,NewTest$Prediction))

ScoreMatrix = matrix(c(122978,0,0,0,0, 34840,0,0,0,0, 16390,0,0,0,0, 7937,0,0,0,0, 1057,0,0,0,0), byrow=TRUE, nrow=5)
122978/nrow(Test)

sum(ScoreMatrix*PenaltyMatrix)/nrow(Test)

install.packages("rpart")
install.packages("rpart.plot")
install.packages("randomForest")


library(rpart)
library(rpart.plot)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=Train, method="class", cp=0.00005)
prp(ClaimsTree)

PredictTest = predict(ClaimsTree, newdata = Test, type = "class")
table(Test$bucket2009, PredictTest)
(114141+16102+118+201)/nrow(Test)
sum(as.matrix(table(Test$bucket2009, PredictTest)) * PenaltyMatrix)/nrow(Test)

ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=Train, method="class", cp=0.00005, parms = list(loss = PenaltyMatrix))

PredictTest = predict(ClaimsTree, newdata = Test, type = "class")
table(Test$bucket2009, PredictTest)
(94310+18942+4692+636+2)/nrow(Test)
sum(as.matrix(table(Test$bucket2009, PredictTest)) * PenaltyMatrix)/nrow(Test)
