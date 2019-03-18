
data(state)
statedata = data.frame(state.x77)
str(statedata)
summary(statedata)
head(statedata)

LMModel = lm(Life.Exp ~ ., data=statedata)
summary(LMModel)

LMPredict = predict(LMModel, data=statedata)

sum((LMPredict - statedata$Life.Exp)^2)

LMModel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data=statedata)
summary(LMModel2)
LMPredict2 = predict(LMModel2, data=statedata)
sum((LMPredict2 - statedata$Life.Exp)^2)

cor(statedata)

library(rpart)
library(rpart.plot)

TreeModel = rpart(Life.Exp ~ ., data=statedata)
summary(TreeModel)
prp(TreeModel)

TreePredict = predict(TreeModel, data=statedata)
sum((TreePredict - statedata$Life.Exp)^2)

TreeModel1 = rpart(Life.Exp ~ ., data=statedata, minbucket = 5)
prp(TreeModel1)
TreePredict1 = predict(TreeModel1, data=statedata)
sum((TreePredict1 - statedata$Life.Exp)^2)

TreeModel2 = rpart(Life.Exp ~ Area, data=statedata, minbucket = 1)
prp(TreeModel2)
TreePredict2 = predict(TreeModel2, data=statedata)
sum((TreePredict2 - statedata$Life.Exp)^2)

library(caret)
library(e1071)

set.seed(111)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:10)*0.001)