census = read.csv("census.csv")

library(caTools)
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, split == T)
test = subset(census, split == F)

LogModel = glm(over50k ~ ., data = train, family = binomial)
summary(LogModel)

# Logisitic regression model 
PredictLog = predict(LogModel, newdata = test, type = "response")
PredictLog

table(test$over50k, PredictLog > 0.5)
(9051 + 1888)/nrow(test) # Accuracy = 0.8552107

# Baseline model for test set
table(test$over50k)
9713/nrow(test) # Accuracy = 0.7593621

# CART Model
library(rpart)
library(rpart.plot)
CARTModel = rpart(over50k ~ ., data = train, method = "class")
prp(CARTModel)
PredictCART = predict(CARTModel, newdata = test, type = "class")
table(test$over50k, PredictCART)
(9243+1596)/nrow(test) # Accuracy = 0.8473927

# Calculate AUC for the CART Model
library(ROCR)
PredictROC = predict(CARTModel, newdata = test)
pred = prediction(PredictROC[,2],test$over50k)
perf = performance(pred,"tpr","fpr")
plot(perf)
as.numeric(performance(pred, "auc")@y.values)

# AUC for LogModel
PredictROC1 = predict(LogModel, newdata = test)
PredictROC1
pred1 = prediction(PredictROC1, test$over50k)
perf1 = performance(pred1, "tpr", "fpr")
plot(perf1)
as.numeric(performance(pred1, "auc")@y.values)

# Random FOrest Model
library(randomForest)
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
ForestModel = randomForest(over50k ~ ., data = trainSmall)
ForestPredict = predict(ForestModel, newdata = test)

table(test$over50k, ForestPredict)
(8843+2049)/nrow(test) # Accuracy = 0.8515362

# Finding most commonly used variables in Random Forest Model 
vu = varUsed(ForestModel, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(ForestModel$forest$xlevels[vusorted$ix]))
varImpPlot(ForestModel)

# Cross validation for the CART Model
library(caret)
library(e1071)
set.seed(2)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid( .cp = seq(0.002,0.1,0.002))
tr = train(over50k ~ ., data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

CARTModel2 = rpart(over50k ~ ., data = train, method = "class", cp = 0.002)
CARTPredict2 = predict(CARTModel2, newdata = test, type = "class")
table(test$over50k, CARTPredict2)
(9178+1838)/nrow(test) # Accuracy = 
prp(CARTModel2)
