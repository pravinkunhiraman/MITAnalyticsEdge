letters = read.csv("letters_ABPR.csv")
str(letters)
summary(letters)
letters$isB = as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = 0.5)
Train = subset(letters, split == TRUE)
Test = subset(letters, split==F)

# Baseline model -- Not B
table(Test$isB)
1175/(1175+383) # Accuracy = 0.754172

# CART Model
library(rpart)
library(rpart.plot)

CARTb = rpart(isB ~ . - letter, data= Train, method = "class")
prp(CARTb)
PredictB = predict(CARTb, newdata = Test, type = "class")

table(Test$isB, PredictB)
(340 + 1118)/(340+1118+57+43) # Accuracy = 0.9358151

# Random Forest model
library("randomForest")
set.seed(1000)
ForestB = randomForest(isB ~ . - letter, data= Train)
PredictForestB = predict(ForestB, newdata = Test)

table(Test$isB, PredictForestB)
(1163 + 374)/(1163 + 374 + 12 + 9) # Accuracy = 0.9865212

# Multiple classification model 

letters$letter = as.factor( letters$letter )
set.seed(2000)
split1 = sample.split(letters$letter, SplitRatio = 0.5)

TrainNew = subset(letters, split1 == T)
TestNew = subset(letters, split1 == F)

table(TestNew$letter) # baseline prediction is the most common occurrence - P
401/nrow(TestNew) # Accuracy = 0.2573813

CARTLetter = rpart(letter ~ . - isB, data = TrainNew, method = "class") # CART Model
prp(CARTLetter)
CARTLetterPredict = predict(CARTLetter, newdata = TestNew, type = "class")

table(TestNew$letter, CARTLetterPredict)
(348 + 318 + 363 + 340)/nrow(TestNew) # Accuracy = 0.8786906

library(randomForest)

ForestLetter = randomForest(letter ~ . - isB, data = TrainNew)
ForestPredict = predict(ForestLetter, newdata = TestNew)

table(TestNew$letter, ForestPredict)
(390 + 380 + 394 + 361)/nrow(TestNew)
