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

