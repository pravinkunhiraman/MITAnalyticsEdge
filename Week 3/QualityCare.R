quality = read.csv("quality.csv")
str(quality)
table(quality$PoorCare)

install.packages("caTools") # Install new library to help split the data into training and testing sets
library(caTools)

set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)

split

qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)

nrow(qualityTrain)
nrow(qualityTest)

QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial) # Building a logisitc regression model 
summary(QualityLog) # Lower the value of AIC, the better

QualityLog2 = glm(PoorCare ~ OfficeVisits + Narcotics + ERVisits, data = qualityTrain, family = binomial)
summary(QualityLog2)

predictTrain = predict(QualityLog, type = "response") # Checking the quality of the model 
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)
predictTrain

QualityLog3 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog3)

table(qualityTrain$PoorCare, predictTrain > 0.2) # TO get the confusion matrix. 0.2 is the threshold in this command

install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain $PoorCare)
ROCperf = performance(ROCRpred, "tpr", "fpr")

plot(ROCperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-0.2,1.7))

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredTest = prediction(predictTest, qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredTest, "auc")@y.values)
auc
