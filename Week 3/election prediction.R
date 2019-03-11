polling = read.csv("PollingData.csv")
str(polling)
table(polling$Year)

summary(polling)

install.packages("mice")
library("mice")

simple = polling[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
simple

set.seed(144)
imputed = complete(mice(simple))
imputed_edx = read.csv("PollingData_Imputed.csv")

polling$Rasmussen = imputed$Rasmussen
polling$SurveyUSA = imputed$SurveyUSA

polling$Rasmussen = imputed_edx$Rasmussen
polling$SurveyUSA = imputed_edx$SurveyUSA

summary(polling)

Train = subset(polling, Year == 2004 | Year == 2008)
Test = subset(polling, Year == 2012)
str(Test)

table(Train$Republican) # Baseline model
table(sign(Train$Rasmussen)) # Smart baseline model based on Rasmussen poll results
table(Train$Republican, sign(Train$Rasmussen)) #Final model

cor(Train[c("Rasmussen", "SurveyUSA","DiffCount","PropR","Republican")])

mod1 = glm(Republican ~ PropR, data = Train, family=binomial)
summary(mod1) # AIC = 19.772

pred1 = predict(mod1, type = "response")
table(Train$Republican,pred1>=0.5)

mod2 = glm(Republican ~ SurveyUSA + DiffCount, data = Train, family=binomial)
summary(mod2) # AIC = 21.374

pred2 = predict(mod2, type = "response")
table(Train$Republican,pred2>=0.5)

# Working with the Testing set

table(Test$Republican, sign(Test$Rasmussen))
TestPrediction = predict(mod2, newdata = Test, type = "response")
table(Test$Republican, TestPrediction > 0.5)
subset(Test, TestPrediction > 0.5 & Republican == 0)
