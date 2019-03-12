parole = read.csv("parole.csv")
str(parole)
summary(parole)
table(parole$violator)

parole$crime = as.factor(parole$crime)
parole$state = as.factor(parole$state)

summary(parole)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)

Model1 = glm(violator ~ ., data=train, family = binomial)
summary(Model1)

-4.2411574+0.3869904+0.8867192+(-0.000175 * 50) + (-0.1238867 *3) + (12 *0.0802954)+(2*0.6837143) # -1.016884
odds = exp(-1.016884)
odds
prob = odds/(1+odds)
prob

str(parole)
WhiteMan = c(1,1,1,50,0,0,0,3,12,0,1,0,0)
#Consider a parolee who is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny. Answer the following questions based on the model's predictions for this individual.
Coeff = c(-4.2411574,0.3869904,0.8867192,-0.0001756,0.4433007,0.8349797,-3.3967878,-0.1238867,0.0802954,1.6119919,0.6837143,-0.2781054,-0.0117627)

SumCoeffVec = WhiteMan*Coeff
SumCoeffVec

SumofCoeff = sum(SumCoeffVec[c(1:13)])
SumCoeffVec[1] + SumCoeffVec[2] + SumCoeffVec[3] + SumCoeffVec[4] + SumCoeffVec[5] + SumCoeffVec[6] + SumCoeffVec[7] + SumCoeffVec[8] + SumCoeffVec[9] + SumCoeffVec[10] + SumCoeffVec[11] + SumCoeffVec[12] +SumCoeffVec[13]

# Log(odds) is -1.700629
odds = exp(-1.700629)
odds #0.1825687
prob = odds/(1+odds)
prob # 0.1543831


PredictOutcome = predict(Model1, newdata=test, type = "response")
which.max(PredictOutcome)
sort(table(PredictOutcome))
summary(PredictOutcome)

table(test$violator, PredictOutcome > 0.5)

12/(12+11) # Sensitivity
167/(167+12) # Specificity 
(167+12)/(167+12+11+12) # Accuracy 

table(test$violator)
179/(179+23)

library(ROCR)

ROCRpred = prediction(PredictOutcome, test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values)
