#File: loans.csv

loans = read.csv("loans.csv")
str(loans)
summary(loans)

table(loans$not.fully.paid)
1533/(8045+1533)

NAValues = subset(loans, is.na(loans$log.annual.inc) | is.na(loans$days.with.cr.line)| is.na(loans$revol.util)| is.na(loans$inq.last.6mths) | is.na(loans$delinq.2yrs) | is.na(loans$pub.rec))
summary(NAValues)
table(NAValues$not.fully.paid)
12/(50+12)

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
summary(loans)

# Split to Training and Test
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid, SplitRatio = 0.7)
train = subset(loans, split == T)
test = subset(loans, split == F)

Model1 = glm(not.fully.paid ~ ., data=train, family = binomial)
summary(Model1)
(-9.294e-03* 700) - (-9.294e-03 * 710)
exp(0.09294)

predicted.risk = predict(Model1, newdata = test, type = "response")
test$predicted.risk = predicted.risk
summary(test)

table(test$not.fully.paid, predicted.risk > 0.5)
table(test$not.fully.paid, test$predicted.risk > 0.5)

#Accuracy
(2400+3)/(2400+13+457+3)

#Baseline model
table(test$not.fully.paid)
2413/(2413+460)

library(ROCR)
ROCRpred = prediction(test$predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred,"auc")@y.values)

summary(test)
Model2 = glm( not.fully.paid ~ int.rate, data=train, family=binomial)
summary(Model2)

cor(test)
cor(test$int.rate,test$fico,test$credit.policy,test$purpose)
vars = c("int.rate","installment","pub.rec","fico","days.with.cr.line")
cor(test[vars])

NewPrediction = predict(Model2, newdata = test, type = "response")
summary(NewPrediction)
table(test$not.fully.paid, NewPrediction > 0.5)
nrow(test)
ROCRpred2 = prediction(NewPrediction, test$not.fully.paid)
as.numeric(performance(ROCRpred2,"auc")@y.values)

# Investment
10 * exp(0.06*3)
?exp

# Profitability

test$profit = exp(test$int.rate * 3) - 1
test$profit[test$not.fully.paid == 1] = -1
sort(table(test$profit))

which.max(test$profit)
test$profit[1780]
test[1780,]
0.8894769 * 10

HighInterest = subset(test, test$int.rate > 0.15)
summary(HighInterest)
table(HighInterest$not.fully.paid)
110/(110+327)

cutoff = sort(HighInterest$predicted.risk, decreasing=FALSE)[100]
cutoff

selectedLoans = subset(HighInterest, HighInterest$predicted.risk<=cutoff)
sum(c(1:10))
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
