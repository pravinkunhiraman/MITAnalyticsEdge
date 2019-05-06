bikes = read.csv("bikes.csv")
str(bikes)
table(bikes$season)
head(bikes)
tapply(bikes$count,bikes$season,sum)
summary(bikes$temp)
tapply(bikes$temp,bikes$demand_level,mean)

library(caTools)
set.seed(100)
split = sample.split(bikes$demand_level, SplitRatio = 0.7)

train = subset(bikes, split == T)
test = subset(bikes, split == F)

str(train)

LogModel = glm(demand_level ~ temp, data = train, family = binomial)
summary(LogModel)

log.pred = predict(LogModel, newdata = test, type = "response")
table(test$demand_level, log.pred > 0.5)
(2109+272)/nrow(test)

table(test$demand_level)
2287/nrow(test)

272/(272+707)
178/(178+2109)

NewLogModel = glm(demand_level ~ ., data = train, family = binomial)
summary(NewLogModel)
cor(bikes$temp, bikes$weather)

NewLogModel2 = glm(demand_level ~ season + holiday + workingday + weather + temp + humidity + windspeed + hour, data = train, family = binomial)
summary(NewLogModel2)
new.pred.log = predict(NewLogModel2, newdata = test, type = "response")
table(test$demand_level, new.pred.log > 0.19)
(2039 + 467)/nrow(test)

467/(467+248)
248/(248+2039)

library(ROCR)
ROCRpred = prediction(new.pred.log, test$demand_level)
plot(ROCRpred)
as.numeric(performance(ROCRpred,"auc")@y.values)
ROCperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCperf)
plot(ROCperf, colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj = c(-0.2,1.7))
