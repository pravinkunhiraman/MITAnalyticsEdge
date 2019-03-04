FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
summary(FluTrain)
head(FluTrain)

hist(log(FluTrain$ILI))

plot(log(FluTrain$ILI),FluTrain$Queries)
plot(FluTrain$Queries,log(FluTrain$ILI))

FluTrend1  = lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
cor(log(FluTrain$ILI), FluTrain$Queries)

FluTest = read.csv("FluTest.csv")
?exp

log(1000)
exp(6.907755)

PredTest1 = exp(predict(FluTrend1, newdata = FluTest))
PredTest1[11] # 2.187378
FluTest$Week

?which

install.packages("zoo") # install zoo package 
library(zoo)

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE) # Create a new variable that has time series data lagging by 2 weeks
FluTrain$ILILag2 = coredata(ILILag2)
FluTrain

summary(FluTrain$ILILag2)

FluTrend2 = lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)

plot(log(FluTrain$ILILag2), log(FluTrain$ILI))

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE) # Create a new variable that has time series data lagging by 2 weeks
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

head(FluTrain)
tail(FluTrain)
nrow(FluTrain)
FluTrain$ILI[416]

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

head(FluTest)

PredTest2 = exp(predict(FluTrend2, newdata = FluTest))

SSE = sum((PredTest2-FluTest$ILI)^2)
SSE
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

SSE2 = sum((PredTest1-FluTest$ILI)^2)
SSE2
RMSE2 = sqrt(SSE2/nrow(FluTest))
RMSE2

?arima
