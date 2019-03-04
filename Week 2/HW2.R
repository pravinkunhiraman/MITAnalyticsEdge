pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")

str(pisaTrain)
str(pisaTest)

tapply(pisaTrain$readingScore, pisaTrain$male, mean)

summary(pisaTrain)
levels(pisaTrain$raceeth)

pisaTrain = na.omit(pisaTrain) # Omit observations with na values in them 
str(pisaTrain)

pisaTest = na.omit(pisaTest)
str(pisaTest)

table(pisaTrain$raceeth)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White") # Modify factor value to the value in the second function arg
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)

predTest = predict(lmScore, pisaTest)
summary(predTest)

SSE = sum((predTest - pisaTest$readingScore)^2)
SSE
RMSE = sqrt(SSE/nrow(pisaTest))
nrow(pisaTest)
RMSE

mean(pisaTrain$readingScore)
SST = sum((mean(pisaTrain$readingScore)-pisaTest$readingScore)^2)
SST

R2 = 1-SSE/SST
R2
