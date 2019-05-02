baseball = read.csv("baseball.csv")
str(baseball)

moneyball = subset(baseball, Year < 2012)
str(moneyball)
table(moneyball$Year)
moneyball$RD = moneyball$RS - moneyball$RA

LinModel = lm(W ~ RD, data = moneyball)
summary(LinModel)
 pred = predict(LinModel, data = moneyball)
SSE = sum((pred-moneyball$W)^2)
