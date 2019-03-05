data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)
statedata
head(statedata)
str(statedata)
summary(statedata)

plot(statedata$x, statedata$y)

tapply(statedata$HS.Grad,statedata$state.region,mean)
tapply(statedata$Murder,statedata$state.region,mean)

?boxplot
boxplot(statedata$Murder ~ statedata$state.region)

Noreast = subset(statedata, statedata$state.region == "Northeast")
Noreast

summary(statedata$Murder)

LEModel = lm(Life.Exp ~ Population + Murder+ HS.Grad+ Frost, data = statedata)
summary(LEModel)

plot(statedata$Income, statedata$Life.Exp)

LEpredict = predict(LEModel,statedata)

LEpredict
sort(LEpredict)

which.min(statedata$Life.Exp)
statedata$state.name[40]

which.max(statedata$Life.Exp)
statedata$state.name[11]

ResidualDiff = abs(LEpredict - statedata$Life.Exp)
  
sort(ResidualDiff)
