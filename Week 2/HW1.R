ClimateChange = read.csv("climate_change.csv")
str(ClimateChange)
plot(ClimateChange$Year, ClimateChange$CO2)

ClimateTrain = subset(ClimateChange,ClimateChange$Year<=2006)
str(ClimateTrain)

ClimateTest = subset(ClimateChange,ClimateChange$Year>2006)
str(ClimateTest)

ClimateModel = lm(Temp ~  MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=ClimateTrain)
summary(ClimateModel)

ClimateModel2 = lm(Temp ~  MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,data=ClimateTrain)
summary(ClimateModel2)

cor(ClimateTrain)

ClimateModel3 = lm(Temp ~  MEI +N2O + TSI + Aerosols,data=ClimateTrain)
summary(ClimateModel3)

?step
step(ClimateModel)

ClimateModel4 = lm(formula = Temp ~ MEI + CO2 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = ClimateTrain)
summary(ClimateModel4)

ClimatePredict = predict(ClimateModel,ClimateTest)
ClimatePredict

SSE = sum((ClimatePredict - ClimateTest$Temp)^2)
SST = sum((mean(ClimateTrain$Temp) - ClimateTest$Temp)^2)
R2 = 1 - SSE/SST
R2
