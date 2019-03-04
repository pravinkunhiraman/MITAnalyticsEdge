# Linear regression in R. File reference: wine.csv, wine_test.csv

wine = read.csv("wine.csv")
str(wine)
summary(wine)

model1 = lm(Price ~ AGST,data = wine) # use lm function to create a linear model --> lm(Depvar ~ Indepvar, data = dataframe_tobeused)
summary(model1) # view the model details incl. R squared and adjusted R squared
model1$residuals # All residuals are available as a vector
SSE = sum(model1$residuals^2) # Sum of Squared Errors
SSE # 5.734875

model2 = lm(Price ~ AGST + HarvestRain, data=wine) # Add additional indept variables to the model using + sign
summary(model2)
SSE = sum(model2$residuals^2)
SSE # 2.970373

str(wine)
model3 = lm(Price ~ AGST+HarvestRain+WinterRain+Age+FrancePop,data=wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE

model4 = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(model4)

model5 = lm(Price ~ AGST+HarvestRain+WinterRain+Age,data=wine)
summary(model5)
SSE = sum(model5$residuals^2)
SSE

# Correlation ----

cor(wine$WinterRain, wine$Price)
cor(wine$FrancePop, wine$Age)
cor(wine)

model6 = lm(Price ~ AGST+HarvestRain+WinterRain,data=wine)
summary(model6)

wineTest = read.csv("wine_test.csv")
str(wineTest)
head(wineTest)

predictTest = predict(model5,newdata = wineTest)
predictTest

SSE = sum((wineTest$Price-predictTest)^2)
SSE
SST = sum((wineTest$Price - mean(wine$Price))^2)
SST
1-SSE/SST
