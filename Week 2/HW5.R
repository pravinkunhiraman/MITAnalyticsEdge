Elantra = read.csv("elantra.csv")
str(Elantra)
head(Elantra)
tail(Elantra)

ElantraTrain = subset(Elantra, Elantra$Year<=2012)
ElantraTest = subset(Elantra,Elantra$Year>2012)

nrow(Elantra)
nrow(ElantraTrain)
nrow(ElantraTest)

str(ElantraTrain)

SalesModel = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries , data = ElantraTrain)
summary(SalesModel)

SalesModel2 = lm(ElantraSales ~ Month + Unemployment + CPI_all + CPI_energy + Queries , data = ElantraTrain)
summary(SalesModel2)

ElantraTrain$MonthFactor = as.factor(ElantraTrain$Month)
ElantraTrain

SalesModel3 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy + Queries , data = ElantraTrain)
summary(SalesModel3)

cor(ElantraTrain$CPI_energy,ElantraTrain$CPI_all)

cor(ElantraTrain$Queries,ElantraTrain$CPI_all)

SalesModel4 = lm(ElantraSales ~ MonthFactor + Unemployment + CPI_all + CPI_energy , data = ElantraTrain)
summary(SalesModel4)

ElantraTest$MonthFactor = as.factor(ElantraTest$Month)
ElantraPredict = predict(SalesModel4,newdata = ElantraTest)

SSE = sum((ElantraPredict - ElantraTest$ElantraSales)^2)
SSE
mean(ElantraTrain$ElantraSales)

SST = sum((mean(ElantraTrain$ElantraSales) - ElantraTest$ElantraSales)^2)
R2 = 1-SSE/SST
R2

ElantraError = ElantraTest
ElantraError$SalesError = abs(ElantraPredict - ElantraTest$ElantraSales)
which.max(ElantraError$SalesError)

ElantraError
