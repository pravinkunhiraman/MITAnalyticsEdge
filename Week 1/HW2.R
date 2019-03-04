# Files: BoeingStock.csv, CocaColaStock.csv, GEStock.csv, IBMStock.csv, ProcterGambleStock.csv

IBM = read.csv("IBMStock.csv")
GE = read.csv("GEStock.csv")
ProcterGamble = read.csv("ProcterGambleStock.csv")
CocaCola = read.csv("CocaColaStock.csv")
Boeing = read.csv("BoeingStock.csv")

str(GE)
head(GE)

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

str(GE)
str(ProcterGamble)
summary(GE)
nrow(GE)
head(GE)
480/12
summary(IBM)
summary(GE)
summary(CocaCola)
summary(Boeing)
summary(ProcterGamble)
sd(ProcterGamble$StockPrice)

plot(CocaCola$Date,CocaCola$StockPrice,"l", col = "red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = "blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)

plot(CocaCola$Date[301:432],CocaCola$StockPrice[301:432],type="l",col="red",ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432],col="blue")
colors()
lines(GE$Date[301:432], GE$StockPrice[301:432],col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432],lty=2)
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432],lty=5)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)

plot(CocaCola$Date[408:432],CocaCola$StockPrice[408:432],type="l",col="red",ylim=c(0,210))
lines(ProcterGamble$Date[408:432], ProcterGamble$StockPrice[408:432],col="blue")
lines(GE$Date[408:432], GE$StockPrice[408:432],col="green")
lines(IBM$Date[408:432], IBM$StockPrice[408:432],lty=2)
lines(Boeing$Date[408:432], Boeing$StockPrice[408:432],lty=5)

tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)

tapply(GE$StockPrice, months(GE$Date), mean)
mean(GE$StockPrice)

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
mean(CocaCola$StockPrice)

months(IBM$Date)
