# File reference: USDA.csv

USDA = read.csv("USDA.csv")
USDA

str(USDA)
summary(USDA)

USDA$Description[which.max(USDA$Sodium)]

names(USDA) # Provides info about the labels of the variables in data frame

HighSodium = subset(USDA,Sodium>10000)
nrow(HighSodium)
HighSodium$Description
HighSodium

match("CAVIAR",USDA$Description) # Match function to search for first arg in the second arg (vector)
USDA$Sodium[4154]
USDA$Sodium[match("CAVIAR",USDA$Description)]

summary(USDA$Sodium)
sd(USDA$Sodium,na.rm=TRUE)

# Plots ----

plot(USDA$Protein,USDA$TotalFat,xlab="Protein",ylab="Fat",main = "Protein vs Fat",col="red")

hist(USDA$VitaminC,xlab="Vitamin C (mg)",main = "Histogram of Vitamin C levels",xlim=c(0,100),breaks=2000)

boxplot(USDA$Sugar,main = "Box plot of sugar levels",ylab = "Sugar (g)")

# Adding variables 

USDA$Sodium[50] > mean(USDA$Sodium,na.rm=TRUE)
HighSodium = USDA$Sodium > mean(USDA$Sodium,na.rm=TRUE)
head(HighSodium)

HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm=TRUE)) # as.numeric function converts to numeric
head(HighSodium)

USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium,na.rm=TRUE))
str(USDA)

USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat,na.rm=TRUE))
USDA$HighProtein = as.numeric(USDA$Protein > mean(USDA$Protein,na.rm=TRUE))
USDA$HighCarbs = as.numeric(USDA$Carbohydrate > mean(USDA$Carbohydrate,na.rm=TRUE))

table(USDA$HighSodium)
table(USDA$HighSodium,USDA$HighFat)

tapply(USDA$Iron, USDA$HighProtein,mean,na.rm=TRUE)
?tapply
