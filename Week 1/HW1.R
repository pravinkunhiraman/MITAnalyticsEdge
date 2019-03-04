# File reference: mvtWeek1.csv

mvt = read.csv("mvtWeek1.csv")
str(mvt)
nrow(mvt)

max(mvt$ID)
min(mvt$Beat)
summary(mvt$Arrest)
summary(mvt$LocationDescription)

mvt$Date[1]

DateConvert = as.Date(strptime(mvt$Date,"%m/%d/%y %H:%M"))
DateConvert
summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Month
mvt$Weekday = weekdays(DateConvert)
mvt$Weekday

mvt$Date
mvt$Date = DateConvert

table(mvt$Month)
table(mvt$Weekday)
table(mvt$Month, mvt$Arrest)

# Plots ----

hist(mvt$Date,breaks = 100)

# boxplot(mvt$Arrest ~ mvt$Date)
boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Year,mvt$Arrest)

sort(table(mvt$LocationDescription))

?subset
Top5 = subset(mvt, LocationDescription == "STREET" | 
                LocationDescription == "PARKING LOT/GARAGE(NON.RESID.)" |
              LocationDescription == "ALLEY" |
              LocationDescription == "GAS STATION" |
              LocationDescription == "DRIVEWAY - RESIDENTIAL")

str(Top5)


summary(Top5)
Top5$LocationDescription = factor(Top5$LocationDescription)
summary(Top5)

table(Top5$LocationDescription,Top5$Arrest)

table(Top5$Weekday, Top5$LocationDescription)
