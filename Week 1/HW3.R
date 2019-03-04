# Reference file: CPSData.csv, CountryCodes.csv, MetroAreaCodes.csv

CPS = read.csv("CPSData.csv")
summary(CPS)
str(CPS)
head(CPS)

sort(table(CPS$State))

table(CPS$Citizenship)
116639+7073
123712+7590
123712/131302

table(CPS$Race, CPS$Hispanic)

summary(CPS$PeopleInHousehold)

is.na(CPS$Married)
table(CPS$Region,is.na(CPS$MetroAreaCode))
is.na(CPS$MetroAreaCode) # Returns FALSE when MetroAreaCode is present. TRUE means interviewee is in non-metro area
table(CPS$MetroAreaCode)

nrow(subset(CPS,!is.na(CPS$MetroAreaCode) & CPS$State == "Rhode Island"))
nrow(subset(CPS,CPS$State == "Rhode Island"))

mean(c(TRUE, FALSE, FALSE, TRUE,FALSE))

sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = read.csv("MetroAreaCodes.csv")
CountryMap = read.csv("CountryCodes.csv")

summary(CountryMap)
str(CountryMap)
summary(MetroAreaMap)
str(MetroAreaMap)

CPS = merge(CPS,MetroAreaMap,by.x="MetroAreaCode",by.y="Code",all.x = TRUE)

summary(CPS)

table(is.na(CPS$MetroArea))

sort(table(CPS$MetroArea))

sort(tapply(CPS$Hispanic,CPS$MetroArea,mean))

sort(tapply(CPS$Race == "Asian",CPS$MetroArea,mean))

sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,na.rm=TRUE))

CPS = merge(CPS,CountryMap,by.x="CountryOfBirthCode",by.y="Code",all.x = TRUE)
summary(CPS)

table(is.na(CPS$Country))
sort(table(CPS$Country))

table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
# tapply(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA",CPS$Country,mean,na.rm=TRUE)
tapply(CPS$Country != "United States", CPS$MetroArea,mean,na.rm=TRUE)

NYInterviwees = subset(CPS,CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA")
nrow(NYInterviwees)

1-0.6913397

table(CPS$Country, CPS$MetroArea)

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea,sum,na.rm=TRUE))
