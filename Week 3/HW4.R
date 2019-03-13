baseball = read.csv("baseball.csv")
str(baseball)
summary(baseball)

sum(table(baseball$Year, baseball$Team == 1))
table(baseball$Year, baseball$Team == 1)

head(baseball)

sum(table(baseball$Team))

length(table(baseball$Year))
2012-1962

baseball_old = baseball

baseball = subset(baseball, baseball$Playoffs == 1)
table(table(baseball$Year))

PlayoffTable = table(baseball$Year)
PlayoffTable
names(PlayoffTable)

PlayoffTable[c(1990, 2001)]
PlayoffTable["1990", "2001"]
PlayoffTable[c("1990", "2001")]

baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]
baseball

nrow(subset(baseball, NumCompetitors == 8))

baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
baseball

table(baseball$WorldSeries)
summary(baseball)

summary(glm(WorldSeries~Year, data=baseball, family="binomial")) # significant
summary(glm(WorldSeries~RS, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~RA, data=baseball, family="binomial")) # significant
summary(glm(WorldSeries~W, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~OBP, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~SLG, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~BA, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~RankSeason, data=baseball, family="binomial")) # significant
summary(glm(WorldSeries~OOBP, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~OSLG, data=baseball, family="binomial")) # NOT significant
summary(glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")) # significant
summary(glm(WorldSeries~League, data=baseball, family="binomial")) # significant

summary(glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = "binomial"))

var = c("Year","RA","RankSeason","NumCompetitors")
cor(baseball[var])

summary(glm(WorldSeries~Year, data=baseball, family="binomial")) # AIC: 232.35
summary(glm(WorldSeries~RA, data=baseball, family="binomial")) # AIC: 237.88
summary(glm(WorldSeries~RankSeason, data=baseball, family="binomial")) # AIC: 238.75
summary(glm(WorldSeries~NumCompetitors, data=baseball, family="binomial")) # AIC: 230.96

summary(glm(WorldSeries~Year+RA, data=baseball, family="binomial")) # AIC: 233.88
summary(glm(WorldSeries~Year+RankSeason, data=baseball, family="binomial")) # AIC: 233.55
summary(glm(WorldSeries~Year+NumCompetitors, data=baseball, family="binomial")) # AIC: 232.9
summary(glm(WorldSeries~RA+RankSeason, data=baseball, family="binomial")) # AIC: 238.22
summary(glm(WorldSeries~RA+NumCompetitors, data=baseball, family="binomial")) # AIC: 232.74
summary(glm(WorldSeries~RankSeason+NumCompetitors, data=baseball, family="binomial")) # AIC: 232.52
