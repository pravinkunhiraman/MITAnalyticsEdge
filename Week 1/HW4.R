#file: AnonymityPoll.csv

poll = read.csv("AnonymityPoll.csv")
str(poll)
summary(poll)

table(poll$Smartphone)
nrow(is.na(poll$Smartphone))

table(poll$State, poll$Region)
table(poll$Region,poll$State)

table(poll$Internet.Use == "1",poll$Smartphone=="1")

table(is.na(poll$Internet.Use))
table(is.na(poll$Smartphone))

limited = subset(poll,poll$Internet.Use=="1" | poll$Smartphone == "1")
str(limited)
summary(limited)

table(limited$Info.On.Internet)
table(limited$Worry.About.Info=="1")
table(limited$Anonymity.Possible)
table(limited$Tried.Masking.Identity)
table(limited$Privacy.Laws.Effective)

hist(limited$Age)
max(table(limited$Age, limited$Info.On.Internet))

jitter(c(1, 2, 3))
?jitter
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
plot(limited$Age, limited$Info.On.Internet)

tapply(limited$Info.On.Internet,limited$Smartphone,mean,na.rm=TRUE)
tapply(limited$Tried.Masking.Identity,limited$Smartphone,mean,na.rm=TRUE)
