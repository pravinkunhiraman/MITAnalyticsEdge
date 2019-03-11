# File: songs.csv
# rm(list=ls())
songs = read.csv("songs.csv")
str(songs)
summary(songs)

nrow(subset(songs, year == 2010))
table(songs$year)

table(songs$artistname)
MJ = subset(songs, artistname == "Michael Jackson")
MJ
subset(songs, artistname == "Michael Jackson" & Top10 == 1)

table(songs$timesignature)
sort(table(songs$tempo))
which.max(songs$tempo)
songs[6206,]

# Regression model

Train = subset(songs, year <= 2009)
Test = subset(songs, year == 2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = Train[ , !(names(Train) %in% nonvars) ]
SongsTest = Test[,!(names(Test) %in% nonvars)]

head(SongsTest)
head(Test)

Model1 = glm(Top10 ~ ., SongsTrain, family = binomial)
summary(Model1)

cor(SongsTrain$loudness, SongsTrain$energy)

Model2 = glm(Top10 ~ . - loudness, SongsTrain, family = binomial)
summary(Model2)

Model3 = glm(Top10 ~ . - energy, SongsTrain, family = binomial)
summary(Model3)

PredictSong = predict(Model3, newdata=SongsTest, type = "response")
table(SongsTest$Top10, PredictSong > 0.45)

table(SongsTest$Top10)
sensitivity = 19/(19+40)
sensitivity
specificity = 309/(309+5)
specificity
