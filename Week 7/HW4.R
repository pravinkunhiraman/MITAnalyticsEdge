rm(list=ls())
parole = read.csv("parole.csv")
str(parole)
table(parole$male, parole$violator)
14/(14+64)

table(subset(parole, parole$state == 2)$crime)
library(ggplot2)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0, color = 'black', fill = 'cornflowerblue')
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0, fill = 'cornflowerblue', color = "black")
?geom_histogram
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0) + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5, boundary = 0) + facet_grid(. ~ male)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, boundary = 0)

colorPalette = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, boundary = 0) + scale_fill_manual(values=colorPalette)

ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5, boundary = 0, position = "identity", alpha=0.5) + scale_fill_manual(values=colorPalette)

?scale_fill_manual
