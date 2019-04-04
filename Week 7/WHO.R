WHO = read.csv("WHO.csv")
str(WHO)
plot(WHO$GNI, WHO$FertilityRate)

install.packages("labeling")
library(ggplot2)

scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point(color="blue",size = 3, shape = 17)
scatterplot + geom_point(color="dark red",size = 3, shape = 8)
scatterplot + geom_point(color="dark red",size = 3, shape = 8) + ggtitle("Fertility rate vs GNI")
fertilityGNIplot = scatterplot + geom_point(color="dark red",size = 3, shape = 8) + ggtitle("Fertility rate vs GNI")
pdf("MyPlot.pdf")
print(fertilityGNIplot)
dev.off()

scatterplot + geom_point(color="blue",size = 3, shape = 15)

ggplot(WHO, aes(x = GNI, y = FertilityRate, color = Region)) + geom_point()
ggplot(WHO, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) + geom_point()
ggplot(WHO, aes(x = FertilityRate, y = Under15)) + geom_point()
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point()

model = lm(Under15~log(FertilityRate), data = WHO)
summary(model)

ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm")
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level = 0.99)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = F)
ggplot(WHO, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", se = F, color = "orange")

ggplot(WHO, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette="Dark2")
