airlines = read.csv("AirlinesCluster.csv")
str(airlines)
summary(airlines)

library(caret)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)

distances = dist(airlinesNorm, method = "euclidean")
clusterGrp = hclust(distances, method = "ward.D")
plot(clusterGrp)
rect.hclust(clusterGrp, k=3, border = "red")

clusterAir = cutree(clusterGrp, k = 5)

cluster1 = subset(airlinesNorm, clusterAir==1)
cluster2 = subset(airlinesNorm, clusterAir==2)
cluster3 = subset(airlinesNorm, clusterAir==3)
cluster4 = subset(airlinesNorm, clusterAir==4)
cluster5 = subset(airlinesNorm, clusterAir==5)

tapply(airlines$Balance, clusterAir, mean)
tapply(airlines$QualMiles, clusterAir, mean)
tapply(airlines$BonusMiles, clusterAir, mean)
tapply(airlines$BonusTrans, clusterAir, mean)
tapply(airlines$FlightMiles, clusterAir, mean)
tapply(airlines$FlightTrans, clusterAir, mean)
tapply(airlines$DaysSinceEnroll, clusterAir, mean)

set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
KMC$cluster

table(KMC$cluster)
KMC$centers
tapply(airlines$Balance, KMC$cluster, mean)
tapply(airlines$QualMiles, KMC$cluster, mean)
tapply(airlines$BonusMiles, KMC$cluster, mean)
tapply(airlines$BonusTrans, KMC$cluster, mean)
tapply(airlines$FlightMiles, KMC$cluster, mean)
tapply(airlines$FlightTrans, KMC$cluster, mean)
tapply(airlines$DaysSinceEnroll, KMC$cluster, mean)
