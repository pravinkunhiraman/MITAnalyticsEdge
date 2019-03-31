rm(list = ls())
flower = read.csv("flower.csv", header = FALSE)
str(flower)
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
distance = dist(flowerVector, method = "euclidean")

clusterIntensity = hclust(distance, method = "ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k=3, border = "red")
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters

tapply(flowerVector, flowerClusters, mean)
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes = F)
image(flowerMatrix, axes = F, col = grey(seq(0,1,length=256)))

healthy = read.csv("healthy.csv", header = F)
healthyMatrix = as.matrix(healthy)
image(healthyMatrix, axes = F, col = grey(seq(0,1,length = 256)))
healthyVector = as.vector((healthyMatrix))

k=5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)

healthyCluster = KMC$cluster
KMC$centers[2]

dim(healthyCluster) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyCluster, axes = F, col = rainbow(k))


KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)
KMC2$withinss


tumor = read.csv("tumor.csv",header=FALSE)
tumorMatrix = as.matrix(tumor)
tumorVector = as.vector(tumorMatrix)

install.packages("flexcust")
library(flexclust)

KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, tumorVector)
dim(tumorClusters) = c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes = F, col =rainbow(k))
