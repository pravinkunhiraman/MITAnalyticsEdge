dailykos = read.csv("dailykos.csv")
str(dailykos)

# Hierarchical clustering
distance = dist(dailykos, method = "euclidean")
clusterdailykos = hclust(distance, method = "ward.D")
plot(clusterdailykos)

clusterGroups = cutree(clusterdailykos, k = 7)

cluster1 = subset(dailykos, clusterGroups == 1)
cluster2 = subset(dailykos, clusterGroups == 2)
cluster3 = subset(dailykos, clusterGroups == 3)
cluster4 = subset(dailykos, clusterGroups == 4)
cluster5 = subset(dailykos, clusterGroups == 5)
cluster6 = subset(dailykos, clusterGroups == 6)
cluster7 = subset(dailykos, clusterGroups == 7)

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# K-means clustering 
DKMatrix = as.matrix(dailykos)
DKVector = as.vector((DKMatrix))

set.seed(1000)
DKClusters = kmeans(dailykos, centers = 7)
str(DKClusters)
DKClusters$cluster

Cluster1 = subset(dailykos, DKClusters$cluster == 1)
Cluster2 = subset(dailykos, DKClusters$cluster == 2)
Cluster3 = subset(dailykos, DKClusters$cluster == 3)
Cluster4 = subset(dailykos, DKClusters$cluster == 4)
Cluster5 = subset(dailykos, DKClusters$cluster == 5)
Cluster6 = subset(dailykos, DKClusters$cluster == 6)
Cluster7 = subset(dailykos, DKClusters$cluster == 7)

tail(sort(colMeans(Cluster1)))
tail(sort(colMeans(Cluster2)))
tail(sort(colMeans(Cluster3)))
tail(sort(colMeans(Cluster4)))
tail(sort(colMeans(Cluster5)))
tail(sort(colMeans(Cluster6)))
tail(sort(colMeans(Cluster7)))

table(cluster1,Cluster1)
Cluster2
table(clusterGroups,DKClusters$cluster)
