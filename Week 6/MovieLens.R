movies = read.table("movieLens.txt",header = FALSE, sep = "|", quote = "\"")
str(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unknown", "Action", "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance == 1 & movies$Drama == 1)

distances = dist(movies[2:20],method = "euclidean")
clusterMovies = hclust(distances, method = "ward.D")
plot(clusterMovies)

clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)

subset(movies, Title == "Men in Black (1997)")
clusterGroups[257]

cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

colMeans(subset(movies[2:20], clusterGroups == 1))

spl = split(movies[2:20], clusterGroups)
spl[[1]]
colMeans(spl[[1]])
lapply(spl, colMeans)

movies[2:20]


clusterGroups2 = cutree(clusterMovies, k = 2)
spl2 = split(movies[2:20], clusterGroups2)
lapply(spl2, colMeans)
