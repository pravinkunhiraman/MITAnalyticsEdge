edges = read.csv("edges.csv")
users = read.csv("users.csv")
str(edges)
str(users)
table(users$locale)
head(edges)
V1data = table(edges$V1)
V2data = table(edges$V2)

edgesnew = as.data.frame(c(edges$V1,edges$V2))
edgesnew
str(edgesnew)
str(edges)
table(edges)
names(edgesnew) <- c("id")

counter = as.data.frame(table(edgesnew$id))
as.data.frame(table(edges$V1))
as.data.frame(table(edges$V2))
str(counter)
names(counter) <- c("id","Count")
edges$V2 == 3986
?merge

usersnew = merge(users,counter, by = "id",all.x = T)
usersnew
CounterZero = usersnew$Count
CounterZero[is.na(CounterZero)] = 0
CounterZero
usersnew$CountNew = CounterZero
usersnew
summary(usersnew$CountNew)

users
table(users$gender, users$school)
install.packages("igraph")
library(igraph)

?graph.data.frame

g = graph.data.frame(edges, FALSE, users) 
plot(g, vertex.size=5, vertex.label=NA)
table(degree(g) >=10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
which.max(V(g)$size)
V(g)$size[51]
which.min(V(g)$size)
V(g)$size[4]

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "blue"
plot(g, vertex.label=NA)
V(g)$school

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "blue"
plot(g, vertex.label=NA)
V(g)$school

users


?igraph.plotting
rglplot(g)
install.packages("manipulateWidget")
??edge.width
