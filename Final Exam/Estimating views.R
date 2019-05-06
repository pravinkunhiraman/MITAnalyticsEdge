train = read.csv("youtube_train.csv")
str(train)
summary(train)
max(train$views)
train[7890,]
which.min(train$dislikes)
train[195,]

likedvideos = subset(train, likes > 1000000)
str(likedvideos)
str(subset(likedvideos, likedvideos$comment_count > 100000))
subset(train, train$views == 225211923)

tapply(train$dislikes, train$category, sum)

mean(train$logviews)
cor(train$logviews, train$logdislikes)

test = read.csv("youtube_test.csv")

Model = lm(logviews ~ logdislikes, data = train)
summary(Model)

pred = predict(Model, newdata = test)

SSE = sum((pred - test$logviews)^2)
SST = sum((mean(train$logviews) - test$logviews)^2)

R2 = 1-SSE/SST
R2

vars = c("train$logdislikes", "train$loglikes", "train$logcomments", "train$tags", "train$publish_month", "train$trending_month")
cor(train$logcomments, train$loglikes)

NewModel = lm(logviews ~ logdislikes + tags + trending_month, data = train)
summary(NewModel)

pred.test = predict(NewModel, newdata = test)

SSE1 = sum((pred.test - test$logviews)^2)
SST1 = sum((mean(train$logviews) - test$logviews)^2)
R2 = 1-SSE1/SST1
R2
summary(Model)
log_dislikes = log(1000)
exp(8.305149 + 0.786930 * log_dislikes)

##-- Regression Tree 

library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

tree = rpart(logviews ~ logdislikes + tags + trending_month, data = train, cp = 0.05)
prp(tree)

predTree = predict(tree, newdata = test)

tree.sse = sum((predTree - test$logviews)^2)
treeR2 = 1 - tree.sse/SST
treeR2

set.seed(100)
tr.control = trainControl(method = "cv", number = 10)
cp.grid = expand.grid(.cp = (0:50)*0.0001)
(0:50)*0.0001

tr = train(logviews ~ logdislikes + tags + trending_month, data = train, method = "rpart", trControl = tr.control, tuneGrid = cp.grid)
tr

Newtree = rpart(logviews ~ logdislikes + tags + trending_month, data = train, cp = 0.0001)
newpredTree = predict(Newtree, newdata = test)
tree.sse.ew = sum((newpredTree - test$logviews)^2)
newtreeR2 = 1 - tree.sse.ew/SST
newtreeR2

library(randomForest)

set.seed(100)
Forest = randomForest(logviews ~ logdislikes + tags + trending_month, data = train, nodesize = 200, ntree = 50)
forestpred = predict(Forest, newdata = test)

forestsse = sum((forestpred - test$logviews)^2)
forestR2 = 1 - forestsse/SST
forestR2
