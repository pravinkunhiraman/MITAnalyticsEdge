wiki = read.csv("wiki.csv", stringsAsFactors = FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)

dtmAdded = DocumentTermMatrix(corpusAdded)
dtmAdded

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
sparseAdded

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
str(wordsAdded)



corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)

dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
sparseRemoved

wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal
library(caTools)
set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

train = subset(wikiWords, split == T)
test = subset(wikiWords, split == F)

table(test$Vandal)
618/nrow(test)

library(rpart)
library(rpart.plot)

CARTWiki = rpart(Vandal ~ ., data =test, method = "class")
pred = predict(CARTWiki, newdata = test)
pred
pred[,2]
pred.wiki = pred[,2]
table(test$Vandal, pred.wiki > 0.5)
(617+14)/nrow(test)

prp(CARTWiki)



wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
CART2 = rpart(Vandal ~ ., data = wikiTrain2, method = "class")
prednew = predict(CART2, newdata = wikiTest2)
pred.wiki.new = prednew[,2]
table(wikiTest2$Vandal, pred.wiki.new>0.5)
(605+64)/nrow(wikiTest2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3 = subset(wikiWords2, split==TRUE)
wikiTest3 = subset(wikiWords2, split==FALSE)
CART3 = rpart(Vandal ~ ., data = wikiTrain3, method = "class")
prednew1 = predict(CART3, newdata = wikiTest3)
pred.wiki.new1 = prednew1[,2]
table(wikiTest2$Vandal, pred.wiki.new1>0.5)
(514+248)/nrow(wikiTest3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

Train1 = subset(wikiWords3, split==TRUE)
Test1 = subset(wikiWords3, split==FALSE)
NewCART = rpart(Vandal ~ ., data = Train1, method = "class")
WikiPredict = predict(NewCART, newdata = Test1)
wiki.pred = WikiPredict[,2]
table(Test1$Vandal, wiki.pred>0.5)
(595+241)/nrow(Test1)
prp(NewCART)
