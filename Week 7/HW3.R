tweets = read.csv("tweets.csv",stringsAsFactors = F)
str(tweets)
library(tm)

corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,stopwords("english"))
corpus$content
dtm = DocumentTermMatrix(corpus)

newtweets = as.data.frame(as.matrix(dtm))
str(newtweets)

install.packages("wordcloud")
library(wordcloud)
?wordcloud
colnames(newtweets)
colSums(newtweets)

wordcloud(colnames(newtweets),colSums(newtweets), scale=c(2,0.25))

corpus = tm_map(corpus,removeWords,c(stopwords("english"),"apple"))
dtm = DocumentTermMatrix(corpus)
newtweets = as.data.frame(as.matrix(dtm))
wordcloud(colnames(newtweets),colSums(newtweets), scale=c(4,0.5))

NegTweets = subset(tweets, Avg <=-1)
str(NegTweets)
corpus1 = Corpus(VectorSource(NegTweets$Tweet))
corpus1 = tm_map(corpus1,tolower)
corpus1 = tm_map(corpus1,removePunctuation)
corpus1 = tm_map(corpus1,removeWords,c(stopwords("english"),"apple"))
dtm1 = DocumentTermMatrix(corpus1)
newtweets1 = as.data.frame(as.matrix(dtm1))
wordcloud(colnames(newtweets1),colSums(newtweets1), scale=c(4,0.5),random.color = T)
wordcloud(colnames(newtweets),colSums(newtweets), scale=c(4,0.5), random.color = T,colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
wordcloud(colnames(newtweets),colSums(newtweets), scale=c(4,0.5), random.color = T,colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
brewer.pal(9, "Blues")[c(-5, -6, -7, -8, -9)]
?colors

install.packages("RColorBrewer")
library(RColorBrewer)
brewer.pal()
display.brewer.all()
