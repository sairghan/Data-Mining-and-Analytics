##LOADING LIBRARIES##
library(isa2)
library(SnowballC)
library (tm)
#
#  read in five articles: everything in texts folder
#  
one=Corpus(DirSource("C:/Users/sag163/Desktop/texts"))
summary(one)
#
#  do some transformations
#
two=tm_map(one,tolower)
two=tm_map(two,removeNumbers)
two=tm_map(two,removePunctuation)
two=tm_map(two,stripWhitespace)
two=tm_map(two,removeWords,stopwords("english"))
two=tm_map(two,stemDocument,language="english")
two=tm_map(two,PlainTextDocument)  # fix error caused by tolower
#
#  get tfidf matrix
#
three=DocumentTermMatrix(one,control=list(weighting=weightTfIdf))
three
#
# hclust
#
four=as.matrix(three,ncols=5)
five=dist(four)
par(mar=c(1,1,1,1))
plot(hclust(five))
#
# try with proxy and cosine
#
library(proxy)
fiveb=data.frame(four)
six=dist(fiveb,method="cosine")
plot(hclust(six))
