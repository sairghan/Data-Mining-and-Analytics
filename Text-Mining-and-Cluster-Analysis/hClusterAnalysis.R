
########### Mining Text from a dataset and performing Heirarchical Analysis on documents ################
########## DATASET Source: ftp://cran.r-project.org/pub/R/web/packages/tm/##############################
########################################################################################################

############################ CHUNK 1: LOADING LIBRARIES ############################
library(tm)
library(proxy) 
library(SnowballC)


############################ CHUNK 2: DATA IMPORT ##################################

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),readerControl = list(reader = readReut21578XMLasPlain))

########################### CHUNK 3: DATA EXPORT ###################################

writeCorpus(reuters)

########################## CHUNK 4: INSPECTING CORPORA ##############################

inspect(reuters[1:2])

########################## CHUNK 5: Transformations (Preprocessing) #################


#Eliminating Extra Whitespace

reuters <- tm_map(reuters, stripWhitespace)

#Convert to Lower Case

reuters <- tm_map(reuters, content_transformer(tolower))

#Remove Stopwords

reuters <- tm_map(reuters, removeWords, stopwords("english"))

#Stemming

reuters <- tm_map(reuters, stemDocument)


########################## CHUNK 6: Creating Term-Document Matrices ###################


dtm <- DocumentTermMatrix(reuters, control=list(weighting=weightTfIdf)) 



########################## CHUNK 7: Operations on Term-Document Matrices ##############

findFreqTerms(dtm, 6)
findAssocs(dtm, "opec", 0.7)

#create a smaller dataset with // inspect(removeSparseTerms("your dataset name",0.6)) //

dtm2 <- inspect(removeSparseTerms(dtm, 0.6)) 




######################### CHUNK 8: Hierarchical Analysis ##############################


#Euclidean

plot(hclust(dist(dtm2)), main="Cluster Dendrogram: Euclidean")

#Cosine

plot(hclust(dist(dtm2, method="cosine")), main="Cluster Dendrogram: Cosine")


