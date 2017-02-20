#  Install the packages you need to mine text   
#  You only need to do this step once.   

Needed <- c("tm", "SnowballCC", "RColorBrewer", "ggplot2", "wordcloud", "biclust", "cluster", "igraph", "fpc")   
install.packages(Needed, dependencies=TRUE)   

install.packages("Rcampdf", repos = "http://datacube.wu.ac.at/", type = "source")

##########################################################################################
#                                  Loading Texts                                         #
##########################################################################################      

# On a Mac:

cname <- file.path("~", "Desktop", "texts")   
cname   
dir(cname)   # Use this to check whether the texts are loaded.   

#########################################################################################

  
# On a PC:  

cname <- file.path("C:", "texts")   
cname   
dir(cname)   
##########################################################################################

##########################################################################################
#                                Start Your Analyses                                     #
##########################################################################################

library(tm)   ##  R package for text mining.
docs <- Corpus(DirSource(cname))   

## Preprocessing 

docs <- tm_map(docs, removePunctuation)   # Removing punctuation:   
docs <- tm_map(docs, removeNumbers)      # Removing numbers:   
docs <- tm_map(docs, tolower)   # Converting text to lowercase:    
docs <- tm_map(docs, removeWords, stopwords("english"))   # Removing "stopwords" 

library(SnowballC)   
docs <- tm_map(docs, stemDocument)   # Removing common word endings (e.g., "ing", "es")   
docs <- tm_map(docs, stripWhitespace)   # Stripping whitespace   
docs <- tm_map(docs, PlainTextDocument)

## This is the end of the preprocessing stage. 


### Stage the Data 

dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)   

### Explore your data  
### Exporting the matrix to Excel.

freq <- colSums(as.matrix(dtm))   
length(freq)   
ord <- order(freq)   
m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="DocumentTermMatrix.csv")   
  
#  Removing sparse terms:  

dtms <- removeSparseTerms(dtm, 0.1) # This makes a matrix that is 10% empty space, maximum.   

### Word Frequency 

head(table(freq), 20)   

# The above output is two rows of numbers. The top number is the frequency with which 
# words appear and the bottom number reflects how many words appear that frequently. 

tail(table(freq), 20)   

# Considering only the 20 greatest frequencies

# View a table of the terms after removing sparse terms, as above.

freq <- colSums(as.matrix(dtms))   
freq 

# The above matrix was created using a data transformation we made earlier. 
# An alternate view of frequency:  
# This will identify all terms that appear frequently (in this case, 50 or more times).

findFreqTerms(dtm, lowfreq=50)   # Change "lowfreq" to whatever is most appropriate for your data.

   
### Plot Word Frequencies
# Plot words that appear at least 50 times.Change "freq" to whatever is most appropriate for your data. 
# Better keep the freq value higher if mining a big data set. 

library(ggplot2)   
wf <- data.frame(word=names(freq), freq=freq)   
p <- ggplot(subset(wf, freq>50), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   

  
## Relationships Between Terms
### Term Correlations
# See the description above for more guidance with correlations.
# If words always appear together, then correlation=1.0.    
findAssocs(dtm, c("question" , "analysi"), corlimit=0.98) # specifying a correlation limit of 0.98   
# 
# Change "question" & "analysi" to terms that actually appear in your texts.
# Also adjust the `corlimit= ` to any value you feel is necessary.
#
# 
### Word Clouds
# First load the package that makes word clouds in R.    
library(wordcloud)   
dtms <- removeSparseTerms(dtm, 0.15) # Preparing the data (max 15% empty space)   
freq <- colSums(as.matrix(dtm)) # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=100, rot.per=0.2, colors=dark2)    

### Clustering by Term Similarity

### Hierarchal Clustering   
dtms <- removeSparseTerms(dtm, 0.15) # This makes a matrix that is only 15% empty space.
library(cluster)   
d <- dist(t(dtms), method="euclidian")   # First calculate distance between words
fit <- hclust(d=d, method="ward")   
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=2)   # "k" defines the number of clusters you are using   
rect.hclust(fit, k=2, border="red") # draw dendogram with red borders around the clusters   

### K-means clustering   
library(fpc)   
library(cluster)  
dtms <- removeSparseTerms(dtm, 0.15) # Preparing the data (max 15% empty space)   
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  