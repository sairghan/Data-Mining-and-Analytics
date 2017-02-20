########################## Classification in R #######################

######################## Loading required packages ###################

library(class)
library(pacman)
pacman::p_load

############################ Load Data ###############################

df <- read.csv("C:/Users/sag163/Desktop/ccdefault.csv", header = T)
colnames(df)
head(df)

######################### Normalize Data #############################

########## Defining the custom function #############

normalize <- function(x) {
  norm <- ((x - min(x))/(max(x) - min(x)))
  return(norm)
}

######### Applying the custom function to a data frame #############

dfn <- as.data.frame(lapply(df[, 2:24], normalize))
head(dfn)

##### Renaming the outcome variable ##########

dfn <- cbind(dfn, df[, 25])
names(dfn)[24] <- "DEFAULT"

############# Checking the data ##############

colnames(dfn)
head(dfn)

############### Splitting data into training & testing sets ###############

########## Training (2/3 rds) & Testing (1/3 rd) ##############

set.seed(2786) 
dfn.split <- sample(2, nrow(dfn), replace = TRUE, prob = c(2/3, 1/3))

######## Creating Training & Testing data sets ###########

dfn.train <- dfn[dfn.split == 1, 1:23]
dfn.test  <- dfn[dfn.split == 2, 1:23]

######## Creating outcome labels ###########

dfn.train.labels <- dfn[dfn.split == 1, 24]
dfn.test.labels <- dfn[dfn.split == 2, 24]

################### Building & Testing the Classifier ######################

dfn.pred <- knn(train = dfn.train, test = dfn.test, cl = dfn.train.labels, k = 9)

############ Compare predicted & observed outcomes

table(dfn.pred, dfn.test.labels)






