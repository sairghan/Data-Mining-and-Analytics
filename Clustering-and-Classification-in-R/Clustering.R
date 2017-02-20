#################### Clustering in R #####################

##################### LOAD DATA ##########################
##################### Read CSV ###########################
states <- read.csv("~/Desktop/ClusterData.csv", header = T)
colnames(states)

################### Numerical Data #######################

st <- states[, 3:27]
row.names(st) <- states[, 2]
colnames(st)

#################### Sports Data #########################

sports <- st[, 8:11]
head(sports)

#################### Clustering ###########################
############### Create distance matrix ####################

d <- dist(st)

############## Heirarchical Clustering ####################

c <- hclust(d)
c

############# Plot Dendogram #######################

plot(c, main = "Cluster with all searches and personality")

############ Nest Commands #########################

plot(hclust(dist(sports)),main = "Sports Searches")



