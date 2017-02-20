###############################################################################
########## Data Reduction using PCA (Principal Component Analysis) ############
###############################################################################

if (!require("pacman")) install.packages("pacman") #install package pacman if needed

library(pacman)
p_load(psych)      #load package - "psych"#
p_depends(psych)    #check dependencies#
p_load(GPArotation) #install dependencies#

############################ Loading the data set #############################

b5 <- read.csv("C:/Users/Sai Rakesh Ghanta/Desktop/b5.csv", header = T)
colnames(b5)
boxplot(b5)

####################### Principal Component Analysis ##########################

##### Here I use function "principal" from psych package we loaded above.###### 
#################### You can also use prcomp, princomp ########################

####################### First PCA with no rotation ############################

pc0 <- principal(b5, nfactors = 5)
pc0

################ Second PCA with Oblimin rotation (Oblique) ####################

pc1 <- principal(b5, nfactors = 5, rotate = "oblimin")
pc1

################## PLOT position of variables on components ####################

plot(pc1)

############################# Clear Workspace ##################################

rm(list = ls())






