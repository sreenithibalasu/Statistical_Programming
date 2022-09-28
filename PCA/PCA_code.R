library(DescTools)
library(Hmisc) #Describe Function
library(psych) #Multiple Functions for Statistics and Multivariate Analysis
library(GGally) #ggpairs Function
library(ggplot2) #ggplot2 Functions
library(vioplot) #Violin Plot Function
library(corrplot) #Plot Correlations
library(REdaS) #Bartlett's Test of Sphericity
library(psych) #PCA/FA functions
library(factoextra) #PCA Visualizations
library("FactoMineR") #PCA functions
library(ade4) #PCA Visualizations

##############################################################################################

#Set Working Directory
setwd('/Users/sreenithibalasubramanian/Downloads')


#Read in Datasets

big5 <- read.csv(file="BIG5.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(big5)
#1,010-Sample Size and 150 variables

#Show for first 6 rows of data
head(big5)

names(big5)

################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(big5))

#no missing values

################################################################################

View(big5)

#data only has numeric values

################################################################################
#Make subsets
names(big5)

e <- big5[, c(1:10)]
n <- big5[, c(11:20)]
a <- big5[, c(21:30)]
c <- big5[, c(31:40)]
o <- big5[, c(41:50)]

##################################################
#PCA/FA
##################################################

#Test KMO Sampling Adequacy

# install.packages("psych")
# install.packages("REdaS")
# install.packages("fmsb")
library(psych)
KMO(big5)
#Overall MSA =  0.91

#Test Bartlett's Test of Sphericity
library(REdaS)
bart_spher(big5)
#p-value < 2.22e-16 (Very Small Number)


#Test for Reliability Analysis using Cronbach's Alpha
library(psych)
alpha(big5, check.keys = TRUE)
#raw_alpha = 0.88

library(fmsb)
CronbachAlpha(big5)
#value 0.54969
#######################################################
#Parallel Analysis (Horn's parallel analysis)


#Compares actual eigenvalues with ones from a Monte-Carlo simulated dataset of the same size


library(psych)

comp <- fa.parallel(big5)
comp

#we see from the (parallel analysis) Monte Carlo simulations that the number of factors selected is 7
# and the number of components selected is 10. But this simulation highly over estimates, but is an initial
#analysis. We'd want to use less than this

#components with eigen values >1: 8, so typically we'd want to use close to 8 components
#parallel analysis of scree plots
#######################################################
#Create PCA

#center and scale the data
p = prcomp(big5, center=T, scale=T)
p

#Check Scree Plot
plot(p)
abline(1, 0)

#components with eigen values > 1
#knee is closer to 8

#Check PCA Summary Information
summary(p)
print(p)

biplot(p)
########################################################
#PCA


p2 = psych::principal(big5, rotate="varimax", nfactors=6, scores=TRUE)
p2

p3<-print(p2$loadings, cutoff=.47, sort=T)


#PCAs Other Available Information

ls(p2)

p2$values
p2$communality
p2$rot.mat
########################################################################################

#Calculating scores

scores <- p2$scores

cor(scores)

summary(scores)

scores_1 <- scores[,1]

min_score <- min(scores_1)
min_score

max_score <- max(scores_1)
max_score

scores_2 <- scores[,2]
min_score <- min(scores_2)
min_score

max_score <- max(scores_2)
max_score

scores_3 <- scores[,3]
min_score <- min(scores_3)
min_score

max_score <- max(scores_3)
max_score

scores_4 <- scores[,4]
min_score <- min(scores_4)
min_score

max_score <- max(scores_4)
max_score

scores_5 <- scores[,5]
min_score <- min(scores_5)
min_score

max_score <- max(scores_5)
max_score

scores_6 <- scores[,6]
min_score <- min(scores_6)
min_score

max_score <- max(scores_6)
max_score


#Conducting Factor Analysis

fit = factanal(big5, 5)
print(fit$loadings, cutoff=.4, sort=T)

