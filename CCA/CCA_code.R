#Set Working Directory
setwd('/Users/sreenithibalasubramanian/Desktop/UIUC MSIM/Sem 1/IS507-DATA-STATS')


#Read in Datasets

responses <- read.csv(file="responses.csv", header=TRUE, sep=",")

#Check Sample Size and Number of Variables
dim(responses)
#1,010-Sample Size and 150 variables

#Show for first 6 rows of data
head(responses)

names(responses)
################################################################################################

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(responses))
#571 total missing values (571 cells with missing data)


#Treat Missing Values

#Listwise Deletion
responses2 <- na.omit(responses)

#Check new data has no missing data
sum(is.na(responses2))


#################################################################################################################
# Perform a canonical correlation analysis describing the relationships between 
# the music and spending variables using the data under the R PCA and FA Lab

#Show Structure of Dataset
str(responses2, list.len=ncol(responses2))

#Show column Numbers
names(responses2)

#Categorical Variables (Var_num):  Smoking (74), Alcohol (75), Punctuality (108), Lying (109), Internet.usuage (133), Gender (145), 
#                         Left...right.handed (146), Education (147), Only.child(148), Village.town (149), House...block.of.flats (150)


#Create new subsets of data (Numeric Variables Only)

responses3 <- responses2[,c(1:73,76,77:107,110:132,134:140,141:144)]

music <- responses2[,1:19]
spending <- responses2[,134:140]
#################################################################################################################
#Base Package
library(foreign)
library(CCA)
library(yacca)
library(MASS)
# This gives us the canonical correlates, but no significance tests
c <- cancor(music, spending)
c$cor
#################################################################################################################
#Breakdown of the Correlations
matcor(music, spending)

#Correlations between music and spending (Y)
cc_mm = cc(music, spending)
cc_mm$cor

#Funcrions for CCA
ls(cc_mm)

#XCoef Correlations
cc_mm$xcoef

#YCoef Correlations
cc_mm$ycoef

#Calculate Scores
loadings_mm = comput(music, spending, cc_mm)
ls(loadings_mm)

#Correlation X Scores
loadings_mm$corr.X.xscores

#Correlation Y Scores
loadings_mm$corr.Y.yscores

# A basic visualization of the canonical correlation
plt.cc(cc_mm)

#################################################################################################################

library(yacca)
c2 = cca(music, spending)
c2

F.test.cca(c2)

#CV1
helio.plot(c2, cv=1, x.name="Music Values", 
           y.name="Spending Values")

#CV2
helio.plot(c2, cv=2, x.name="Music Values", 
           y.name="Spending Values")

#Function Names
ls(c2)

# Perform a chi-square test on C2
c2
ls(c2)
c2$chisq
c2$df
summary(c2)
round(pchisq(c2$chisq, c2$df, lower.tail=F), 3)

#################################################################################################################
#BREAST CANCER DATASET

library(caret)
library(mltools)
setwd("/Users/sreenithibalasubramanian/Downloads")

data <-  read.csv("data.csv",header=TRUE, sep=',')
head(data)

names(data)


#diagnosis is the target variable

View(data)

#Check for Missing Values (i.e. NAs)

#For All Variables
sum(is.na(data))
#571 total missing values (571 cells with missing data)


#Treat Missing Values

#Listwise Deletion
data[is.na(data)] <- 0

#Check new data has no missing data
sum(is.na(data))

# row <- one_hot(as.data.table(data$diagnosis))
dummy <- dummyVars("~.", data=data)
data_onehot <- data.frame(predict(dummy, newdata=data))

names(data_onehot)

#doing CCA for 
mean <- data_onehot[, c(4:13)]
names(mean)
diagnosis <- data_onehot[, c(2,3)]
names(diagnosis)
worst <- data_onehot[, c(24:33)]
names(worst)


# library(plyr)
# diagnosis$diagnosis <- revalue(diagnosis$diagnosis, c("M"=1, "B"=0))
# diagnosis$diagnosis <- as.numeric(diagnosis$diagnosis)

#################################################################################################################

library(yacca)
c2 = cca(mean,worst)
c2

F.test.cca(c2)

#CV1
helio.plot(c2, cv=1, x.name="Mean Params", 
           y.name="Worst Params")
helio.plot(c2, cv=2, x.name="Mean Params", 
           y.name="Worst Params")


#Function Names
ls(c2)

# Perform a chi-square test on C2
c2
ls(c2)
c2$chisq
c2$df
summary(c2)
round(pchisq(c2$chisq, c2$df, lower.tail=F), 3)

# c <- cancor(mean, worst)
# c$cor
