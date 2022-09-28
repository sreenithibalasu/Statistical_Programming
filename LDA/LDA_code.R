setwd("/Users/sreenithibalasubramanian/Downloads")

df <-  read.csv("Employee.csv",header=TRUE, sep=',')
head(df)

View(df)

# check for missing values. Result: there's none
sum(is.na(df))

# change categorical strings to numbers

library(plyr)
table(df$Education)
df$Education <- revalue(df$Education, c("Bachelors"="1", "Masters"="2", "PHD"="3"))
df$Education <- as.numeric(df$Education)

table(df$Gender)
df$Gender <- revalue(df$Gender, c("Male"="1", "Female"="2"))
df$Gender <- as.numeric(df$Gender)

table(df$EverBenched)
df$EverBenched <- revalue(df$EverBenched, c("No"="1", "Yes"="2"))
df$EverBenched <- as.numeric(df$EverBenched)

# Let's keep City for now
table(df$City)
df$City <- revalue(df$City, c("Bangalore"="1", "New Delhi"="2", "Pune"="3"))
df$City <- as.numeric(df$City)

names(df)
###############################################################################
### LDA USING LEAVE ONE OUT CROSS VALIDATION ###
###############################################################################
# DEPENDENT VAR: LeaveOrNot
library(MASS)

employeeLDA_CV <- lda(LeaveOrNot ~ ., data=df, CV=TRUE)
employeeLDA_CV

# percent correct for each category of LeaveOrNot
ct <- table(df$LeaveOrNot, employeeLDA_CV$class)
diag(prop.table(ct, 1))

# total percent correct
sum(diag(prop.table(ct)))

#Accuracy is around 71.05%

###############################################################################
### LDA USING TRAIN AND TEST ###
###############################################################################
#Creating Training and Testing Samples

library(caTools)
set.seed(42)   

sample = sample.split(df,SplitRatio = 0.80) 
train =subset(df,sample ==TRUE) 
test=subset(df, sample==FALSE)

employeeLDA_train = lda(LeaveOrNot ~ ., data=train)
employeeLDA_train

plot(employeeLDA_train)

predicted<-predict(employeeLDA_train, newdata=test[,1:8])$class

# Compare the results of the prediction
table(predicted, test$LeaveOrNot)

mean(predicted== test$LeaveOrNot)
# Accuracy =72.43% (for a given random split, the accuracy can change)

############################################################################
#PROJECT DATASET

#BREAST CANCER DATASET

# library(caret)
# library(mltools)
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

#Fill with 0s
data[is.na(data)] <- 0

#Check new data has no missing data
sum(is.na(data))

#Hypothesis testing - is there a difference in mean radius for people with malignant and benign tumours?
#Null hypothesis: there is NO difference in radius between M and B
library(readr)
library(plyr)
data$diagnosis <- revalue(data$diagnosis, c("M"="1", "B"="0"))
data$diagnosis <- as.numeric(data$diagnosis)
###############################################################################
### LDA USING LEAVE ONE OUT CROSS VALIDATION ###
###############################################################################

#remove ID
data_new = data[2:32]
names(data_new)

library(MASS)

diagnosisLDA_CV <- lda(diagnosis ~ ., data=data_new, CV=TRUE)
diagnosisLDA_CV

# percent correct for each category of LeaveOrNot
ct <- table(data_new$diagnosis, diagnosisLDA_CV$class)
diag(prop.table(ct, 1))

# total percent correct
sum(diag(prop.table(ct)))

#Accuracy comes to ~ 95.78%
###############################################################################
### LDA USING TRAIN AND TEST ###
###############################################################################
#Creating Training and Testing Samples

library(caTools)
set.seed(123)   

sample = sample.split(data_new,SplitRatio = 0.80) 
train =subset(data_new,sample ==TRUE) 
test=subset(data_new, sample==FALSE)

diagnosisLDA_train = lda(diagnosis ~ ., data=train)
diagnosisLDA_train

plot(diagnosisLDA_train)

predicted<-predict(diagnosisLDA_train, newdata=test[,2:31])$class

# Compare the results of the prediction
table(predicted, test$diagnosis)

mean(predicted== test$diagnosis)
#Accuracy  = 98.44%
###############################################################################
