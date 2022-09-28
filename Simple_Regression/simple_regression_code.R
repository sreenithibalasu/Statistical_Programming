setwd("/Users/sreenithibalasubramanian/Desktop/UIUC MSIM/Sem 1/IS507-DATA-STATS")

# Read the data
library(readr)
library(plyr)

df <- read.csv("Student_Grade_Prediction.csv")

#Check if the data is read properly

View(df)

head(df)

#Print column names
names(df)

# Check dimensions - 395 rows, 33 columns
dim(df)


# Check missing data - RESULT: NONE
sum(is.na(df))

# Variables needed:

# G3
table(df$G3)

#G2
table(df$G2)

#Internet
table(df$internet)

#Activities
table(df$activities)

#higher education
table(df$higher)

#extra support
table(df$schoolsup)

# change string to numerical categoricals for internet, activities, higher education, school support and sex

df$internet <- revalue(df$internet, c("yes"="1", "no"="0"))
df$internet <- as.numeric(df$internet)

df$activities <- revalue(df$activities, c("yes"="1", "no"="0"))
df$activities <- as.numeric(df$activities)

df$higher <- revalue(df$higher, c("yes"="1", "no"="0"))
df$higher <- as.numeric(df$higher)

df$schoolsup <- revalue(df$schoolsup, c("yes"="1", "no"="0"))
df$schoolsup <- as.numeric(df$schoolsup)

df$sex <- revalue(df$sex, c("M"="0", "F"="1"))
df$sex <- as.numeric(df$sex)

df$nursery <- revalue(df$nursery, c("yes"="1", "no"="0"))
df$nursery <- as.numeric(df$nursery)

df$romantic <- revalue(df$romantic, c("yes"="1", "no"="0"))
df$romantic <- as.numeric(df$romantic)

df$famsup <- revalue(df$famsup, c("yes"="1", "no"="0"))
df$famsup <- as.numeric(df$famsup)

df$paid <- revalue(df$paid, c("yes"="1", "no"="0"))
df$paid <- as.numeric(df$paid)

table(df$Mjob)
df$Mjob <- revalue(df$Mjob, c("at_home"="1", "health"="2",  "other"="3", "services"="4" , "teacher"="5" ))
df$Mjob <- as.numeric(df$Mjob)

table(df$Fjob)
df$Fjob <- revalue(df$Fjob, c("at_home"="1", "health"="2",  "other"="3", "services"="4" , "teacher"="5" ))
df$Fjob <- as.numeric(df$Fjob)

table(df$reason)
df$reason <- revalue(df$reason, c("course"="1", "home"="2", "other"="3", "reputation"="4"))
df$reason <- as.numeric(df$reason)

table(df$guardian)
df$guardian <- revalue(df$guardian, c("father"="1", "mother"="2",  "other"="3" ))
df$guardian <- as.numeric(df$guardian)

table(df$school)
df$school <- revalue(df$school, c("GP"="1", "MS"="2"))
df$school <- as.numeric(df$school)

table(df$famsize)
df$famsize <- revalue(df$famsize, c("GT3"="1", "LE3"="2"))
df$famsize <- as.numeric(df$famsize)

table(df$address)
df$address <- revalue(df$address, c("R"="1", "U"="2"))
df$address <- as.numeric(df$address)

table(df$Pstatus)
df$Pstatus <- revalue(df$Pstatus, c("A"="1", "T"="2"))
df$Pstatus <- as.numeric(df$Pstatus)

#Descrpitpve statistics

library(Hmisc)

describe(df)

nrow(df)
#-------------------------------------------------------------------------------------------------------

#1. Create a multiple linear regression to explain the third period exam(G3).

names(df)
#Check Spearman Correlations
df_filtered <- df[, c(1:30, 33)]

library(corrplot)

corrplot(cor(df_filtered, method = "spearman"))

corrplot(cor(df_filtered, method = "spearman"), method="number")


#Using Manual Multiple Linear Regression

#Create Initial Linear Regression Model

model1 <- lm(G3 ~ ., data=df_filtered)
model1

library(DescTools)
VIF(model1)

summary(model1)

#Diagnostic Plots for Model Fit
par(mfrow = c(2, 2))
plot(model1)

#Using Stepwise Multiple Linear Regression

null = lm(G3 ~ 1-G3, data=df_filtered)
null

full = lm(G3 ~ .-G3, data=df_filtered)
full

#Stepwise Regression
train_Step = step(null, scope = list(upper=full), direction="both")
summary(train_Step)

#-------------------------------------------------------------------------------------------------------
#Lasso Regression

x=model.matrix(G3 ~ ., data=df_filtered)
y=df_filtered$G3

library(glmnet)
cv.lasso <- cv.glmnet(x,y, typemeasure="mse", alpha=1)
cv.lasso

ls(cv.lasso)

Lambda.best <- cv.lasso$lambda.min

predict(cv.lasso, s = Lambda.best, type = "coefficients")


#Ordinary Least Squares Regression

model2 <- lm(G3 ~ sex+age+address+famsize+Pstatus+Medu+reason+traveltime+studytime+failures+schoolsup+famsup+higher+internet+romantic+freetime+goout+absences, data=df_filtered)
summary(model2)
