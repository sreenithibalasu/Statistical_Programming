# Set working directory

setwd("/Users/sreenithibalasubramanian/Desktop/UIUC MSIM/Sem 1/IS507-DATA-STATS")

# Read the data
library(readr)

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

df$internet_num <- revalue(df$internet, c("yes"="1", "no"="0"))
df$internet_num <- as.numeric(df$internet_num)

df$activities_num <- revalue(df$activities, c("yes"="1", "no"="0"))
df$activities_num <- as.numeric(df$activities_num)

df$higher_num <- revalue(df$higher, c("yes"="1", "no"="0"))
df$higher_num <- as.numeric(df$higher_num)

df$schoolsup_num <- revalue(df$schoolsup, c("yes"="1", "no"="0"))
df$schoolsup_num <- as.numeric(df$schoolsup_num)

df$sex_num <- revalue(df$sex, c("M"="0", "F"="1"))
df$sex_num <- as.numeric(df$sex_num)

#Descrpitpve statistics

library(Hmisc)

describe(df)
# ------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Is there a difference for G3- final grades for these Portuguese students between students with internet from students without internet?

library(RVAideMemoire)

byf.shapiro(as.matrix(df$G3)~internet_num,data=df)


#Mann-Whitney U (Wilcoxon) test - Nonparametric T-Test

wilcox.test(df$G3~df$internet) # where y is numeric and A is A binary factor

# DATA IS NOT NORMAL SINCE P-VALUE = 0.0002 < 0.05 and 1.878e-11 < 0.05
# looking at p-value = 0.0324 < 0.05. THERE IS  A DIFFERENCE IN FINAL GRADE (G3) FOR PEOPLE WITH AND WITHOUT INTERNET

# ------------------------------------------------------------------------------------------------------------------------------------------------

# Is there a relationship between activities and higher education (higher variable)? - chi-squared test
library(gmodels)

shapiro.test(df$activities_num) # p-value < 2.2e-16 < 0.05 = NOT NORMAL
shapiro.test(df$higher_num) # p-value < 2.2e-16 < 0.05 = NOT NORMAL

CrossTable(df$activities_num,df$higher_num,  digits = 2, expected=TRUE, prop.r=TRUE, 
           prop.c = TRUE, prop.chisq = FALSE, chisq = TRUE, fisher = TRUE, format="SPSS")

# Answer: USING CHI-SQUARED TEST: p = 0.055 >  0.050. THERE IS NO RELATION BETWEEN ACTIVITIES AND HIGHER EDUCATION
# ------------------------------------------------------------------------------------------------------------------------------------------------

# Is there a difference between the G2-second grade and the G3-final grade?


# check for normality

shapiro.test(df$G2) # P < 0.05 = NOT NORMAL
shapiro.test(df$G3)  #P < 0.05 = NOT NORMAL


#Paired Test - Nonparametric (Sign Rank test)

wilcox.test(df$G2, df$G3, paired=TRUE) 

#  DATA IS NOT NORMAL, WE USE WILCOX TEST -  P-VALUE =0.874  >  0.05 HENCE THERE IS NO DIFFERENCE BWT G2 AND G3
# ------------------------------------------------------------------------------------------------------------------------------------------------

# Create two categorical variables for G2-second grade and the G3-final grade using a cutoff of 15. 
# Test whether there is a difference between the two categorical variables.

df$G2_binary <-cut(df$G2,c(0, 15, Inf), include.lowest = TRUE, labels=c("0","1"))
df$G2_binary <- as.numeric(df$G2_binary)

df$G3_binary <-cut(df$G3, c(0, 15, Inf), include.lowest = TRUE, labels=c("0","1"))
df$G3_binary <- as.numeric(df$G3_binary)

table(df$G2_binary)

table(df$G3_binary)

shapiro.test(df$G2_binary)
shapiro.test(df$G3_binary)

mcnemar.test(df$G2_binary,df$G3_binary)

# ACCORDING TO MCNEMAR TEST, P = 0.096  > 0.05. There is no difference
# ------------------------------------------------------------------------------------------------------------------------------------------------

# 5. What is the proportion of students receiving extra educational support? • Test whether this proportion is different than 50%?

table(df$schoolsup_num) 

51+344 

extra_sup <- prop.test(x=51, n=395, p=0.50, correct=FALSE) 

#2-YES, 1-NO 
extra_sup

# ------------------------------------------------------------------------------------------------------------------------------------------------

# What is the proportion of males and females in extracurricular activities? 
# • Construct a 95% Confidence Interval
table(df$sex, df$activities)

table(df[df$activities=='yes',]$sex)

#female
96 + 105
female_yes<- prop.test(x=96, n=201, p=0.5, correct=FALSE)

#MALE
male_yes<-  prop.test(x=105, n=201, p=0.5, correct=FALSE)

female_yes
male_yes

#------------------------------------------------------------------------------------------------------------
