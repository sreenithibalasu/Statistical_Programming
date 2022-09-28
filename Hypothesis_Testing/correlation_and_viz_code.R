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

#-------------------------------------------------------------------------------------------------------
#Q1: Does study time (studytime) affect a students' grade on the third period (G3) exam?
# Use ANOVA.Check for normality before

shapiro.test(df$studytime) #p-value < 2.2e-16 < 0.05. data not normal

shapiro.test(df$G3) #p-value = 8.836e-13 < 0.05. data not normal

# since data not normal use nonparametric anova correlation

#Kruskal-Wallis test - Nonparametric ANOVA
kruskal.test(G3~studytime, data = df) 

library(FSA)
dunnTest(G3~studytime, data = df)
# p-value = 0.5557  > 0.5. Studytime does not affect G3 
#-------------------------------------------------------------------------------------------------------
#Q2: What is the relationship or association between first period exam (G1) and absebces (absences)

# Use correlation test, but check for normality before

shapiro.test(df$absences) #p-value < 2.2e-16 < 0.05. data not normal

shapiro.test(df$G1) #p-value = 2.454e-06 < 0.05. data not normal

# since data not normal use spearman correlation

cor.test(df$absences, df$G1, alternative = "two.sided", method = "spearman", conf.level = 0.95)

# p = 0.9293 > 0.05. There is no relation between the two variables
#-------------------------------------------------------------------------------------------------------
#Q3: Visualize the correlations between second exam (G2) and family relations to absences

library(corrplot)

names(df)

df_g2 <- df[, c(24:30, 32)]
corrplot(cor(df_g2, method = "spearman"), method="number")
#-------------------------------------------------------------------------------------------------------
#Q4: visualizations
table(df$reason)

df_GP <- df[df$school == 'GP', ]
df_MS <- df[df$school == 'MS', ]
countsGP <- table(df_GP$reason)
# lbls <- paste(names(countsGP), "\n", countsGP, sep="")

colors <- c("yellow2","olivedrab3","orangered3", "yellow3") 

lbls <- paste0(round(100 * countsGP/sum(countsGP), 2), "%")

pie(countsGP, labels = lbls, col=colors,
    main="Pie Chart of Reasons")

legend("topleft", legend = names(countsGP),
       fill =  colors)

barplot(countsGP, main="Reason for choosing GP Distribution", 
        xlab="Reason", ylab="Count")

