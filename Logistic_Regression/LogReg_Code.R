setwd("/Users/sreenithibalasubramanian/Downloads")

df <-  read.csv("Employee.csv",header=TRUE, sep=',')
head(df)

View(df)

# check for missing values. Result: there's none
sum(is.na(df))

# change categorical strings to numbers

library(plyr)
table(df$Education)
#df$Education <- revalue(df$Education, c("Bachelors"="1", "Masters"="2", "PHD"="3"))
#df$Education <- as.numeric(df$Education)

#table(df$Gender)
#df$Gender <- revalue(df$Gender, c("Male"="1", "Female"="2"))
#df$Gender <- as.numeric(df$Gender)

#table(df$EverBenched)
#df$EverBenched <- revalue(df$EverBenched, c("No"="1", "Yes"="2"))
#df$EverBenched <- as.numeric(df$EverBenched)

# Let's keep City for now
#table(df$City)
#df$City <- revalue(df$City, c("Bangalore"="1", "New Delhi"="2", "Pune"="3"))
#df$City <- as.numeric(df$City)

names(df)

###############################################################################
### TRAIN TEST SPLIT ###
###############################################################################

#Libraries

library(dplyr)     # data wrangling
library(ggplot2)   # plotting
library(rsample)   # training and testing splitting
library(caret)     # for logistic regression modeling and prediction outputs
library(vip)       # variable importance

# split into train and test sets

set.seed(42)
split <- initial_split(df, prop=.8, strata="LeaveOrNot")
train <- training(split)
test <- testing(split)

###############################################################################
### LOGISTIC REGRESSION ###
###############################################################################

#For explaining dependent variable

df$LeaveOrNot <- as.factor(df$LeaveOrNot)

log_reg <- glm(
  LeaveOrNot ~ .,
  family = "binomial", 
  data = df
)

summary(log_reg) #Coefficients Not in exponential form


#Coefficients in exponential form
log_reg %>% 
  gtsummary::tbl_regression(exp = TRUE) 

train$LeaveOrNot<- as.factor(train$LeaveOrNot)

#For Predicting dependent variable
log_reg = train(
  form = LeaveOrNot ~ .,
  data = train,
  method = "glm",
  family = "binomial"
)

#Confusion Matrix
confusionMatrix(predict(log_reg, test), as.factor(test$LeaveOrNot))


#Variables of Importance

vip(log_reg, num_features = 10)


#ROC Curves

log_reg_train <- glm(LeaveOrNot ~ ., data=train, family=binomial)


#ROC Curves
library(ROCR)

log_reg_test_prob <- log_reg_train %>% predict(test, type = "response")
log_reg_test_prob

preds <- prediction(as.numeric(log_reg_test_prob), test$LeaveOrNot)

perf <- performance(preds,"tpr","fpr")
plot(perf,colorize=TRUE)


library(precrec)
precrec_obj <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot)
autoplot(precrec_obj)

## Get AUCs
sm_aucs <- auc(precrec_obj)
## Shows AUCs
sm_aucs

precrec_obj2 <- evalmod(scores = log_reg_test_prob, labels = test$LeaveOrNot, mode="basic")
autoplot(precrec_obj2)   


library(ROCit)
## Warning: package 'ROCit' was built under R version 3.5.2
ROCit_obj <- rocit(score=log_reg_test_prob,class=test$LeaveOrNot)
plot(ROCit_obj)

summary(ROCit_obj)

measure <- measureit(score = log_reg_test_prob, class = test$LeaveOrNot,
                     measure = c("ACC", "MIS", "SENS", "SPEC", "PREC", "REC","PPV","NPV", "FSCR"))
measure

