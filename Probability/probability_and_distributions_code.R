# Q1- male/female distribution

male_bachelors <- 0.16
male_grad <- 0.09

prob_man_atleast_bachelors <- (male_bachelors + male_grad)

print(paste("Probability of randomly chosen man has at least a Bachelors degree = ", prob_man_atleast_bachelors))

woman_bachelors <- 0.17
woman_grad <- 0.09

prob_woman_atleast_bachelors <- (woman_bachelors + woman_grad)

print(paste("Probability of randomly chosen woman has at least a Bachelors degree = ", prob_woman_atleast_bachelors))

# Assumption: a man and a woman getting a bachelors degree are independent events

prob_man_and_woman_bachelors <- prob_man_atleast_bachelors * prob_woman_atleast_bachelors

print(paste("Probability of man and woman has at least a Bachelors degree = ", prob_man_and_woman_bachelors))


#Q2 - rolling two dice

prob_sum_dice_5 <- 2 * (2/36.0)
prob_sum_dice_not_5 <- 1 - prob_sum_dice_5

print(paste("Probability of sum dice is not 5 =", round(prob_sum_dice_not_5,3)))

prob_sum_atleast_7 <- ((6+5+4+3+2+1)/36.0)
print(paste("Probability of sum atleast 7 =", round(prob_sum_atleast_7,3)))

prob_sum_no_more_than_8 <- (1+2+3+4+5+6+5)/36.0
print(paste("Probability of sum no more than 8 =", round(prob_sum_no_more_than_8,3)))

#Q3

prob_excellent_health <- 0.2329

prob_excellent_health_and_coverage <- 0.2099
prob_excellent_health_and_no_coverage  <- 0.0230
prob_coverage <- 0.8738
prob_no_coverage <- 0.1262

prob_excellent_health_given_coverage <- prob_excellent_health_and_coverage/prob_coverage

print(paste("P(excellent health | coverage YES) = ", round(prob_excellent_health_given_coverage,3)))

prob_excellent_health_given_no_coverage <- prob_excellent_health_and_no_coverage/prob_no_coverage

print(paste("P(excellent health | coverage NO) = ", round(prob_excellent_health_given_no_coverage,3)))

#Q4
p_A <- 0.3
p_B <- 0.7

p_A_and_B <- p_A * p_B
print(paste("P(A and B) = ", p_A_and_B))

p_A_or_B <- p_A + p_B
print(paste("P(A or B) = ", p_A_or_B))

P_A_given_B <- p_A_and_B / p_B
print(paste("P(A|B) = ", P_A_given_B))

#Q5

#Z > -1.13
pnorm(-1.13, lower.tail = FALSE)

#Z<0.18
pnorm(0.18)

#Z>8
pnorm(8, lower.tail = FALSE)

# |Z| < 0.5
pnorm(0.5) - pnorm(-0.5)

#Q6

verbal <- 160
quants <- 157

mean_verbal <- 151
std_verbal <- 7

mean_quants <- 153
std_quants <- 7.67

z_score_verbal <- (verbal - mean_verbal)/std_verbal
print(paste("VERBAL Z-score = ",round(z_score_verbal,3)))

z_score_quants <- (quants - mean_quants)/std_quants
print(paste("QUANTS Z-score = ",round(z_score_quants,3)))

#Percentile
pnorm(z_score_verbal)

pnorm(z_score_quants)

# Generating visualizations
# Source: https://github.com/coatless/visualize

#install.packages('visualize')
library('visualize')

#Z > -1.13
visualize.norm(stat=-1.13,mu=0,sd=1,section="upper")

#Z<0.18
visualize.norm(stat=0.18,mu=0,sd=1,section="lower")

#Z>8
visualize.norm(stat=8,mu=0,sd=1,section="upper")

# |Z| < 0.5
visualize.norm(stat=c(-0.5,0.5),mu=0,sd=1,section="bounded")
