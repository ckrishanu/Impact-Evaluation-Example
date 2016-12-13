#-------------------------- Meta ------------------------------
#The examples in this R file are based on a subsample from the Bangladesh
#Household Survey 1991/92-1998/99 (Khandker et al., 2010)
#Attribution 3.0 IGO - CC BY 3.0 IGO
#http://www.pep-net.org/sites/pep-net.org/files/typo3doc/pdf/Training_Material/PEP_Guide_Stata.pdf
#
# Contributors: Krishanu Chakraborty
# 12th December 2016
# Version of code : 1.0.0
# R version : R version 3.3.2 (Sincere Pumpkin Patch)
# Last edited by : Krishanu Chakraborty


#--------------------- Data, Summary Stataistics and Tables ------------------------------------
# load packages

library(haven)
library(psych)
library(plyr)
library(reshape2)
library(gmodels)
library(graphics)
library(dplyr)
library(randomizr)
library(quantreg)

# set working directories

# getwd()
setwd("D:/R/Impact Evaluation Exercise")

# let's download the zip file containing the Stata do file and the dataset 
# and unzip it to the working directory.

download.file("http://www.pep-net.org/fileadmin/medias/pdf/Training_Material/hh_91_practice.dta", dest="dataset.zip", mode="wb")

unzip ("dataset.zip", exdir = getwd())

# let's load the dataset

hh_91_practice <- data.frame(read_dta("D:/R/Impact Evaluation Exercise/hh_91_practice.dta"))

# let us view the dataset

View(hh_91_practice)

# let us see the basic summary statistic

descr::descr(hh_91_practice)

# mean,median,25th and 75th quartiles,min,max

summary(hh_91_practice)

# item name ,item number, nvalid, mean, sd, 
# median, mad, min, max, skew, kurtosis, se

describe(hh_91_practice)

# The variable exptot measures total household expenditures and the variable
# sexhead identifies the gender of the household heads. This last one takes a
# value of 1 if the head of the household is a man and 0 if it is a woman.

attach(hh_91_practice)
mytable <- table(hh_91_practice$sexhead,hh_91_practice$exptot)
margin.table(mytable, 1)
describeBy(hh_91_practice$exptot, hh_91_practice$sexhead)
detach(hh_91_practice)



#----------------------------- Graphics ---------------------------------------------------
# distribution of the total household expenditures exptot

d <- density(hh_91_practice$exptot, kernel = c("epanechnikov"))

plot(d, xlab = "HH per capita total expenditure: Tk/year", main = "Distribution of household expenditure", col= "dark red")

# We draw the distribution of the household expenditures for each group depending on the 
# gender of the household head. We label the two distributions appropriately

# calculate the denisty functions

d0 <- density(subset(hh_91_practice$exptot,hh_91_practice$sexhead == 0 ), kernel = c("epanechnikov"))
d1 <- density(subset(hh_91_practice$exptot,hh_91_practice$sexhead == 1 ), kernel = c("epanechnikov"))

# plot the kernel density for sexhead == 0

plot(d0, xlab = "HH per capita total expenditure: Tk/year", main = "Distribution of household expenditure by gender of the hh head", col= "blue")

# add line for sexhead == 1

lines(d1 , col = 'red')

# add appropraite legends

legend( 10000, .0002, c('male', 'female'), lty = c(1,1), col = c('red', 'blue'), box.lwd = 0)

# Quite often, for statistical reasons and interpretation purposes, economists
# scale and transform the variables of interest. We create a new variable with the
# natural logarithm of the total household expenditures.

hh_91_practice$lnexptot <- log(hh_91_practice$exptot)

# As an exercise, draw the distribution of the transformed variable lnexptot.



#-------------------------- Random Assignment to the Treatment-----------------------------

# Suppose that we are asked to evaluate an upcoming microcredit program for
# Bangladesh. Ideally, we would like to compare two identical groups, with the
# only difference that one group would participate in the program (treatment group) 
# and the other would not (control group). Since the program has not
# yet been implemented, we can use random assignment to create the two
# groups. When judiciously implemented, this experimental approach guarantees
# that any significant difference between the future outcomes of the two
# groups is caused by the program. This chapter explains how to create those
# two groups before the program starts and includes some basic power calculations.
# To illustrate the randomization procedure we use the baseline dataset
# hh_91_practice.dta. This is a fictitious baseline survey with information about
# the target population before the microcredit program is implemented.

# Our baseline data contains information on 583 households from 87 villages. For
# the purpose of our evaluation, we want to select an experimental sample of
# 300 households, so that it is representative of the surveyed population. A simple
# way to select a representative experimental sample is to implement a virtual
# lottery. We distribute at random one "lottery ticket" to each household in the
# survey and select those with the lowest 300 numbers. The question of how many
# households to select will be addressed later.

describe(hh_91_practice)

# The code below creates the variable random which represents the lottery
# tickets. To draw the actual numbers we use the runif() command. It
# assigns a number to each household from the uniform distribution in the interval
# [0, 1]. Then, we sort all the households in increasing order with respect to
# their lottery ticket and select the first 300. In order to identify the experimental
# sample, we create the dummy variable experiment. It takes a value of 1 for
# households participating in the evaluation and 0 for the rest.


set.seed(20110402)

# Once we have randomly selected the households that will participate in our
# evaluation, the experimental sample should not change. To make sure that the
# same households are selected every time we execute our code, we shall use
# the set seed command before we draw the lottery tickets. Intuitively, this command
# anchors the random process to a particular algorithm to create those
# random numbers. It allows us to obtain the same results every time we run the
# program. In practice, the value of the seed does not matter as long as there is
# no obvious pattern.

anyDuplicated(hh_91_practice$nh)
hh_91_practice <- hh_91_practice[order(hh_91_practice$nh),]
hh_91_practice$random <- runif(nrow(hh_91_practice))
hh_91_practice <- hh_91_practice[order(hh_91_practice$random),]
hh_91_practice$index <- 1:nrow(hh_91_practice)
hh_91_practice$experiment <- 0
hh_91_practice$experiment[hh_91_practice$index <= 300] <- 1

mytable <- table(hh_91_practice$experiment,hh_91_practice$index)
margin.table(mytable, 1)


#---------------------------------- External Validity -------------------------------------
# #External validity means that the experimental sample is representative of the target
# population. When there is external validity, the conclusions from the experimental
# sample can be extrapolated to the target population from which this
# sample was drawn. When the experimental sample is large enough, the average
# of its variables tends toward the population mean (law of large numbers).1
# The code below explores the representativeness of an experimental sample of
# 20 households compared to the larger experimental sample of 300 households.

hh_91_practice$experiment_20 <- 0
hh_91_practice$experiment_20[hh_91_practice$index <= 20] <- 1

# The variable experiment_20 selects an experimental sample of 20 households
# instead of 300. We plot 3 densities of the exptot variable: one with all of
# the baseline data, one with the large experimental sample of 300 households,
# and one with the small experimental sample of 20 households.


d <- density(hh_91_practice$exptot, kernel = c("epanechnikov"))
d300 <- density(subset(hh_91_practice$exptot,hh_91_practice$experiment == 1 ), kernel = c("epanechnikov"))
d20 <- density(subset(hh_91_practice$exptot,hh_91_practice$experiment_20 == 1 ), kernel = c("epanechnikov"))

# plot the kernel density

plot(d, xlab = "HH per capita total expenditure: Tk/year", main = "Sample Size and representativeness",lty= 2, col= "blue")

# add other lines

lines(d1 , lty =3 , lwd = 2, col = 'red')
lines(d20, lty =4, lwd= 4, col = 'green')

# add appropraite legends

legend( 10000, .0002, c('survey', 'large sample', 'small sample'), lty = c(2,3,4), col = c('blue', 'red', 'green'), box.lwd = 0)

# we conclude that larger experimental samples are closer
# (more similar) to the original survey data
# 
# ------------------------------------- Treatment and control group -------------------------------
# 
# The second step consists in separating the experimental sample into two identical
# groups: treatment and control. Again, the variable random simulates
# the draw of a lottery number for each household in the experimental sample.

set.seed(19320419)

hh_91_practice_exp <- subset(hh_91_practice, hh_91_practice$experiment == 1)
hh_91_practice_exp$random <- runif(nrow(hh_91_practice_exp))
hh_91_practice_exp$treatment <- 0
hh_91_practice_exp$treatment <- as.numeric(hh_91_practice_exp$random < 0.5)
table(hh_91_practice_exp$treatment)
describeBy(hh_91_practice_exp$sexhead, hh_91_practice_exp$treatment)

#stratification

# #Suppose that we want to make sure that we have the same number of female
# household heads in the control and the treatment groups. We do this by running
# a separate lottery for the households with a female household head. For each
# value of sexhead we run a lottery to assign half of the households to the treatment
# group and half of the households to the control group

set.seed(19320419)
hh_91_practice_exp$random <- runif(nrow(hh_91_practice_exp))
hh_91_practice_exp <- hh_91_practice_exp %>%
  group_by(sexhead) %>%
  arrange(sexhead) %>% ## sorted gender
  mutate(strata_size=n())

hh_91_practice_exp <- hh_91_practice_exp %>%
  group_by(sexhead) %>%
  arrange(sexhead, random) %>% ## sorted by gender
  mutate(strata_index=row_number())

hh_91_practice_exp$treatment_stratified <- as.numeric(hh_91_practice_exp$strata_index <= hh_91_practice_exp$strata_size / 2) 

describeBy(hh_91_practice_exp$sexhead, hh_91_practice_exp$treatment_stratified)
describeBy( hh_91_practice_exp$treatment_stratified, hh_91_practice_exp$sexhead)

# The level of randomization is mainly guided by the nature of the intervention.
# In our microcredit example, the random assignment can be done among households,
# villages or thanas (sub-districts). To avoid contamination, Hawthorne effects
# or John Henry effects, it could be useful to randomize at the village level.
# For this, we need a unique village identifier (vill). 

# We do a clustered random assignment using village ID as the cluster

clust_var <- with(hh_91_practice_exp, hh_91_practice_exp$vill)

#Assign treatment

hh_91_practice_exp$T_vill <- cluster_ra(clust_var = clust_var)

# Internal validity means that the control group provides a valid counterfactual for
# the treatment group. When an assignment process is random, we obtain two
# groups that have a high probability of being statistically identical, so long as
# the size of the experimental group is sufficiently large. We can test the similarity
# between two groups simply by comparing the variables' means prior to the
# program.
# 
# --------------------------------- Validation of the research design ---------------------------
# 
# # A test of equality of means gives us the probability that the observed differences
# in means between the treatment and the control groups prior to the program
# are due to random chance and not to systematic differences. ttest$p.value

t.test(hh_91_practice_exp$sexhead[hh_91_practice_exp$treatment == 1],hh_91_practice_exp$sexhead[hh_91_practice_exp$treatment == 0] ,var.equal = TRUE)

# When the randomization is done at an aggregate level, the error terms are not
# independent. Individuals in the same group may be subject to common shocks.
# In our case, households from the same village may be have similar unobserved
# characteristics. We can measure the intra class correlation within villages.

ICC::ICCest(hh_91_practice_exp$vill, hh_91_practice_exp$exptot, hh_91_practice_exp)

# Here, the intraclass correlation is 13%, which can be considered small. In
# general, to account for the correlation of households within villages, we use a
# technique called clustering

# For example, when validating the research design, the test of equality of means can also 
# be implemented running a regression. Adding the clustered option adjusts the standard errors
# when there is a potential correlation. Again, if the assignment is truly random,
# we should not reject the null hypothesis of equality of means. It is possible to
# regress the treatment variable on a set of pre-treatment variables that we want
# to test. Including many regressors allows us to perform several tests at a time. As
# a rule of thumb, the randomization can be considered successful if we do not
# reject the null hypothesis for 90% of the baseline regressors tested.

model_reg <- lm(hh_91_practice_exp$treatment ~ hh_91_practice_exp$sexhead+hh_91_practice_exp$agehead+hh_91_practice_exp$educhead+hh_91_practice_exp$famsize+hh_91_practice_exp$hhland+hh_91_practice_exp$hhasset, data = hh_91_practice_exp, na.action = na.exclude)      

# call for clsutered SE  by divid
source("Clustered_SE.R")
output <- super.cluster.fun(model_reg, hh_91_practice_exp$vill)
print(output)

#------------------------------------- Power Calculations ---------------------------------------

# Power calculations are a major component of a program evaluation and should
# be computed regardless of the evaluation technique, experimental or not. Power
# calculations indicate the sample size required to detect a given program impact.
# Calculations can be done using a variety of (free) software such as Optimal
# Design. We use R to illustrate the basic procedure

# required sample size
# 
# # We use the baseline data (before the program is implemented) to run power
# calculations. Suppose that your theory of change predicts that the microcredit
# program will increase household consumption (exptot). The first step to calculate
# the required sample size is to propose expected outcome values for the
# counterfactual. We create the local macros mean_0 and sd_0, which contain
# our expectations about the mean and the standard deviation of the outcome
# variable in the absence of the program. In practice, we approximate those
# values using the pre-intervention average of the outcome variable (lnexptot) in
# the baseline dataset.

mean_0 <- mean(hh_91_practice$exptot)
sd_0 <- sd(hh_91_practice$exptot)

# Often, the baseline data is not available prior to the study and you need to
# determine sample size before you collect your own baseline data. For this, you
# can rely on other sources of data from the population that you are interested
# in. These sources must contain the information that you need to estimate the
# mean and the standard deviation of the outcome variable in the absence of
# the program.
# The second step to calculate the required sample size consists in proposing
# the expected outcome values of treatment group in the future. We set the
# macro mean_1 to our expected outcome in the treatment group. To determine
# this value, you need to dig into the literature to find similar program effects that
# have been estimated before. You should be as conservative as possible when
# setting this value; it should correspond to the minimum program effect that you
# are willing to detect. In our case, we assume that on average the microcredit
# program increases the total household expenditures by 600 taka.

mean_1 <- mean_0 + 600

source("sample_calculation.R")
sampsi.mean(mean_0, mean_1, sd = sd_0)

# Under the assumptions stated above, we require 120 households in the treatment
# group and 120 households in the control group to detect any program
# effect larger than 600 taka.

# Clustering and Required Sample Size

# When calculating the required sample size, we also need to account for potential
# correlations between participants. In our example, this corresponds to the
# correlation of households within a village. The higher the correlation, the larger
# the sample size

my_icc_vec <- ICC::ICCest(hh_91_practice$vill, hh_91_practice$exptot)
my_rho <- as.numeric(my_icc_vec[1])

m <- as.numeric(87)

source("sample_calculation.R")
ss <- sampsi.means(mean_0, mean_1, sd_0, power = 0.80)
samp.clus(ss, my_rho, m)

# power of the evaluation

n_0 <- as.numeric(nrow(hh_91_practice_exp[hh_91_practice_exp$treatment==0,]))
n_1 <- as.numeric(nrow(hh_91_practice_exp[hh_91_practice_exp$treatment==1,]))

k <- as.numeric(n_1/n_0)

sampsi.power <- function (muA, muB, nA, nB, kappa , sd, alpha = 0.05)
{
  z=(muA-muB)/(sd*sqrt((1+1/kappa)/nB))
  power=pnorm(z-qnorm(1-alpha/2))+pnorm(-z-qnorm(1-alpha/2))
  return_list_name <<- c("Alpha Two sided", "mA", "mB","sd1", "sd2", "nA", "nB", "nA/nB", "Estimated    Power")
  return_list_val <<-c(alpha, muA, muB, sd, sd, ceiling(nA),ceiling(nB), k, power) 
  return_list <<- as.data.frame(cbind(return_list_name, return_list_val)) 
  return(return_list)
}

sampsi.power(muA = mean_0, muB = mean_1, nA = n_0, nB = n_1, kappa = k, sd = sd_0)

# Under the stated assumptions, if we randomize at the household level assigning
# 145 households to the treatment group and 155 households to the control
# group, and if the program increases the total expenditures by 600 taka, our
# evaluation has a power of 86.27%.
# 

#----------------------------------- Program Impact Estimation ------------------------------------

# Demonstrates how to estimate a program impact using follow-up
# data. You can download the dataset hh_follow-up.dta, which contains information
# about a fictitious randomized evaluation of a microcredit program. The
# variables are similar to the variables in the baseline dataset from the previous
# chapter.
# 
# set working directories

# getwd()
setwd("D:/R/Impact Evaluation Exercise")

# let's load the dataset

hh_follow_up <- read_dta("D:/R/Impact Evaluation Exercise/hh_follow-up.dta")

# let us view the dataset

View(hh_follow_up)

# let us see the basic summary statistic

descr::descr(hh_follow_up)

# mean,median,25th and 75th quartiles,min,max

summary(hh_follow_up)

# item name ,item number, nvalid, mean, sd, 
# median, mad, min, max, skew, kurtosis, se

describe(hh_follow_up)

# The dataset contains 300 observation and has the same variables as the
# Bangladeshi household survey. Additionally, the treatment variable T takes a
# value of 1 if a household benefited from the microcredit program (treatment
# group) and 0 otherwise (control group). There are 150 treated households and
# 150 in the control group.

table(hh_follow_up$T)

# outcome variable

# We expect the microcredit program to increase household consumption in the
# short run. Some of the resources offered by microcredit could have been invested
# into physical or human capital. Our selected outcome variable is the
# natural logarithm of the total expenditures. This transformation facilitates the
# interpretation of the estimates.

hh_follow_up$lnexptot <- log(hh_follow_up$exptot)

# ---------------------------------- Average Treatment Effect -----------------------------------
# 
# In our sample, the assignment to treatment and control is perfectly random. This
# means that the probability of participating in the program for any household in
# the population of interest is independent of the potential gain from the program.
# In this case, the estimation of the Average Treatment Effect (ATE) among the
# potential beneficiaries is a simple difference of means between the treatment
# group and the control group. As discussed before, we can test the difference of
# means between two groups using a linear regression and clustering by village.

t.test(hh_follow_up$lnexptot[hh_follow_up$T == 0],hh_follow_up$lnexptot[hh_follow_up$T == 1] ,var.equal = TRUE)

# The estimation results suggest that, on average, households who benefit from
# microcredit increase their expenditures by 17% . Moreover, this increase is statistically
# significant. We can also run the regression clustering by village.

# We can also run the regression clustering by village.

model_reg_follow_up <- lm(hh_follow_up$lnexptot~hh_follow_up$T)

source("Clustered_SE.R")
output <- super.cluster.fun(model_reg_follow_up, hh_follow_up$vill)
print(output)

# On average, households that benefit from microcredit increase their expenditures
# by 1016 taka compared to non-beneficiaries. In order to improve the
# precision of our estimates, we can add other exogenous variables to the regression.
# When those variables are correlated to the outcome variable (lnexptot or
# exptot) but uncorrelated to the treatment (T ), the confidence interval of the
# estimated program impact becomes smaller. However, if the assignment is truly
# random, the program impact estimate itself should remain unchanged.


# ----------------------- Heterogenerous Impact ------------------------------------

# It is possible that the program impact depend on the characteristics of the microcredit
# beneficiaries. The evidence of heterogeneous effect could help to
# unravel the channels through which the impact is generated.


model_het_follow_up <- lm(hh_follow_up$lnexptot~hh_follow_up$T+hh_follow_up$sexhead+hh_follow_up$educhead+hh_follow_up$famsize)

source("Clustered_SE.R")
output <- super.cluster.fun(model_het_follow_up, hh_follow_up$vill)
print(output)

# Households where the household head is more educated benefit more from
# the microcredit program. This is shown by a positive and significant estimate
# of the variable educhead. One possibility is that educated beneficiaries invest
# resources from the credit into more highly income-generating activities. On average,
# each additional year of education of the household head is associated
# with a 6% increase of the program impact. Larger families appear to benefit
# less from the program. Each additional family member reduces the program
# impact by 4%. Moreover, the gender of the household head is not related to
# the program impact. The p-value associated with the variable sexhead is larger
# than 0.05; therefore, its estimate is not significant

# --------------------------------------- Quantile Treatment Effect -----------------------

# We measure the effect of a program on the mean outcome because we expect
# the program to shift the distribution of this variable. Nonetheless, it is possible
# that the program affect the outcome at other points of its distribution. For example, it could #affect its median (50th percentile) or any other percentile. The
# quantile regression estimates the program effect at any percentile of the outcome
# variable distribution.

quantile_fit <- rq(formula = hh_follow_up$lnexptot ~ hh_follow_up$T, tau = 0.5, data = hh_follow_up)
summary(quantile_fit)

# The program impact on intermediary outcomes and unintended effects are important
# to evaluate. Aside from the outcome variable, other variables may be
# worth looking at. The intermediary outcomes should be determined before the
# empirical work starts. To select them, you can use insights about the program or
# expectations about its impact, for example, the context in which the program
# takes place and what we expect from economic theory. In the case of a microcredit
# program, intermediary outcomes include expenses on productive assets
# and spending on health of adults and children. Besides looking at intermediary
# outcomes, one may want to test whether the program has some unintended effects.
# For example, if women are the recipients of aid the program may impact
# domestic violence; this effect could be positive or negative.
