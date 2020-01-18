########################################################
########################################################
########################################################
#############                              #############
#############     NEURAL NETWORK RESULTS   #############
#############                              #############
########################################################
########################################################
########################################################
# LOAD ALL RELEVANT LIBRARIES:
library(lme4)
library(nlme)
library(boot)
library(car) 
library(reshape2)
library(ggplot2)
library(ez)
library(plyr)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
library(BayesFactor)
library(foreign)
library(dplyr)
library(lattice)
library(openxlsx)
options(scipen=9999)


######################
# data preprocessing #
######################
# load data
D = read.csv(file.choose(), header = TRUE)

# get dimension of D
dim(D)

# test_choice
D$test_choice = revalue(x = as.factor(D$test_choice), 
                        c("0" = "Incorrect", "1"="Correct"))

# memory_check
D$memory_check = revalue(x = as.factor(D$memory_check), 
                        c("0" = "Incorrect", "1"="Correct"))


# age
D$age = revalue(x = as.factor(D$age), 
                    c("0" = "Older", "1"="Younger"))


# subset data to obtain two dataframes for 2 and 3 year olds #
D.2yo = subset(D, ! age %in% c("Older"))    # This creates a subsetted dataframe 
# containing only the data of the 2-year-olds



D.3yo = subset(D, ! age %in% c("Younger")) # This creates a subsetted dataframe 
# containing only the data of the 2-year-olds


####################
####################
# global function  #
####################
####################
glm.global.boot = function(x,data){
  glm.fit = function(data,b,formula){ 
    d= data[b,]
    dif.1 =  glm(d[,x]~1, data=data, family = "binomial")
    return(coef(dif.1))
  }
  glm.Bootobj = boot(data, glm.fit, R=5000)
  return(c(exp(glm.Bootobj$t0),exp(glm.Bootobj$t0)  + 1.96*c(-sd(glm.Bootobj$t), 
                                                             sd(glm.Bootobj$t))))
}

glm.global.boot_modified = function(x,y,data){
  glm.fit = function(data,b,formula){ 
    d= data[b,]
    dif.1 =  glm(d[,x]~d[,y], data=data, family = "binomial")
    return(coef(dif.1))
  }
  glm.Bootobj = boot(data, glm.fit, R=5000)
  return(c(exp(glm.Bootobj$t0[[2]]),exp(glm.Bootobj$t0[[2]])  + 1.96*c(-sd(glm.Bootobj$t), 
                                                                       sd(glm.Bootobj$t))))
}

#####################################
# preliminary analyses and plotting #
#####################################
# get distribution of successes and failure 
table(D$test_choice)


# get proportion (or odds) of success (i.e., 1/0+1), averaged 
# across all predictors
baseline_sucess_prob = table(D$test_choice)[[2]]/(table(D$test_choice)[[1]]+table(D$test_choice)[[2]])
baseline_success_odds = baseline_sucess_prob/(1-baseline_sucess_prob) # this is what will be shown
# as the overall 'odds' for a
# a model in which all the 
# predictors are set to '0'
# more precisely, the log of the
# odds will be what's reported by R
# I.e., log(baseline_success_odds)

# get baseline proportion (or odds) of success (i.e., 1/0+1), for
# the two-year-olds but averaged across all predictors
table(D$test_choice[D$age=="Younger"])
two_year_old_success_prob = table(D$test_choice[D$age=="Younger"])[[2]]/(table(D$test_choice[D$age=="Younger"])[[1]]+
                                                                               table(D$test_choice[D$age=="Younger"])[[2]])

two_year_old_success_odds = two_year_old_success_prob/(1-two_year_old_success_prob) # this is what will be shown
# as the overall 'odds' for a
# a model in which all the 
# predictors are set to '0'
# more precisely, the log of the
# odds will be what's reported by R
# I.e., log(two_year_old_success_odds)


#################
# main analysis #
#################
# predictors/regressors #
# effective_object_demonstrated_first
# feature_appended_to_correct_object
# correct_test_object_location
# memory_check

########################
# response variable(s) #
########################
# test_choice
# memory_check


###########
# METHODS #
###########

## 2-YEAR-OLDS ##
# HUs 
table(D.2yo$test_choice[D.2yo$param_change=="HU"])
HUs.2yo.main = glm(test_choice~1, data=D.2yo,
              subset = D.2yo$param_change=="HU",
              family="binomial")
summary(HUs.2yo.main)

HUs.2yo.main.ORs = exp(coefficients(HUs.2yo.main))
HUs.2yo.main.ORs

HUs.2yo.main.BFs = proportionBF(4,8,p=0.5)
HUs.2yo.main.BFs

# WD
table(D.2yo$test_choice[D.2yo$param_change=="WD"])
WD.2yo.main = glm(test_choice~1, data=D.2yo,
                   subset = D.2yo$param_change=="WD",
                   family="binomial")
summary(WD.2yo.main)

WD.2yo.main.ORs = exp(coefficients(WD.2yo.main))
WD.2yo.main.ORs

WD.2yo.main.BFs = proportionBF(5,8,p=0.5)
WD.2yo.main.BFs

# MM
table(D.2yo$test_choice[D.2yo$param_change=="MM"])
MM.2yo.main = glm(test_choice~1, data=D.2yo,
                  subset = D.2yo$param_change=="MM",
                  family="binomial")
summary(MM.2yo.main)

MM.2yo.main.ORs = exp(coefficients(MM.2yo.main))
MM.2yo.main.ORs

MM.2yo.main.BFs = proportionBF(3,8,p=0.5)
MM.2yo.main.BFs


## 3-YEAR-OLDS ##

# HUs 
HUs.3yo = subset(D.3yo, ! param_change %in% c("MM", "WD"))
HUs.3yo.TAB = table(HUs.3yo$test_choice)
HUs.3yo.chi.square = chisq.test(HUs.3yo.TAB,
                                correct = FALSE)
HUs.3yo.chi.square

HUs.3yo.main.BFs = proportionBF(8,8,p=0.5)
HUs.3yo.main.BFs


# WD
WD.3yo = subset(D.3yo, ! param_change %in% c("HU", "MM"))
WD.3yo.TAB = table(WD.3yo$test_choice)
WD.3yo.chi.square = chisq.test(WD.3yo.TAB,
                                correct = FALSE)
WD.3yo.chi.square

WD.3yo.main.BFs = proportionBF(8,8,p=0.5)
WD.3yo.main.BFs

# MM
MM.3yo = subset(D.3yo, ! param_change %in% c("HU", "WD"))
MM.3yo.TAB = table(MM.3yo$test_choice)
MM.3yo.chi.square = chisq.test(MM.3yo.TAB,
                               correct = FALSE)
MM.3yo.chi.square

MM.3yo.main.BFs = proportionBF(8,8,p=0.5)
MM.3yo.main.BFs