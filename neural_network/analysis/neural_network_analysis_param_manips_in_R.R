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
D$mc_status = revalue(x = as.factor(D$mc_status), 
                    c("0" = "MC_Correct", "1"="MC_Incorrect"))


# subset data to obtain two dataframes for 2 and 3 year olds #
D.MCincorrect = subset(D, ! mc_status %in% c("MC_Correct"))    # This creates a subsetted dataframe 
# containing only the data of the 2-year-olds



D.MCcorrect = subset(D, ! mc_status %in% c("MC_Incorrect")) # This creates a subsetted dataframe 
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
table(D$test_choice[D$mc_status=="MC_Incorrect"])
mc_incorrect_success_prob = table(D$test_choice[D$mc_status=="MC_Incorrect"])[[2]]/(table(D$test_choice[D$mc_status=="MC_Incorrect"])[[1]]+
                                                                               table(D$test_choice[D$mc_status=="MC_Incorrect"])[[2]])

mc_incorrect_success_odds = mc_incorrect_success_prob/(1-mc_incorrect_success_prob) # this is what will be shown
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

## MC_Incorrect ##
# HUs 
table(D.MCincorrect$test_choice[D.MCincorrect$param_change=="HU"])
HUs.MCincorrect.main = binom.test(4,8,p=0.5)
HUs.MCincorrect.main

HUs.MCincorrect.main.BFs = proportionBF(4,8,p=0.5)
HUs.MCincorrect.main.BFs

# WD
table(D.MCincorrect$test_choice[D.MCincorrect$param_change=="WD"])
WD.MCincorrect.main = binom.test(5,8,p=0.5)
WD.MCincorrect.main

WD.MCincorrect.main.BFs = proportionBF(5,8,p=0.5)
WD.MCincorrect.main.BFs

# MM
table(D.MCincorrect$test_choice[D.MCincorrect$param_change=="MM"])
MM.MCincorrect.main = binom.test(5,8,p=0.5)
MM.MCincorrect.main

MM.MCincorrect.main.BFs = proportionBF(3,8,p=0.5)
MM.MCincorrect.main.BFs


## MC_Correct ##

# HUs 
table(D.MCcorrect$test_choice[D.MCcorrect$param_change=="HU"])
HUs.MCcorrect.main = binom.test(8,8,p=0.5)
HUs.MCcorrect.main

HUs.MCcorrect.main.BFs = proportionBF(8,8,p=0.5)
HUs.MCcorrect.main.BFs


# WD
table(D.MCcorrect$test_choice[D.MCcorrect$param_change=="WD"])
WD.MCcorrect.main = binom.test(8,8,p=0.5)
WD.MCcorrect.main

WD.MCcorrect.main.BFs = proportionBF(8,8,p=0.5)
WD.MCcorrect.main.BFs

# MM
table(D.MCcorrect$test_choice[D.MCcorrect$param_change=="MM"])
MM.MCcorrect.main = binom.test(8,8,p=0.5)
MM.MCcorrect.main

MM.MCcorrect.main.BFs = proportionBF(8,8,p=0.5)
MM.MCcorrect.main.BFs