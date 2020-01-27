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


# subset data for those who failed the memory check
D.failed.mc = subset(D, ! memory_check %in% c("Correct"))


# subset data for those who passed the memory check
D.passed.mc = subset(D, ! memory_check %in% c("Incorrect"))

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

##########################################################################
# MAIN ANALYSES: COMPARING THOSE WHO PASSED AND FAILED THE MEMORY CHECK #
##########################################################################
# Passed vs Failed proportion of correct test choices
table(D$test_choice[D$memory_check=="Correct"])
table(D$test_choice[D$memory_check=="Incorrect"])
main_analysis_mc_binom_test = binom.test(20,22, 
                                         p = 0.5)
main_analysis_mc_binom_test

# BF for 2s vs 3s of correct test choices
proportionBF(17,20,p=0.5)

#####################################
# failed MCers: test choices #
#####################################
# main analysis
main.glm.fit.failed.mc = glm(test_choice~1, data=D.failed.mc, family="binomial")
summary(main.glm.fit.failed.mc)

# ORs 
main.glm.fit.failed.mc_ORs = exp(coefficients(main.glm.fit.failed.mc))
main.glm.fit.failed.mc_ORs

# 95% CI
main.glm.fit.failed.mc_boot = glm.global.boot(x=9, D.failed.mc)
main.glm.fit.failed.mc_boot

# BF
main.glm.fit.failed.mc_BF = proportionBF(10,17,p=0.5)
main.glm.fit.failed.mc_BF



#####################################
# Passed MCers: test choices #
#####################################
# main analysis
main.glm.fit.passed.mc = glm(test_choice~1, data=D.passed.mc, family="binomial")
summary(main.glm.fit.passed.mc)

main.glm.fit.passed.mc.binom.test = binom.test(20,20,p=0.5)
main.glm.fit.passed.mc.binom.test

# ORs 
main.glm.fit.passed.mc_ORs = exp(coefficients(main.glm.fit.passed.mc))
main.glm.fit.passed.mc_ORs

# 95% CI
main.glm.fit.passed.mc_boot = glm.global.boot(x=4, D.passed.mc)
main.glm.fit.passed.mc_boot

# BF
main.glm.fit.passed.mc_BF = proportionBF(20,20,p=0.5)
main.glm.fit.passed.mc_BF


#############################
# CROSS EXPERIMENT ANALYSIS #
#############################
D.2 = read.csv(file.choose(), header = TRUE)

# get dimension of D
dim(D)

# test_choice
D.2$test_choice = revalue(x = as.factor(D.2$test_choice), 
                          c("0" = "Incorrect", "1"="Correct"))

# memory_check
D.2$memory_check = revalue(x = as.factor(D.2$memory_check), 
                           c("0" = "Incorrect", "1"="Correct"))

# age
D.2$age = revalue(x = as.factor(D.2$age), 
                  c("0" = "Older_Nets", "1"="Younger_Nets",
                    "2" = "Twos", "3" = "Threes"))

# type
D.2$type = revalue(x = as.factor(D.2$type), 
                 c("0" = "Network", "1"="Human"))

# subset data into different dataframe
D.passed.nets.children = subset(D.2, ! memory_check %in% c("Incorrect"))
D.passed.nets.children$memory_check = factor(D.passed.nets.children$memory_check) 
str(D.passed.nets.children)

D.failed.nets.children = subset(D.2, ! memory_check %in% c("Correct"))
D.failed.nets.children$memory_check = factor(D.failed.nets.children$memory_check) 
str(D.failed.nets.children)

# Passed nets vs Passed Humans comparison #
# test choice comparison
main_passed_threes_nets_kids_TAB = xtabs(~test_choice+type,data = D.passed.nets.children)
main_passed_threes_nets_kids_TAB

main_passed_threes_nets_kids_binom_test = binom.test(20,37,p=0.5)
main_passed_threes_nets_kids_binom_test

# BF
main_passed_threes_nets_kids_BF = proportionBF(20,37,p=0.5)
main_passed_threes_nets_kids_BF


# Failed nets vs Passed Humans comparison #
# test choice comparison
main_failed_threes_nets_kids_TAB = xtabs(~test_choice+type,data = D.failed.nets.children)
main_failed_threes_nets_kids_TAB

main_failed_threes_nets_kids_binom_test = binom.test(3,5,p=0.5)
main_failed_threes_nets_kids_binom_test

# BF
main_failed_threes_nets_kids_BF = proportionBF(3,5,p=0.5)
main_failed_threes_nets_kids_BF