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

# mc_status
D$mc_status = revalue(x = as.factor(D$mc_status), 
                c("0" = "MC_Consistent", "1"="MC_Inconsistent"))


# subset data to obtain two dataframes for 2 and 3 year olds #
D.MCinconsistent = subset(D, ! mc_status %in% c("MC_Consistent"))    # This creates a subsetted dataframe 
# containing only the data of the 2-year-olds



D.MCconsistent = subset(D, ! mc_status %in% c("MC_Inconsistent")) # This creates a subsetted dataframe 
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


# get proportion (or odds) of success (i.e., 1/0+1), avermc_statusd 
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
# the two-year-olds but avermc_statusd across all predictors
table(D$test_choice[D$mc_status=="MC_Inconsistent"])
two_year_old_success_prob = table(D$test_choice[D$mc_status=="MC_Inconsistent"])[[2]]/(table(D$test_choice[D$mc_status=="MC_Inconsistent"])[[1]]+
                                                                           table(D$test_choice[D$mc_status=="MC_Inconsistent"])[[2]])

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

#####################################
# Passed MCers: test choices #
#####################################
table(D$test_choice[D$mc_status=="MC_Consistent"])
main_analysis_passed_mc_binom_test = binom.test(32,32, 
                                         p = 0.5)
main_analysis_passed_mc_binom_test


main_analysis_passed_mc_BF = proportionBF(32,32,p=0.5)
main_analysis_passed_mc_BF

#####################################
# failed MCers: test choices #
#####################################
table(D$test_choice[D$mc_status=="MC_Inconsistent"])
main_analysis_failed_mc_binom_test = binom.test(16,32, 
                                                p = 0.5)
main_analysis_failed_mc_binom_test


main_analysis_failed_mc_BF = proportionBF(16,32,p=0.5)
main_analysis_failed_mc_BF



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

# mc_status
D.2$mc_status = revalue(x = as.factor(D.2$mc_status), 
                  c("0" = "MC_Consistent_Nets", "1"="MC_Inconsistent_Nets",
                    "2" = "MC_Consistent_Kids", "3" = "MC_Inconsistent_Nets"))

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

main_passed_threes_nets_kids_chisq_test = chisq.test(xtabs(~test_choice+type,data = D.passed.nets.children)[2,])
main_passed_threes_nets_kids_chisq_test

# BF
main_passed_threes_nets_kids_BF = proportionBF(32,79,p=0.5)
main_passed_threes_nets_kids_BF


# Failed nets vs Passed Humans comparison #
# test choice comparison
main_failed_threes_nets_kids_TAB = xtabs(~test_choice+type,data = D.failed.nets.children)
main_failed_threes_nets_kids_TAB

main_failed_threes_nets_kids_chisq_test = chisq.test(xtabs(~test_choice+type,data = D.failed.nets.children)[2,])
main_failed_threes_nets_kids_chisq_test

# BF
main_failed_threes_nets_kids_BF = proportionBF(16,26,p=0.5)
main_failed_threes_nets_kids_BF