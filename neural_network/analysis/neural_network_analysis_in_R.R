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

####################################
# MAIN ANALYSES COMPARING 2S TO 3S #
####################################
main_analysis_comparison = glm(test_choice~(age+condition)^2,
                               data=D, family="binomial",
                               na.action = na.omit)
summary(main_analysis_comparison)
Anova(main_analysis_comparison)

# ORs 
main_analysis_ORs = exp(coefficients(main_analysis_comparison))
main_analysis_ORs


# 95% CIs
main_analysis_CIs = glm.global.boot_modified(9, 3, D)
main_analysis_CIs

# BF
main_analysis_conting_tab = xtabs(~ age_cat + test_choice, data = D)
main_analysis_conting_tab

main_analysis_conting_BF = contingencyTableBF(main_analysis_conting_tab,
                                              sampleType = "indepMulti",
                                              fixedMargin = "cols")
main_analysis_conting_BF


#####################################
# 2-year-old analyses: test choices #
#####################################
# main analysis
main.glm.fit.2yo = glm(test_choice~1, data=D.2yo, family="binomial")
summary(main.glm.fit.2yo)

# ORs 
main_analysis_twos_ORs = exp(coefficients(main.glm.fit.2yo))
main_analysis_twos_ORs

# 95% CI
main_analysis_twos_boot = glm.global.boot(x=4,D.2yo)
main_analysis_twos_boot

# BF
main_analysis_twos_BF = proportionBF(11,20,p=0.5)
main_analysis_twos_BF


#############################
# 2-year-old omnibus figure #
#############################

# omnibus 2-yo figure
p.2yo.tc = ggplot(D.2yo, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2yo.tc+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#FF9999","black")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))



#######################################################################
############ SUMMARY '2-yo data-subsetted full model' #################
#######################################################################
# This analysis is consistent with the results from Experiment 1, in which
# it was shown that 2-year-olds were equally likely to choose the correct and
# incorrect test objects. 
########################################################################


#######################
# 3-year-old analyses #
#######################
# main analysis
main.glm.fit.3yo.tabs = xtabs(~test_choice, data = D.3yo)
main.glm.fit.3yo.tabs

main.glm.fit.3yo = chisq.test(main.glm.fit.3yo.tabs,
                              correct = FALSE)
main.glm.fit.3yo

# BF
table(D.3yo$test_choice)
main_analysis_threes_BF = proportionBF(20,20,p=0.5)
main_analysis_threes_BF


# omnibus 3-yo memory-check figure
p.3yo.mc = ggplot(D.3yo, aes(memory_check)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3yo.mc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))



#######################################################################
############ SUMMARY '3-yo data-subsetted full model' #################
#######################################################################
# The analyses indicates that, unlike the younger networks, the older networks
# were both more likely to choose the correct test object and more likely to 
# pass the memory check, which indicates that they encoded both the first-
# and second-order relations between the objects, the features, and the 
# machines' activation. 

# Together, these results indicate a developmental progression in children's
# ability to detect SOCs involving an objects' internal feature and its
# causal efficacy. 
########################################################################