########################################################
########################################################
########################################################
#############                              #############
#############     SOC ANALYSIS SCRIPT      #############
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


# convert relevant numerics to factors
# sex
D$sex = as.factor(D$sex)

# effective_object_demonstrated_first
D$effective_object_demonstrated_first = revalue(x = as.factor(D$effective_object_demonstrated_first), 
                     c("0" = "No", "1"="Yes"))

# feature_appended_to_correct_object
D$feature_appended_to_correct_object = revalue(x = as.factor(D$feature_appended_to_correct_object), 
                                                c("0" = "Circle", "1"="Diamond"))

# correct_test_object_location
D$correct_test_object_location = revalue(x = as.factor(D$correct_test_object_location), 
                                         c("0" = "Left", "1"="Right"))

# test_choice
D$test_choice = revalue(x = as.factor(D$test_choice), 
                                         c("0" = "Incorrect", "1"="Correct"))

# memory_check
D$memory_check = revalue(x = as.factor(D$memory_check), 
                        c("0" = "Incorrect", "1"="Correct"))
# age_cat
D$age_cat = revalue(x = as.factor(D$age_cat), 
                         c("0" = "Younger", "1"="Older"))


# subset data to obtain two dataframes for 2 and 3 year olds #
D.2yo = subset(D, ! age_cat %in% c("Older"))    # This creates a subsetted dataframe 
                                        # containing only the data of the 2-year-olds



D.3yo = subset(D, ! age_cat %in% c("Younger")) # This creates a subsetted dataframe 
                                     # containing only the data of the 2-year-olds



# subset data to obtain two dataframes for those who passed and failed memory check #
D.passed.mc = subset(D, ! memory_check %in% c("Incorrect"))    # This creates a subsetted dataframe 
# containing only the data of those who passed the memory check



D.failed.mc = subset(D, ! memory_check %in% c("Correct")) # This creates a subsetted dataframe 
# containing only the data of those who failed the memory check


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
table(D$test_choice[D$age_cat=="Younger"])
two_year_old_success_prob = table(D$test_choice[D$age_cat=="Younger"])[[2]]/(table(D$test_choice[D$age_cat=="Younger"])[[1]]+
                                                                   table(D$test_choice[D$age_cat=="Younger"])[[2]])

two_year_old_success_odds = two_year_old_success_prob/(1-two_year_old_success_prob) # this is what will be shown
                                                                                    # as the overall 'odds' for a
                                                                                    # a model in which all the 
                                                                                    # predictors are set to '0'
                                                                                    # more precisely, the log of the
                                                                                    # odds will be what's reported by R
                                                                                    # I.e., log(two_year_old_success_odds)

# plot two-year-old (baseline) distributions
p.2y = ggplot(D.2yo, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2y+geom_bar()

# get baseline proportion (or odds) of success (i.e., 1/0+1), for
# the three-year-olds but averaged across all predictors
table(D$test_choice[D$age_cat=="Older"])
three_year_old_success_prob = table(D$test_choice[D$age_cat=="Older"])[[2]]/(table(D$test_choice[D$age_cat=="Older"])[[1]]+
                                                                     table(D$test_choice[D$age_cat=="Older"])[[2]])
three_year_old_success_odds = three_year_old_success_prob/(1-three_year_old_success_prob) # this is what will be shown
                                                                                          # as the overall 'odds' for a
                                                                                          # a model in which all the 
                                                                                          # predictors are set to '0'
                                                                                          # more precisely, the log of the
                                                                                          # odds will be what's reported by R
                                                                                          # I.e., log(two_year_old_success_odds)


# plot three-year-old (baseline) distributions
p.3y = ggplot(D.3yo, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3y+geom_bar()


##########################################################################
# preliminary analysis to determine whether random effects are necessary #
##########################################################################
# the analyses below establish whether a random intercept is needed
mod1 = glmer(test_choice ~ 1 + (1|ID), data=D, family = "binomial")
summary(mod1) # SUMMARY OF MODEL 1
icc_mod1 = mod1@theta[1]^2/ (mod1@theta[1]^2 + (3.14159^2/3)) # ICC of mod1
icc_mod1

mod1_2yo = glmer(test_choice ~ 1 + (1|ID), data=D.2yo, family = "binomial")
summary(mod1_2yo) # SUMMARY OF MODEL 1
icc_mod1_2yo = mod1_2yo@theta[1]^2/ (mod1_2yo@theta[1]^2 + (3.14159^2/3)) # ICC of mod1
icc_mod1_2yo

mod1_3yo = glmer(test_choice ~ 1 + (1|ID), data=D.3yo, family = "binomial")
summary(mod1_3yo) # SUMMARY OF MODEL 1
icc_mod1_3yo = mod1_3yo@theta[1]^2/ (mod1_3yo@theta[1]^2 + (3.14159^2/3)) # ICC of mod1
icc_mod1_3yo

# summary #
# The above analysis indicate that the inclusion of a random-effect intercept for subjects
# is not necessary, given that the random intercepts account for nearly 0 percent of the 
# variance i the log odds of answering the test question correctly. This means that
# the analysis reported below do not include a random-intercept term for subjects.


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

# number of 2-year-olds,
# age range, mean age,
# and distribution of males 
# and females
nrow(D.2yo)
range(D.2yo$age)*12
mean(D.2yo$age)*12
table(D.2yo$sex)

# number of 3-year-olds,
# age range, mean age,
# and distribution of males 
# and females
nrow(D.3yo)
range(D.3yo$age)*12
mean(D.3yo$age)*12
table(D.3yo$sex)


###########################
# MEMORY CHECK COMPARISON #
###########################
#  examining whether the sexes differed in their odds
# of passing the memory check
sex_mc_comparisons = glm(memory_check~sex,
                                  data=D, family="binomial",
                                  na.action = na.omit)
summary(sex_mc_comparisons)
Anova(sex_mc_comparisons)

# intial analysis to examine whether 2- and 3-year-olds
# differed in their odds of passing the memory check question.
mem_check_age_comp = glm(memory_check~age_cat, data = D, 
                         family = "binomial",
                         na.action = na.omit)
summary(mem_check_age_comp)

# 2- vs. 3-year-old MC comparisons #
# ORs
mem_check_age_comp_ORs = exp(coefficients(mem_check_age_comp))
mem_check_age_comp_ORs

# 95% CI
mem_check_age_comp_boot = glm.global.boot(x=10,y=3,D)
mem_check_age_comp_boot

# BF
mem_check_age_comp_conting_tab = xtabs(~ age_cat + memory_check, data = D)
mem_check_age_comp_conting_tab

mem_check_age_comp_BF = contingencyTableBF(mem_check_age_comp_conting_tab,
                                           sampleType = "indepMulti",
                                           fixedMargin = "cols")
mem_check_age_comp_BF


# 2-year-old MC analysis #  

# two-year-olds overall odds of passing memory check
mem_check_two_year_olds = glm(memory_check~1, data = D.2yo, 
                         family = "binomial",
                         na.action = na.omit)
summary(mem_check_two_year_olds)


# 3-year-old MC analysis #  

# three-year-olds overall odds of passing memory check
mem_check_three_year_olds = glm(memory_check~1, data = D.3yo, 
                              family = "binomial",
                              na.action = na.omit)
summary(mem_check_three_year_olds)


# ORS 
mem_check_threes_ORs = exp(coefficients(mem_check_three_year_olds))
mem_check_threes_ORs

# 95% CI
mem_check_threes_boot = glm.global.boot(x=10,D.3yo)
mem_check_threes_boot

mem_check_threes_boot_2 = exp(confint(mem_check_three_year_olds, method="Wald"))
mem_check_threes_boot_2

# BF
mem_check_threes_BF = proportionBF(30,32,p=0.5)
mem_check_threes_BF


####################################
# MAIN ANALYSES COMPARING 2S TO 3S #
####################################
# 3s vs 2s proportion of correct test choices
table(D$test_choice[D$age_cat=="Younger"])
table(D$test_choice[D$age_cat=="Older"])
main_analysis_binom_test = binom.test(14,20, 
                                      p = 0.5)
main_analysis_binom_test

# BF for 2s vs 3s of correct test choices
proportionBF(14,20,p=0.5)


# Correct MC vs Incorrect MC proportion of correct test choices
table(D$test_choice[D$memory_check=="Correct"])
table(D$test_choice[D$memory_check=="Incorrect"])
main_analysis_binom_test_2 = binom.test(17,20, 
                                      p = 0.5,
                                      alternative = "greater")
main_analysis_binom_test_2

# BF for correct MC vs Incorrect MC proportion of correct test choices
proportionBF(17,20,p=0.5)

main_analysis_comparison = glm(test_choice~age_cat,
                               data=D, family="binomial",
                               na.action = na.omit)
summary(main_analysis_comparison)

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
main_analysis_twos_boot = glm.global.boot(x=9,D.2yo)
main_analysis_twos_boot

# BF
main_analysis_twos_BF = proportionBF(13,32,p=0.5)
main_analysis_twos_BF


# analysis for those who failed the memory check
glm.fit.2yo.failed.mc = glm(test_choice~1, data=D.2yo, subset = D.2yo$memory_check=="Incorrect",
                       family="binomial")
summary(glm.fit.2yo.failed.mc)


# analysis for those who passed the memory check
glm.fit.2yo.passed.mc = glm(test_choice~1, data=D.2yo, subset = D.2yo$memory_check=="Correct",
                            family="binomial")
summary(glm.fit.2yo.passed.mc)

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
# This analysis indicated that not only were 2-year-olds as likely to 
# choose the correct test object as they were to choose the incorrect 
# test object, they were also as likely to pass the memory check as
# they were not to pass the memory check. 

# In addition, although there was a marginally significant main effect
# of sex, subsequent Bayes Factor analyses and bootstrapped CIs
# indicate that this difference was not significant. Thus, this
# main effect was not explored further. 

# Finally, those who did pass the memory check were not more likely
# to choose the correct test object than those who did not pass the 
# memory check.
########################################################################


#######################
# 3-year-old analyses #
#######################
# main analysis
main.glm.fit.3yo = glm(test_choice ~ 1, data=D.3yo, 
                               family = "binomial")
summary(main.glm.fit.3yo)

# ORs 
main_analysis_threes_ORs = exp(coefficients(main.glm.fit.3yo))
main_analysis_threes_ORs

# 95% CI
main_analysis_threes_boot = glm.global.boot(x=9,D.3yo)
main_analysis_threes_boot

# BF
table(D.3yo$test_choice)
main_analysis_threes_BF = proportionBF(23,32,p=0.5)
main_analysis_threes_BF

# analysis for those who failed the memory check
glm.fit.3yo.failed.mc = glm(test_choice~1, data=D.3yo, subset = D.3yo$memory_check=="Incorrect",
                            family="binomial")
summary(glm.fit.3yo.failed.mc) # Note that this analysis doesn't make sense because only 1 child 
                               # failed the memory check. 


# analysis for those who passed the memory check
glm.fit.3yo.passed.mc = glm(test_choice~1, data=D.3yo, subset = D.3yo$memory_check=="Correct",
                            family="binomial")
summary(glm.fit.3yo.passed.mc)

# ORs 
glm.fit.3yo.passed.mc.ORs = exp(coefficients(glm.fit.3yo.passed.mc))
glm.fit.3yo.passed.mc.ORs

# 95% CI
D.3yo.correct.mc = subset(D.3yo, ! memory_check %in% c("Incorrect"))
D.3yo.correct.mc.boot = glm.global.boot(x=9,D.3yo.correct.mc)
D.3yo.correct.mc.boot

# BF
table(D.3yo.correct.mc$test_choice)
D.3yo.correct.mc.BF = proportionBF(8,30,p=0.5)
D.3yo.correct.mc.BF



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
# The analyses indicates that, unlike the 2-year-olds, 3-year-olds were
# both more likely to choose the correct test object and more likely to 
# pass the memory check, which indicates that they encoded both the first-
# and second-order relations between the objects, the features, and the 
# machines' activation. 

# Together, these results indicate a developmental progression in children's
# ability to detect SOCs involving an objects' internal feature and its
# causal efficacy. 
########################################################################