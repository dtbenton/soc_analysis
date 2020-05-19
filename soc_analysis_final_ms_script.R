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
baseline_sucess_prob
baseline_success_odds = baseline_sucess_prob/(1-baseline_sucess_prob) # this is what will be shown
baseline_success_odds                                                                      # as the overall 'odds' for a
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




# get baseline proportion (or odds) of success (i.e., 1/0+1), for
# those who passed the memory check
table(D.passed.mc$test_choice)
passed_mc_success_prob = table(D.passed.mc$test_choice)[[2]]/(table(D.passed.mc$test_choice)[[1]]+
                                                                     table(D.passed.mc$test_choice)[[2]])
passed_mc_success_odds = passed_mc_success_prob/(1-passed_mc_success_prob) 



# get baseline proportion (or odds) of success (i.e., 1/0+1), for
# those who failed the memory check
table(D.failed.mc$test_choice)
failed_mc_success_prob = table(D.failed.mc$test_choice)[[2]]/(table(D.failed.mc$test_choice)[[1]]+
                                                                table(D.failed.mc$test_choice)[[2]])
failed_mc_success_odds = failed_mc_success_prob/(1-failed_mc_success_prob) 



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
mem_check_age_comp = glm(memory_check~age, data = D, 
                         family = "binomial",
                         na.action = na.omit)
summary(mem_check_age_comp)

# 2- vs. 3-year-old MC comparisons #
# ORs
mem_check_age_comp_ORs = exp(coefficients(mem_check_age_comp))
mem_check_age_comp_ORs

# 95% CI
mem_check_age_comp_boot = glm.global.boot_modified(x=10,y=2,D)
mem_check_age_comp_boot

# BF
mem_check_age_alt_model = glm(memory_check~age, data = D, 
                              family = "binomial",
                              na.action = na.omit)

mem_check_age_null_model = glm(memory_check~1, data = D, 
                              family = "binomial",
                              na.action = na.omit)

alt_bic = BIC(mem_check_age_alt_model)
null_bic = BIC(mem_check_age_null_model)

BF10 = 1/exp((alt_bic - null_bic)/2)
BF10




##################################################
# MAIN ANALYSES: COMPARING 2S TO 3S TEST CHOICES #
#################################################
# 3s vs 2s proportion of correct test choices
test_choice_age_comp = glm(test_choice~age, data = D, 
                         family = "binomial",
                         na.action = na.omit)
summary(test_choice_age_comp)


# 2- vs. 3-year-old TC comparisons #
# ORs
test_choice_age_comp_ORs = exp(coefficients(test_choice_age_comp))
test_choice_age_comp_ORs

# 95% CI
mem_check_age_comp_boot = glm.global.boot_modified(x=9,y=2,D)
mem_check_age_comp_boot

# BF
mem_check_age_alt_model_2 = glm(test_choice~age, data = D, 
                              family = "binomial",
                              na.action = na.omit)

mem_check_age_null_model_2 = glm(test_choice~1, data = D, 
                               family = "binomial",
                               na.action = na.omit)

alt_bic_2 = BIC(mem_check_age_alt_model_2)
null_bic_2 = BIC(mem_check_age_null_model_2)

BF10_2 = 1/exp((alt_bic_2 - null_bic_2)/2)
BF10_2




##########################################################################
# MAIN ANALYSES: COMPARING THOSE WHO PASSED AND FAILED THE MEMORY CHECK #
##########################################################################
# Passed vs Failed proportion of correct test choices
table(D$test_choice[D$memory_check=="Correct"])
table(D$test_choice[D$memory_check=="Incorrect"])
main_analysis_mc_binom_test = binom.test(17,20, 
                                      p = 0.5)
main_analysis_mc_binom_test

# BF for 2s vs 3s of correct test choices
proportionBF(17,20,p=0.5)

#####################################
# failed MCers: test choices #
#####################################
# main analysis
failed_mcers_binom_test = binom.test(10,17,p=0.5)
failed_mcers_binom_test

# BF
failed_mcers_binom_test_BF = proportionBF(10,17,p=0.5)
failed_mcers_binom_test_BF



#####################################
# Passed MCers: test choices #
#####################################
# main analysis
passed_mcers_binom_test = binom.test(32,47,p=0.5)
passed_mcers_binom_test

# BF
passed_mcers_binom_test_BF = proportionBF(32,47,p=0.5)
passed_mcers_binom_test_BF



##############################################
# Additional Analyses for Revised Submission #
##############################################
omnibus_chi_square_table = xtabs(~ memory_check + test_choice, data = D)
omnibus_chi_square = chisq.test(omnibus_chi_square_table,correct = FALSE)
omnibus_chi_square

posthoc_chi_square_table = xtabs(~ memory_check + test_choice, data = D)[2,]
posthoc_chi_square_table
posthoc_chi_square = chisq.test(posthoc_chi_square_table, correct = FALSE)
posthoc_chi_square
posthoc_chi_square_BF = proportionBF(32,47,p=0.5)
posthoc_chi_square_BF

main_spearman_analysis_age_test = cor.test(as.numeric(D$test_choice),
                                           D$age,
                                           method="spearman",
                                           data=D)
main_spearman_analysis_age_test

main_spearman_analysis_age_mem = cor.test(as.numeric(D$memory_check),
                                          D$age,
                                          method="spearman",
                                          data=D)
main_spearman_analysis_age_mem