########################################################
########################################################
########################################################
#############                              #############
#############     SOC ANALYSIS SCRIPT      #############
#############                              #############
########################################################
########################################################
########################################################
# load all relevant libraries:
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
options(scipen=9999)

######################
# data preprocessing #
######################
# load data
D = read.csv(file.choose(), header = TRUE)

# get dimension of D
dim(D)

# subset D with appropriate rows
D = D[1:48,]


# get structure of the D
str(D)

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

# age 
D$age = as.factor(D$age)

# subset data to obtain two dataframes for 2 and 3 year olds #
D.2yo = subset(D, ! age %in% c("3"))    # This creates a subsetted dataframe 
                                        # containing only the data of the 2-year-olds
D.2yo = subset(D.2yo, ! memory_check %in% c("Incorrect"))


D.3yo = subset(D, ! age %in% c("2")) # This creates a subsetted dataframe 
                                     # containing only the data of the 2-year-olds
D.3yo = subset(D.3yo, ! memory_check %in% c("Incorrect"))


# remove original ID column and replace it with a cleaner version
D$ID = NULL
D$ID = c(1:48)

# reorder columns
D = D[,c(9,1:8)]


####################
####################
# global function  #
####################
####################
glmer_global_boot = function(x){ # 'x' is
  glmer.fit = x                  # e.G., x = glmer(y~x+(1|ID),data=d,family="binomial")
  
  glmer.fun = function(fit){
    return(fixef(fit))
  }
  result = bootMer(glmer.fit, glmer.fun, nsim = 4000)
  return(result)
} # This function enables users to obtain
  # the parameters needed to compute the 95% bootstrapped
  # confidence intervals for the coefficients/predictors
  # of a generalized linear-mixed effects model
  # Note that the formula to obtain the 95% Bootstrapped CI is
  # original + 1.96*(-std.error,std.error)



lmer_global_boot = function(x){
  lmer.fit = x              
  
  lmer.fun = function(fit){
    return(fixef(fit))
  }
  result = bootMer(lmer.fit, lmer.fun, nsim = 4000)
  return(result)
} # This function is identical to the one above
  # except that it's used to obtain CIs for
  # linear mixed-effects model


glm_global_boot = function(x){ # 'x' is
  glm.fit = x                  # e.G., x = glmer(y~x+(1|ID),data=d,family="binomial")
  
  glm.fun = function(fit){
    return(fit$coefficients)
  }
  result = bootMer(glm.fit, glm.fun, nsim = 4000)
  return(result)
}

glm.global.boot = function(D,formula){
  glm.boot = function(formula, D, indices) {
    d = data[indices,] # allows boot to select sample
    fit = glm(formula, data=d, family="binomial")
    return(summary(fit)$coefficients)
  }
  
  results = boot(data=D,statistic=glm.boot, R=1000,formula=formula)
  return(results)
}

glm.boot = function(formula, data) {
  d = data[indices,] # allows boot to select sample
  fit = glm(formula, data=d, family="binomial")
  return(summary(fit)$coefficients)
}


  


#####################################
# preliminary analyses and plotting #
#####################################

# get distribution of successes and failure 
table(D$test_choice)


# get proportion (or odds) of success (i.e., 1/0+1), averaged 
# across all predictors
baseline_sucess_prob = 29/(19+29)
baseline_success_odds = baseline_sucess_prob/(1-baseline_sucess_prob) # this is what will be shown
                                                                      # as the overall 'odds' for a
                                                                      # a model in which all the 
                                                                      # predictors are set to '0'
                                                                      # more precisely, the log of the
                                                                      # odds will be what's reported by R
                                                                      # I.e., log(baseline_success_odds)

# get proportion (or odds) of success (i.e., 1/0+1), for
# the two-year-olds but averaged across all predictors
table(D$test_choice[D$age==2])
two_year_old_success_prob = 20/(10+20)
two_year_old_success_odds = two_year_old_success_prob/(1-two_year_old_success_prob) # this is what will be shown
                                                                                    # as the overall 'odds' for a
                                                                                    # a model in which all the 
                                                                                    # predictors are set to '0'
                                                                                    # more precisely, the log of the
                                                                                    # odds will be what's reported by R
                                                                                    # I.e., log(two_year_old_success_odds)


# get proportion (or odds) of success (i.e., 1/0+1), for
# the three-year-olds but averaged across all predictors
table(D$test_choice[D$age==3])
three_year_old_success_prob = 9/(18)
three_year_old_success_odds = three_year_old_success_prob/(1-three_year_old_success_prob) # this is what will be shown
                                                                                          # as the overall 'odds' for a
                                                                                          # a model in which all the 
                                                                                          # predictors are set to '0'
                                                                                          # more precisely, the log of the
                                                                                          # odds will be what's reported by R
                                                                                          # I.e., log(two_year_old_success_odds)


# plot initial distributions
p = ggplot(D, aes(test_choice, fill = condition)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p+geom_bar() 

p = ggplot(D, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p+geom_bar()

##########################################################################
# preliminary analysis to determine whether random effects are necessary #
##########################################################################
mod1 = glmer(test_choice ~ 1 + (1|ID), data=D, family = "binomial")
summary(mod1) # SUMMARY OF MODEL 1
icc_mod1 = mod1@theta[1]^2/ (mod1@theta[1]^2 + (3.14159^2/3)) # ICC of mod1

mod1_2yo = glmer(test_choice ~ 1 + (1|ID), data=D.2yo, family = "binomial")
summary(mod1_2yo) # SUMMARY OF MODEL 1
icc_mod1_2yo = mod1_2yo@theta[1]^2/ (mod1_2yo@theta[1]^2 + (3.14159^2/3)) # ICC of mod1

mod1_3yo = glmer(test_choice ~ 1 + (1|ID), data=D.3yo, family = "binomial")
summary(mod1_3yo) # SUMMARY OF MODEL 1
icc_mod1_3yo = mod1_3yo@theta[1]^2/ (mod1_3yo@theta[1]^2 + (3.14159^2/3)) # ICC of mod1

# determine whether inclusion of the subject-level random effect leads to a better model
null_model = glm(test_choice ~ 1, data=D, family = "binomial")
anova(icc_mod1,null_model)
anova(icc_mod1_2yo,null_model)
anova(icc_mod1_3yo,null_model)

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

# response variable #
# test_choice

# no data-subsetted, intercept-only model
glm.fit = glm(test_choice~1, data=D, family="binomial")
summary(glm.fit)


# 2-yo data-subsetted full model
glm.fit.2yo = glm(test_choice~sex+condition+effective_object_demonstrated_first+
                    feature_appended_to_correct_object+correct_test_object_location,
                  data=D.2yo, family="binomial",
                  na.action = na.omit)

summary(glm.fit.2yo)

# summary of '2-yo data-subsetted full model' #
# Because none of the predictors were significant, report 
# only the intercept-only model.

# 2-yo data-subsetted, intercept-only model
glm.fit.2yo.intercept = glm(test_choice ~ 1, data=D.2yo, 
                            family = "binomial")

summary(glm.fit.2yo.intercept)
glm_global_boot(glm.fit.2yo.intercept)



# summary of '2-yo data-subsetted, intercept-only model' #
# This analysis indicates that children were marginally more like.


# 3-yo data-subsetted model
glm.fit.3yo = glm(test_choice~effective_object_demonstrated_first+
                    feature_appended_to_correct_object+
                    correct_test_object_location+
                    memory_check, 
                  data=D.3yo, family="binomial",
                  na.action = na.omit)

summary(glm.fit.3yo)

# 3-yo data-subsetted, intercept-only model
glm.fit.3yo.intercept = glm(test_choice ~ 1, data=D.3yo, 
                            family = "binomial")

summary(glm.fit.3yo.intercept)
glm_global_boot(glm.fit.3yo.intercept)

