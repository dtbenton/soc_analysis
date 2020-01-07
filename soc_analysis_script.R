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
library(foreign)
library(dplyr)
library(lattice)
options(scipen=9999)

######################
# data preprocessing #
######################
# load data
D = read.csv(file.choose(), header = TRUE)

# get dimension of D
dim(D)
D = D[1:63,]


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

glm.global.boot_modified = function(x,z,data){
  glm.fit = function(data,b,formula){ 
    d= data[b,]
    dif.1 =  glm(d[,x]~z, data=data, family = "binomial")
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

########################
# response variable(s) #
########################
# test_choice
# memory_check

# no data-subsetted, intercept-only model
# this analysis examines the overall log odds of choosing
# the correct object averaged over all variables
glm.fit = glm(test_choice~1, data=D, family="binomial")
summary(glm.fit)


###############################
# global analysis#
###############################
names(D)
main.glm.fit = glm(test_choice~(age_cat+memory_check)^2, data=D, family="binomial")
summary(main.glm.fit)
Anova(main.glm.fit)


#####################################
# 2-year-old analyses: test choices #
#####################################
## 2-yo data full model ##
glm.fit.2yo = glm(test_choice~sex+condition+correct_test_object_location,
                  data=D.2yo, family="binomial",
                  na.action = na.omit)
summary(glm.fit.2yo)
Anova(glm.fit.2yo)
bf.2yo.sex = proportionBF(21, 11+21, p = .5) 
xtabs(~test_choice, data = D.2yo)
main.glm.fit.2yo = glm(test_choice~1, data=D.2yo, family="binomial")
summary(main.glm.fit.2yo)

set.seed(2020)
glm.global.boot(8,D.2yo) # 95% C

two.yo.sex.conting = xtabs(~test_choice+sex,data=D.2yo)
bf.2yo.sex = contingencyTableBF(two.yo.sex.conting,
                                sampleType = "indepMulti", 
                                fixedMargin = "cols") # this analysis is to explore the marginally
                                                      # significant effect of sex for the 2-year-olds.
                                                      # This analysis indicated that male and females did
                                                      # did not differ in their test choices.
set.seed(2020)
glm.global.boot_modified(9,D.2yo$sex,D.2yo) # this is supplementary to further show that there was no difference
                                            # between the male and female infants given that the confidence interval
                                            # contains 1. 


# omnibus 2-yo figure
p.2yo.tc = ggplot(D.2yo, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2yo.tc+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#FF9999","black")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))



#####################################
# 2-year-old analyses: memory check #
#####################################
# participants overall odds of responding correctly on the memory_check, 
# where memory_check is the DV
xtabs(~memory_check, data = D.2yo)
glm.2yo.intercept.mem.odds = glm(memory_check ~ 1, data=D.2yo, 
                               family = "binomial")
summary(glm.2yo.intercept.mem.odds)
set.seed(2020)
glm.global.boot(9,D.2yo) # 95% CI


# examines whether participants who passed the memory check were more likely to 
# choose the correct test object
xtabs(~test_choice+memory_check,data=D.2yo)
glm.2yo.mem.test.odds = glm(test_choice ~ memory_check, data=D.2yo, 
                                 family = "binomial")
summary(glm.2yo.mem.test.odds)
Anova(glm.2yo.mem.test.odds)

set.seed(2020)
glm.global.boot(9,D.2yo) # 95% CI



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
## 3-yo data full model ##
glm.fit.3yo = glm(test_choice~condition+memory_check, 
                  data=D.3yo, family="binomial",
                  na.action = na.omit)

summary(glm.fit.3yo)
Anova(glm.fit.3yo)

xtabs(~test_choice, data = D.3yo)
glm.fit.3yo.intercept.tc = glm(test_choice ~ 1, data=D.3yo, 
                               family = "binomial")
summary(glm.fit.3yo.intercept.tc)
set.seed(2020)
glm.global.boot(9,D.3yo) # 95% CI
bf.3yo.intercept = proportionBF(22,22+9,p=0.5) # this analysis indicates that 
                                               # that there was overwhelming evidence
                                               # in favor of the alternative hypothesis
                                               # that 3-year-olds were more likely to choose
                                               # the correct test object.


# participants overall odds of responding correctly on the memory check
xtabs(~memory_check, data = D.3yo) # this analysis indicates that all but 1 3-year-old
                                    # passed the memory check
glm.fit.3yo.intercept.mc = glm(memory_check ~ 1, data=D.3yo, 
                               family = "binomial")
summary(glm.fit.3yo.intercept.mc)
set.seed(2020)
glm.global.boot(9,D.3yo) # 95% CI
bf.3yo.memory.check = proportionBF(30,30+1,p=0.5) # this analysis also indicated that there was overwhelming evidence
                                                  # that the 3-year-olds were more likely to pass the memory check
                                                  # than not to pass the memory check.


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