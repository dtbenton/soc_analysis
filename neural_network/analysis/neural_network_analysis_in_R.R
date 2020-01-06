############################################################
############################################################
############################################################
#############                                  #############
#############     NETWORK ANALYSIS SCRIPT      #############
#############                                  #############
############################################################
############################################################
############################################################
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

# get structure of the D
str(D)

# convert relevant numerics to factors

# age
D$age = revalue(x = as.factor(D$age), 
                c("0" = "Older", "1"="Younger"))

# test_choice
D$test_choice = revalue(x = as.factor(D$test_choice), 
                        c("0" = "Incorrect", "1"="Correct"))


# subset data to obtain two dataframes for 2 and 3 year olds #
D.2yo = subset(D, ! age %in% c("Older"))    # This creates a subsetted dataframe 



D.3yo = subset(D, ! age %in% c("Younger")) # This creates a subsetted dataframe 


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

# plot two-year-old (baseline) distributions
p.2y = ggplot(D.2yo, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2y+geom_bar()

# get baseline proportion (or odds) of success (i.e., 1/0+1), for
# the three-year-olds but averaged across all predictors
table(D$test_choice[D$age=="Older"])
three_year_old_success_prob = table(D$test_choice[D$age=="Older"])[[2]]/(table(D$test_choice[D$age=="Older"])[[1]]+
                                                                     table(D$test_choice[D$age=="Older"])[[2]])
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
# age
# condition

########################
# response variable(s) #
########################
# test_choice


# no data-subsetted, intercept-only model
# this analysis examines the overall log odds of choosing
# the correct object averaged over all variables
glm.fit = glm(test_choice~1, data=D, family="binomial")
summary(glm.fit)


###############################
# global analysis#
###############################
names(D)
main.glm.fit = glm(test_choice~(age+condition)^2, data=D, family="binomial")
summary(main.glm.fit)
Anova(main.glm.fit)


#######################
# 2-year-old analyses #
#######################
## 2-yo data full model ##
glm.fit.2yo = glm(test_choice~condition,
                  data=D.2yo, family="binomial",
                  na.action = na.omit)
summary(glm.fit.2yo)
Anova(glm.fit.2yo)

xtabs(~test_choice, data = D.2yo)
main.glm.fit.2yo = glm(test_choice~1, data=D.2yo, family="binomial")
summary(main.glm.fit.2yo)
set.seed(2020)
glm.global.boot(4,D.2yo) # 95% C


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
# These analyses indicate that the younger networks, who correspond to 2-year-olds,
# were as likely to respond correctly as they were to respond incorrectly at test.
# This finding is in line with the data from the 2-year-olds who also were at chance
# in their test choice. 
########################################################################


#######################
# 3-year-old analyses #
#######################
## 3-yo data full model ##
glm.fit.3yo = glm(test_choice~condition, 
                  data=D.3yo, family="binomial",
                  na.action = na.omit)

summary(glm.fit.3yo)
Anova(glm.fit.3yo)

xtabs(~test_choice, data = D.3yo)
glm.fit.3yo.intercept.tc = glm(test_choice ~ 1, data=D.3yo, 
                               family = "binomial")
summary(glm.fit.3yo.intercept.tc)
glm.global.boot(4,D.3yo) # 95% CI

# omnibus 3-yo figure
p.3yo.tc = ggplot(D.3yo, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3yo.tc+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#FF9999","black")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))




# participants overall odds of responding correctly on the memory check
xtabs(~memory_check, data = D.3yo)
glm.fit.3yo.intercept.mc = glm(memory_check ~ 1, data=D.3yo, 
                               family = "binomial")
summary(glm.fit.3yo.intercept.mc)
glm.global.boot(9,D.3yo) # 95% CI


# omnibus 3-yo memory-check figure
p.3yo.mc = ggplot(D.3yo, aes(memory_check)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3yo.mc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))



## 3-year-old memory check subset analyses ##
# subset data into memory-check(correct) and memory_check(incorrect) dataframes
D.3yo.mcc = subset(D.3yo, ! memory_check %in% c("Incorrect"))
D.3yo.mci = subset(D.3yo, ! memory_check %in% c("Correct"))
dim(D.3yo.mcc)
dim(D.3yo.mci)

# participants overall odds of choosing the correc test object
xtabs(~test_choice, data = D.3yo)
glm.fit.3yo.mcc = glm(test_choice ~ 1, data=D.3yo.mcc, 
                      family = "binomial")
summary(glm.fit.3yo.mcc)
glm.global.boot(8,D.3yo.mcc)


# omnibus 3-yo memory-check figure
xtabs(~test_choice, data = D.3yo.mcc)
p.3yo.mc = ggplot(D.3yo.mcc, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3yo.mc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 18))


##########################################################
# xtabs for the main effects for the 3-year-old analyses #
##########################################################

# effective_object_demonstrated_first
xtabs(~effective_object_demonstrated_first+test_choice, data = D.3yo)
fisher.test(xtabs(~effective_object_demonstrated_first+test_choice, data = D.3yo))

# feature_appended_to_correct_object
xtabs(~feature_appended_to_correct_object+test_choice, data = D.3yo)
fisher.test(xtabs(~feature_appended_to_correct_object+test_choice, data = D.3yo))

# correct_test_object_location
xtabs(~correct_test_object_location+test_choice, data = D.3yo)
fisher.test(xtabs(~correct_test_object_location+test_choice, data = D.3yo))

# condition
xtabs(~condition+test_choice, data = D.3yo)
fisher.test(xtabs(~condition+test_choice, data = D.3yo))


# overall counts for test_choice
xtabs(~test_choice, data = D.3yo)
chisq.test(xtabs(~test_choice, data = D.3yo), correct = FALSE)

# overall counts for memory_check
xtabs(~memory_check, data = D.3yo)
chisq.test(xtabs(~memory_check, data = D.3yo), correct = FALSE)


## 3-year-old plots ##
# test_choice distribution
p.3yo.tc = ggplot(D.3yo, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3yo.tc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20)) +
  
  
  # memory_check distribution
  p.3yo.mc = ggplot(D.3yo, aes(memory_check)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.3yo.mc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))


#######################################################################
############ SUMMARY '3-yo data-subsetted full model' #################
#######################################################################
# The analyses, at present, indicate that the 3-year-olds neither 
# had a greater log odds of choosing the correct test object 
# compared to the incorrect test object nor did they detect second-order 
# relations based on two, separate first-order relations. 

# This analysis should be excepted with a grain of salt given that
# the full sample size (N = 32) has not been collected.
########################################################################