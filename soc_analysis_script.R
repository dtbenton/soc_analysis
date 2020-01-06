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
D = D[1:64,]


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



# reorder columns
D = D[,c(9,1:8)]


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


#######################
# 2-year-old analyses #
#######################
## 2-yo data full model ##
glm.fit.2yo = glm(test_choice~sex+condition+effective_object_demonstrated_first+
                    feature_appended_to_correct_object+correct_test_object_location,
                  data=D.2yo, family="binomial",
                  na.action = na.omit)
summary(glm.fit.2yo)
Anova(glm.fit.2yo)

xtabs(~test_choice, data = D.2yo)
main.glm.fit.2yo = glm(test_choice~1, data=D.2yo, family="binomial")
summary(main.glm.fit.2yo)
glm.global.boot(8,D.2yo) # 95% C


# omnibus 2-yo figure
p.2yo.tc = ggplot(D.2yo, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2yo.tc+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#FF9999","black")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))

##################################
# follow-up analysis: 2-year-old #
##################################
# participants overall odds of responding correctly on the memory_check, 
# where memory_check is the DV
xtabs(~memory_check, data = D.2yo)
glm.2yo.intercept.mem.odds = glm(memory_check ~ 1, data=D.2yo, 
                               family = "binomial")
summary(glm.2yo.intercept.mem.odds)
exp(glm.2yo.intercept.mem.odds$coefficients)
glm.global.boot(9,D.2yo) # 95% CI


xtabs(~test_choice+memory_check,data=D.2yo)
p.2yo = ggplot(D.2yo, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2yo+geom_bar() + theme_bw() + # remove the gray background
  facet_wrap(~memory_check) +
  scale_fill_manual(values= c("#FF9999","black")) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 12))


# follow-up comparisons to examine the marginally significant interaction: 2-year-olds #
# comparing odds of success between 2-year-olds who passed memory check and those who did not #
D.2yo.mcc = subset(D.2yo, ! memory_check %in% c("Incorrect"))
D.2yo.mci = subset(D.2yo, ! memory_check %in% c("Correct"))
dim(D.2yo.mcc)
dim(D.2yo.mci)

# model for 2-yo-mci
D.2yo.mci.glm.fit = glm(test_choice ~ 1, data=D.2yo.mci, 
                        family = "binomial")
summary(D.2yo.mci.glm.fit)
glm.global.boot(8,D.2yo.mci)


# model for 2-yo mcc
D.2yo.mcc.glm.fit = glm(test_choice ~ 1, data=D.2yo.mcc, 
                        family = "binomial")
summary(D.2yo.mcc.glm.fit)
glm.global.boot(8,D.2yo.mcc)


# subset data into memory-check(correct) and memory_check(incorrect) dataframes #
D.mcc = subset(D, ! memory_check %in% c("Incorrect"))
D.mci = subset(D, ! memory_check %in% c("Correct"))


##########################################################
# xtabs for the main effects for the 2-year-old analyses #
##########################################################

# effective_object_demonstrated_first
xtabs(~effective_object_demonstrated_first+test_choice, data = D.2yo)
fisher.test(xtabs(~effective_object_demonstrated_first+test_choice, data = D.2yo))

# feature_appended_to_correct_object
xtabs(~feature_appended_to_correct_object+test_choice, data = D.2yo)
fisher.test(xtabs(~feature_appended_to_correct_object+test_choice, data = D.2yo))

# correct_test_object_location
xtabs(~correct_test_object_location+test_choice, data = D.2yo)
fisher.test(xtabs(~correct_test_object_location+test_choice, data = D.2yo))

# condition
xtabs(~condition+test_choice, data = D.2yo)
fisher.test(xtabs(~condition+test_choice, data = D.2yo))


# overall counts for test_choice
xtabs(~test_choice, data = D.2yo)
chisq.test(xtabs(~test_choice, data = D.2yo), correct = FALSE)

# overall counts for memory_check
xtabs(~memory_check, data = D.2yo)
chisq.test(xtabs(~memory_check, data = D.2yo), correct = FALSE)

## 2-year-old plots ##
# test_choice distribution
p.2yo.tc = ggplot(D.2yo, aes(test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2yo.tc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20)) 

# memory_check distribution
p.2yo.mc = ggplot(D.2yo, aes(memory_check)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
p.2yo.mc+geom_bar(fill=c("#FF9999","black")) + theme_bw() + # remove the gray background
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))

#######################################################################
############ SUMMARY '2-yo data-subsetted full model' #################
#######################################################################
# Although the 2-year-olds did not have a greater log odds of choosing 
# the correct test object compared to the incorrect test object,
# this is NOT because they failed to encode each individual
# first-order relation. These results indicate that 2-year-olds
# simply fail to detect second-order relations based on two, separate
# first-order relations. 

# It is also important to note that none of the other main effects,
# which were subject to counterbalancing, were significant.
########################################################################


#######################
# 3-year-old analyses #
#######################
## 3-yo data full model ##
glm.fit.3yo = glm(test_choice~condition+effective_object_demonstrated_first+
                    memory_check, 
                  data=D.3yo, family="binomial",
                  na.action = na.omit)

summary(glm.fit.3yo)
Anova(glm.fit.3yo)

xtabs(~test_choice, data = D.3yo)
glm.fit.3yo.intercept.tc = glm(test_choice ~ 1, data=D.3yo, 
                               family = "binomial")
summary(glm.fit.3yo.intercept.tc)
glm.global.boot(9,D.3yo) # 95% CI

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
bf.3yo = proportionBF(21, 11+21, p = .5) 

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