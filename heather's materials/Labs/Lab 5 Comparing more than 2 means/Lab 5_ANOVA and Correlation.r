# For next time: find more interesting data than the titanic data
# Add a levene's test for equality of variances
# Make sure answers to exercises are fleshed out and that they provide practice with interpretations

#############################
# NRES 776
# Lab 5: ANOVA and Correlation
#############################

#############################
# Topics
#############################
# Comparing more than two means
#   One-way ANOVA with fixed effects
#   Kruskal-Wallis test
# Correlation
#   Pearson's Corrrelation Test
#   Spearman's Rank Correlation
#############################

###############################################
# Materials
###############################################
# Source code from Chapter 13 of Whitlock and Schluter
# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_13.html
# Labs using R: 
# https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_ANOVA.html
# https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Correlation_Regression.html
###############################################


###############################################
# Load required packages
###############################################
library(dplyr)
library(ggplot2)
library(PMCMRplus) # will probably need to install this package first
library(confintr) # will probably need to install this package first
###############################################

#############################
# Relevant R Datacamp Modules
#############################
# Same modules as recommended last week
# Hypothesis Testing in R: Introduction to ANOVA and rank-based tests
# Introduction to Statistics in R: See portion on correlation
#############################

####################################
# Set working directory
####################################
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 5 Comparing more than 2 means/")
####################################

#################################################
# One-way ANOVA with fixed effects
#################################################

# We'll use the titanic data again
# This time we'll compare whether the mean age of passengers differed by traveling class (1st, 2nd, or 3rd)
titanicData <- read.csv("titanic.csv")

# Look at assumptions
ggplot(titanicData, aes(x = age)) +   
  geom_histogram() + 
  facet_wrap(~ passenger_class, ncol = 1) 

# Summarize data to look at means and standard deviations (to help with assessing any violations of assumptions)
# Use the dplyr package
# remove cases where age is missing (NA)
titanicData2<-subset(titanicData,!is.na(titanicData$age))
# group_by() groups the data frame by the appropriate grouping variable
titanic_by_passenger_class <- group_by(titanicData2,passenger_class)
# summarize puts the data into a table
titanic_by_passenger_classTable<-summarise(titanic_by_passenger_class, group_mean = mean(age, na.rm=TRUE), group_sd = sd(age,na.rm=T), group_se = sd(age,na.rm=T) / sqrt(n()),n=n())
titanicDF<-as.data.frame(titanic_by_passenger_classTable)
titanicDF

# Error bar plot
ggplot(titanicDF, aes(x = passenger_class, y = group_mean)) +
  geom_errorbar(aes(ymin=group_mean-2*group_se, ymax=group_mean+2*group_se), width=.2) + 
  geom_point(size=3,colour="firebrick") +
  labs(x ="Passenger Class" , y = "Age (years) +/- 2*SE") + 
  theme_classic()

# To run an ANOVA, we'll first use the lm() command, which fits a linear model
# Note: an ANOVA is a type of linear model
# Here, you use a formula to specify the model
# age is the response variable and you want to model effects of passenger class (explanatory variable)
# you also have to specify the data (titanicData in this case)
titanicANOVA <- lm(age ~ passenger_class, data = titanicData)

# After running the linear model, we will now cast the results into an ANOVA table that we are familiar with
anova(titanicANOVA)

#################################################
# Tukey-Kramer Test
#################################################

# Looks like results from the ANOVA are significant
# We can run a Tukey-Kramer test (aka Tukey Honest Significant Difference Test) to figure out which means are different
# Use the functions aov() and TukeyHSD()

TukeyHSD(aov(titanicANOVA))

# What would your conclusions and interpretations be from this test? (Hint: look at the initial descriptive table)


#################################################
# Kruskal-Wallis Test
#################################################

# Non-parametric alternative to a t-test
kruskal.test(age ~ passenger_class, data = titanicData)

# We could do a post-hoc test here too
# use the Dunn test because sample sizes are unequal
table(titanicData2$passenger_class)
# need to make sure passenger_class is a factor for the paired comparisons to work
titanicData2$passenger_class<-factor(titanicData2$passenger_class)
kwAllPairsDunnTest(age ~ passenger_class, data = titanicDataw)

###############################################
# Pearson's Correlation
###############################################

# Guppy Data
# Is there a relationship between a father's ornamentation and the attractiveness of his sons to females?
guppyData <- read.csv("chap02f3_3GuppyFatherSonAttractiveness.csv")

# Scatterplot of the data
ggplot(guppyData, aes(x=fatherOrnamentation, 
                      y=sonAttractiveness)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness")

# Get the correlation coefficient
cor(guppyData$sonAttractiveness, guppyData$fatherOrnamentation)

# Get the confidence interval for the correlation coefficient
ci_cor(guppyData$sonAttractiveness, guppyData$fatherOrnamentation, method = "pearson", type = "bootstrap", R = 999, seed = 1)

# Test the hypothesis of no association between variables
cor.test(guppyData$fatherOrnamentation, guppyData$sonAttractiveness)

################################################
# Spearman's Correlation
################################################

cor.test(guppyData$fatherOrnamentation, guppyData$sonAttractiveness, method = "spearman")



#################################################
# Practice Exercises
#################################################

# 1. If you are having trouble solving homework problems, should you sleep on it and try again in the morning?
#    Huber et al. (2004) asked 10 participants to perform a complex spatial learning task on the computer just before going to sleep.
#    EEG recordings were taken of the electrical activity of brain cells during their sleep. The magnitude of the increase in their 
#    "slow-wave" sleep after learning the complex task, compared to baseline amounts, was recorded for all 
#    10 participants. Also recorded was the increase in performance when the participants were challenged with the same 
#    task upon waking. 
#
# a. Create an appropriate graph of the association between the two variables.
#
# b. Calculate the correlation coefficient (r) and standard error of r between the magnitude of the increase in slow-wave sleep and the magnitude of the improvement in performance upon waking.
#
# c. Test the hypothesis that the two variables are correlated in the population. 
#
# d. Is this an observational or an experimental study? Explain. 

# 2. Agricultural and industrial activity have substantially increased the reactive nitrogen content of the atmosphere. 
#    To investigate the potential consequences on ecosystems, Stevens et al. (2015) recorded atmostpheric nitrogen deposition 
#    (kg/ha/yr) and aboveground net primary production (ANPP, in g/m2/yr), a measure of plant biomass accumulation. 
#
# a. Examine a scatter plot of the data.
#
# b. Discuss whether the data meet assumptions of bivariate normality.
#
# c. Use an appropriate correlation test to evaluate whether the two variables are associated. Make sure to state your hypothesis
#    and interpret your results.
#
# d. calculate a confidence interval for the correlation coefficient

# The next 3 questions can be found at the end of this lab: https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_ANOVA.html
# 3. The European cuckoo does not look after its own eggs, but instead lays them in the nests of birds of other species. 
#    Previous studies showed that cuckoos sometimes have evolved to lay eggs that are colored similarly to the host bird species' eggs. 
#    Is the same true of egg size - do cuckoos lay eggs similar in size to the size of the eggs of their hosts? 
#    The data file "cuckooeggs.csv" contains data on the lengths of cuckoo eggs laid in the nests of a variety of host species. 
#    Here we compare the mean size of cuckoo eggs found in the nests of different host species.
# 
# a. Plot a multiple histogram showing cuckoo egg lengths by host species.
# 
# b. Calculate a table that shows the mean and standard deviation of length of cuckoo eggs for each host species.
#    Include the  number of samples in each group to the table. 
# 
# c. Look at the graph and the table. For these data, would ANOVA be a valid method to test for differences between host species 
#    in the lengths of cuckoo eggs in their nests?
#   
# d. Use ANOVA to test for a difference between host species in the mean size of the cuckoo eggs in their nests. 
#    State your hypotheses, statistical results, statistical conclusion, and biological results.
#   
# e. Assuming that ANOVA rejected the null hypotheses of no mean differences, 
#    use a Tukey-Kramer test to decide which pairs of host species are significantly different from each other in cuckoo egg mean length. 
#    What is your conclusion?

# 4. The pollen of the maize (corn) plant is a source of food to larval mosquitoes of the species Anopheles arabiensis, 
#    the main vector of malaria in Ethiopia. The production of maize has increased substantially in certain areas of Ethiopia recently, 
#    and over the same time period, malaria has entered in to new areas where it was previously rare. 
#    This raises the question, is the increase of maize cultivation partly responsible for the increase in malaria?
#    One line of evidence is to look for an association between maize production and malaria incidence at different 
#    geographically dispersed sites (Kebede et al. 2005). 
#    The data set "malaria vs maize.csv" contains information on several high-altitude sites in Ethiopia, 
#    with information about the level of cultivation of maize (low, medium or high in the variable maize_yield) 
#    and the rate of malaria per 10,000 people (incidence_rate_per_ten_thousand).
# 
# a. Plot a multiple histogram to show the relationship between level of maize production and the incidence of malaria.
# 
# b. ANOVA is a logical choice of method to test differences in the mean rate of malaria between sites differing in level of maize production. 
#    Calculate the standard deviation of the incidence rate for each level of maize yield. 
#    Do these data seem to conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.
# 
# c. Compute the log of the incidence rate and redraw the multiple histograms for different levels of maize yield. 
#    Calculate the standard deviation of the log incidence rate for each level of maize yield. 
#    Does the log-transformed data better meet the assumptions of ANOVA than did the untransformed data?
#   
# d. Test for an association between maize yield and malaria incidence. Make sure to state your hypotheses, statistical result,
#    statistical conclusion, and biological interpretation.
# 
# 
# 5. Animals that are infected with a pathogen often have disturbed circadian rhythms. 
#    (A circadian rhythm is an endogenous daily cycle in a behavior or physiological trait that persists in the absence of time cues.) 
#    Shirasu-Hiza et al. (2007) wanted to know whether it was possible that the circadian timing mechanism itself could have an effect 
#    on disease. To test this idea they sampled from three groups of fruit flies: one "normal", one with a mutation in the timing gene 
#    tim01, and one group that had the tim01 mutant in a heterozygous state. They exposed these flies to a dangerous bacteria, 
#    Streptococcus pneumoniae, and measured how long the flies lived afterwards, in days. The date file "circadian mutant health.csv" 
#    shows some of their data.
# 
# a. Plot a histogram of each of the three groups. Do these data match the assumptions of an ANOVA?
#   
# b. Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups of flies.Make sure to state your hypotheses, statistical result,
#    statistical conclusion, and biological interpretation.
# 




#######################################
# Answers
#######################################

# 1. If you are having trouble solving homework problems, should you sleep on it and try again in the morning?
#    Huber et al. (2004) asked 10 participants to perform a complex spatial learning task on the computer just before going to sleep.
#    EEG recordings were taken of the electrical activity of brain cells during their sleep. The magnitude of the increase in their 
#    "slow-wave" sleep after learning the complex task, compared to baseline amounts, was recorded for all 
#    10 participants. Also recorded was the increase in performance when the participants were challenged with the same 
#    task upon waking. 
#
# a. Create an appropriate graph of the association between the two variables.

sleep<-read.csv("chap16q19SleepAndPerformance.csv")
plot(sleep$sleep,sleep$improvement)

# b. Calculate the correlation coefficient (r) between the magnitude of the increase in slow-wave sleep and the magnitude of the improvement in performance upon waking.

cor(sleep$sleep,sleep$improvement)
# r = 0.86

# c. Test the hypothesis that the two variables are correlated in the population. 

cor.test(sleep$sleep,sleep$improvement)

# H0: There is no correlation between increase in slow-wave sleep and increase in
# performance (r = 0). HA: There is a correlation between increase in slow-wave sleep and
# increase in performance (r != 0). t = 4.84, df = 8, p<0.01. Reject H0 (P = 0.0013). Conclude that is a positive correlation between increase in
# slow-wave sleep and increase in task performance.
#

# 2. Agricultural and industrial activity have substantially increased the reactive nitrogen content of the atmosphere. 
#    To investigate the potential consequences on ecosystems, Stevens et al. (2015) recorded atmostpheric nitrogen deposition 
#    (kg/ha/yr) and aboveground net primary production (ANPP, in g/m2/yr), a measure of plant biomass accumulation. 
#
# a. Examine a scatter plot of the data.

nitrogen<-read.csv("chap16q27nitrogenPlantProduction.csv")

# Scatterplot of the data
ggplot(nitrogen, aes(x=atmosphericNDeposition, 
                      y=logANPP)) +
  geom_point() +
  theme_minimal() +
  xlab("Atmospheric N Deposition") +
  ylab("logANPP")

#
# b. Discuss whether the data meet assumptions of bivariate normality.

# No, there is an outlier that means the data are not suitable for pearson's correlation. Should use Spearman's instead. 
#
# c. Use an appropriate correlation test to evaluate whether the two variables are associated. Make sure to state your hypothesis
#    and interpret your results.

cor.test(nitrogen$atmosphericNDeposition,nitrogen$logANPP,method="spearman")

# H0: The correlation between atmospheric nitrogen deposition
# and log ANPP is zero (??S = 0). HA: The correlation between atmospheric nitrogen
# deposition and log ANPP is not zero (??S ??? 0). rS = 0.537, n = 42, p < 0.001, reject H0 (P = 0.0003). The two variables are indeed correlated.

# d. calculate a confidence interval for the correlation coefficient
ci_cor(nitrogen$atmosphericNDeposition,nitrogen$logANPP, method = "spearman", type = "bootstrap", R = 999, seed = 1)

# 3. Cuckoo Question
# a. Plot a multiple histogram showing cuckoo egg lengths by host species.
cuckoo <- read.csv("cuckooeggs.csv")

# examine the data
head(cuckoo)

# plot a histogram
ggplot(cuckoo, aes(x = egg_length)) +   
  geom_histogram() + 
  facet_wrap(~ host_species, ncol = 1) 

# b. Calculate a table that shows the mean and standard deviation of length of cuckoo eggs for each host species.
#    Include the  number of samples in each group to the table. 

# Descriptive statistics table
cuckooTable <- group_by(cuckoo,host_species)
summarise(cuckooTable, group_mean = mean(egg_length, na.rm=TRUE), group_sd = sd(egg_length,na.rm=T),n=n())
# 
# c. Look at the graph and the table. For these data, would ANOVA be a valid method to test for differences between host species 
#    in the lengths of cuckoo eggs in their nests?
#   
# d. Use ANOVA to test for a difference between host species in the mean size of the cuckoo eggs in their nests. 
#    State your hypotheses, statistical results, statistical conclusion, and biological results.

# use an anova to test for differences in egg size of cuckoo eggs in nests of different host species
cuckooANOVA <- lm(egg_length ~ host_species, data = cuckoo)
anova(cuckooANOVA)
#   
# e. Assuming that ANOVA rejected the null hypotheses of no mean differences, 
#    use a Tukey-Kramer test to decide which pairs of host species are significantly different from each other in cuckoo egg mean length. 
#    What is your conclusion?

# TukeyHSD
TukeyHSD(aov(cuckooANOVA))

# Q 4 Maize
# a. Plot a multiple histogram to show the relationship between level of maize production and the incidence of malaria.

maize <- read.csv("malaria vs maize.csv")
head(maize)
nrow(maize)
maize$maize_yield<-factor(maize$maize_yield,levels = c("Low","Medium","High"))

# plot a histogram
ggplot(maize, aes(x = incidence_rate_per_ten_thousand)) +   
  geom_histogram() + 
  facet_wrap(~ maize_yield, ncol = 1) 

# b. ANOVA is a logical choice of method to test differences in the mean rate of malaria between sites differing in level of maize production. 
#    Calculate the standard deviation of the incidence rate for each level of maize yield. 
#    Do these data seem to conform to the assumptions of ANOVA? Describe any violations of assumptions you identify.

# summarize the data
maizeTable <- group_by(maize,maize_yield)
summarise(maizeTable, group_mean = mean(incidence_rate_per_ten_thousand, na.rm=TRUE), group_sd = sd(incidence_rate_per_ten_thousand,na.rm=T),n=n())
# 
# c. Compute the log of the incidence rate and redraw the multiple histograms for different levels of maize yield. 
#    Calculate the standard deviation of the log incidence rate for each level of maize yield. 
#    Does the log-transformed data better meet the assumptions of ANOVA than did the untransformed data?

# try a log transformation to deal with unequal variances
maize$logIncidence<-log(maize$incidence_rate_per_ten_thousand)
ggplot(maize, aes(x = logIncidence)) +   
  geom_histogram() + 
  facet_wrap(~ maize_yield, ncol = 1) 
# summarize the data
maizeTable <- group_by(maize,maize_yield)
summarise(maizeTable, group_mean = mean(logIncidence, na.rm=TRUE), group_sd = sd(logIncidence,na.rm=T),n=n())

#   
# d. Test for an association between maize yield and malaria incidence. Make sure to state your hypotheses, statistical result,
#    statistical conclusion, and biological interpretation.

# anova
maizeANOVA <- lm(logIncidence ~ maize_yield, data = maize)
anova(maizeANOVA)

# TukeyHSD
TukeyHSD(aov(maizeANOVA))


# 5. Fly circadian rhythm
# a. Plot a histogram of each of the three groups. Do these data match the assumptions of an ANOVA?

fly <- read.csv("C:/Users/Heather/UNBC/NRES 776 2020/Labs/ABDLabs/ABDLabs/DataForLabs/circadian mutant health.csv")
ggplot(fly, aes(x = days_to_death)) +   
  geom_histogram() + 
  facet_wrap(~ genotype, ncol = 1) 
flyTable <- group_by(fly,genotype)
summarise(flyTable, group_mean = mean(days_to_death, na.rm=TRUE), group_sd = sd(days_to_death,na.rm=T),n=n())

#   
# b. Use a Kruskal-Wallis test to ask whether lifespan differs between the three groups of flies.Make sure to state your hypotheses, statistical result,
#    statistical conclusion, and biological interpretation.
kruskal.test(days_to_death ~ genotype, data = fly)

# We could do a post-hoc test here too
# use the Dunn test because sample sizes are unequal
library(PMCMRplus)
posthoc.kruskal.dunn.test(days_to_death ~ genotype, data = fly)

