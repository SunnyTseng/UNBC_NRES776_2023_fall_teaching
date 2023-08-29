
####################################
# NRES 776
# Lab 6: Analyzing multiple factors
####################################

#############################
# Topics
#############################
# Linear Regression
# Blocked design
# Factorial ANOVA
# ANCOVA
#############################

##################################
# Load required packages
##################################
library(dplyr)
library(ggplot2)
#install.packages("car", dependencies = TRUE)    # only if not yet installed
#install.packages("tidyr", dependencies = TRUE)  # only if not yet installed
#install.packages("nlme", dependencies = TRUE)  # only if not yet installed
library(car)
library(tidyr)
library(nlme)
#install.packages("dotwhisker")
#install.packages("broom")
library(dotwhisker)
library(broom)
#install.packages("visreg")
library(visreg)
###############################################


###############################################
# Materials used in this lab
###############################################
# Source code from Chapter 13 of Whitlock and Schluter
# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_13.html
# Labs using R: 
# https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Correlation_Regression.html
# Source code from Chapter 18 of Whitlock and Schluter
# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_18.html
# Assignment questions from Chapter 18 of Whitlock and Schluter (#12--Neanderthals, #17--opsins in killifish)
###############################################


#############################
# Relevant R Datacamp Modules
#############################
# Introduction to Regression in R
# Correlation and Regression in R
#############################


####################################
# Set working directory
####################################
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 6 Multiple regression/")
####################################

###############################################
# Linear Regression
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

# Fit a model
lm <- lm(sonAttractiveness ~ fatherOrnamentation, data = guppyData)

# summary shows information from the regression
summary(lm)
# You can also get an ANOVA table
anova(lm)

# The lm object stores other useful information from the regression
# In the command window, type lm$
# Scroll through the options to see what information is available
# plot a histogram of the residuals to see if they look normal
hist(lm$residuals)
# Or
hist(residuals(lm))

# Another useful plot to look at is the actual versus predicted y values
# This plot shows how good your model is at predicting values of y
plot(guppyData$sonAttractiveness,lm$fitted.values)

# lm also has several built-in plotting functions to look at the assumptions
# hit return in the console to go through the different figures
plot(lm)
# Plot 1: Residuals vs. fitted
# Look for non-linear patterns in the residuals; if the line is fairly straight and residuals are evenly spaced that is good
# Plot 2: Normal Q-Q of residuals
# Residuals should follow the theoretical quantile line
# Plot 3: Scale-Location
# Check for assumption of equal variance using this plot
# If variances are equal, the line should be horizontal and straight with points spread randomly along it
# Plot 4: Residual vs Leverage
# Used for finding influential points/outliers

# Now let's plot the regression line (several ways)

# Base R
plot(sonAttractiveness ~ fatherOrnamentation, data = guppyData)
# add the regression line
abline(lm)
# add the equation for the line
text(0.2,1.2,paste0("y=",round(lm$coefficient[[1]],3),"+",round(lm$coefficient[[2]],3),"x"))

# Make a plot with confidence intervals
# Use visreg package
visreg(lm, points.par = list(pch = 16, cex = 1.2, col = "firebrick"))
# You can do the same thing using ggplot
ggplot(guppyData, aes(x=fatherOrnamentation, 
                      y=sonAttractiveness)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's ornamentation") +
  ylab("Son's attractiveness") +    
  geom_smooth(method = "lm", se = TRUE)

# Predict values of Y from original values of x
# generate confidence intervals for the line
guppyConfInt<-predict(lm, interval = "confidence")
# generate prediction intervals for the line
guppyPredict<-predict(lm,interval = "prediction")
guppyNew<-cbind(guppyData,guppyPredict)

# Plot confidence interval and prediction interval
ggplot(guppyNew, aes(y = sonAttractiveness, x = fatherOrnamentation)) +
  geom_point(size = 3, col = "firebrick") +
  geom_smooth(method = "lm", se = TRUE) +
  geom_line(aes(y = lwr), color = "black", linetype = "dashed") +
  geom_line(aes(y = upr), color = "black", linetype = "dashed") +
  labs(x = "Father's Ornamentation", y = "Son's Attractiveness") + 
  theme_classic()

# Predict values of y from new values of x
fatherOrnamentation<-seq(0.03,1.12,by=0.01)
sonAttractiveness<-mat.or.vec(length(fatherOrnamentation),1)
newData<-as.data.frame(cbind(fatherOrnamentation,sonAttractiveness))
newData$sonAttractiveness<-predict(lm,newdata = newData)
plot(newData$fatherOrnamentation,newData$sonAttractiveness)
# now you can add a regression line that doesn't go past the data
plot(guppyData$fatherOrnamentation,guppyData$sonAttractiveness)
lines(newData$fatherOrnamentation,newData$sonAttractiveness)


########################################
# ANOVA with Blocks
########################################
# These are data from the Zooplankton example we discussed in class
# Researchers looked at the effects of fish predator abundance (high, medium, or low) on zooplankton diversity
# Treatments were replicated at five different locations (blocks) in the lake

zooplankton <- read.csv("chap18e2ZooplanktonDepredation.csv", stringsAsFactors = FALSE)
zooplankton

# Set order of groups (low, medium, high)
zooplankton$treatment <- factor(zooplankton$treatment, levels = c("control","low","high"))

# Display the data in a table using tapply (base R)
tapply(zooplankton$diversity, list(Treatment = zooplankton$treatment, 
                                   Location = zooplankton$block), unique)

# This is another way to display data in a table using spread from the tidyr package
spread(data = zooplankton, key = block, value = diversity)

# Plot the data with lines connecting treatments in each block
ggplot(zooplankton, aes(x = treatment, y = diversity)) +  
  geom_line(aes(group = block)) +
  geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
  labs(x = "Treatment", y = "Zooplankton diversity (D)") + 
  theme_classic()

# Fit a null model for the block design
# lme stands for linear mixed effects models and is in the nlme package
# Block is a random effect
zoopNullModel <- lme(diversity ~ 1, random = ~ 1| block, data = zooplankton)

# Fit the full model
zoopRBModel <- lme(diversity ~ treatment, random = ~ 1| block, data = zooplankton)

# visualize the model fit
# only 4 lines visible because one is plotted on top of the other
ggplot(zooplankton, aes(x = treatment, y = diversity)) +  
  geom_line(aes(y = predict(zoopRBModel), group = block)) +
  geom_point(size = 5, col = "firebrick", alpha = 0.5) + 
  labs(x = "Treatment", y = "Zooplankton diversity (D)") + 
  theme_classic()

# F-test to determine whether the model with treatment is an improvement over the null model
anova(zoopRBModel)

# Summary of model output
summary(zoopRBModel)

########################################
# Factorial ANOVA
########################################

# Effects of herbivore exclusion on abundance of red algae in the intertidal zone
algae <- read.csv("chap18e3IntertidalAlgae.csv", stringsAsFactors = FALSE)
head(algae)

# fit the model with no interaction term
algaeNoInteractModel <- lm(sqrtArea ~ height + herbivores, data = algae)

# this command stores the predicted values from the model so you can plot the fit
algae$fit0 <- predict(algaeNoInteractModel)

# plot the model results
# stat_summary command is the part the plots the fitted values
ggplot(data = algae, aes(x = herbivores, y = sqrtArea,
                         fill = height, color = height, shape = height)) + 
  geom_jitter(size = 3, position = position_dodge2(width = 0.7)) +
  scale_colour_manual(values = c("firebrick", "black")) +
  scale_shape_manual(values = c(16, 15)) +
  stat_summary(aes(group = height, y = fit0), fun = mean, geom = "line",
               linetype="dashed", size = 1) +
  labs(x = "Herbivory treatment", y = "Square-root surface area (cm)") +
  theme_classic()

# Fit the full model including the interaction term
# Note: you could write the model as height * herbivores, but it's clearer to include the main effects
algaeFullModel <- lm(sqrtArea ~ height + herbivores + height * herbivores, data = algae)
algae$fit1 <- predict(algaeFullModel)

# Visualize the model fit
ggplot(data = algae, aes(x = herbivores, y = sqrtArea,
                         fill = height, color = height, shape = height)) + 
  geom_jitter(size = 3, position = position_dodge2(width = 0.7)) +
  scale_colour_manual(values = c("firebrick", "black")) +
  scale_shape_manual(values = c(16, 15)) +
  stat_summary(aes(group = height, y = fit1), fun = mean, geom = "line",
               linetype="dashed", size = 1) +
  labs(x = "Herbivory treatment", y = "Square-root surface area (cm)") +
  theme_classic()

# Test whether the interaction model is an improvement over the model without the interaction
# anova works as long as one model is a reduced version of the other
anova(algaeNoInteractModel, algaeFullModel)

# Check assumptions of the model
# Residual plot
plot(residuals(algaeFullModel) ~ fitted(algaeFullModel) )
abline(0,0)

# Q-Q plot
qqnorm(residuals(algaeFullModel), pch = 16, col = "firebrick", 
       las = 1, ylab = "Residuals", xlab = "Normal quantile", main = "")


########################################
# ANCOVA
########################################
# Energy expenditure in mole rate in different castes
# Want to adjust for differences in body mass

moleRat <- read.csv("chap18e4MoleRatLayabouts.csv", stringsAsFactors = FALSE)
head(moleRat)

# Scatter plot of data
ggplot(moleRat, aes(lnMass, lnEnergy, colour = caste, shape = caste)) + 
  geom_point(size = 3) + 
  scale_colour_manual(values = c("firebrick", "firebrick")) +
  scale_shape_manual(values = c(16, 1)) +
  labs(x = "ln(body mass)", y = "ln(daily energy expenditure)") + 
  theme_classic()

# Fit model without interaction term first
moleRatNoInteractModel <- lm(lnEnergy ~ lnMass + caste, data = moleRat)

# Store predicted values
moleRat$fit0 <- predict(moleRatNoInteractModel)

# re-plot the data and include the model fit (geom_line)
ggplot(moleRat, aes(lnMass, lnEnergy, colour = caste, 
                    shape = caste, linetype=caste)) + 
  geom_line(aes(y = fit0), size = 1, color = "black") +
  geom_point(size = 3) + 
  scale_colour_manual(values = c("firebrick", "firebrick")) +
  scale_shape_manual(values = c(16, 1)) +
  labs(x = "ln(body mass)", y = "ln(daily energy expenditure)") + 
  theme_classic()

# Fit the model with the interaction term
moleRatFullModel <- lm(lnEnergy ~ lnMass * caste, data = moleRat)
moleRatFullModel2 <- lm(lnEnergy ~ lnMass + caste + lnMass * caste, data = moleRat)

# Plot the full model
# geom_smooth(method="lm"....) plots the full model
ggplot(moleRat, aes(lnMass, lnEnergy, colour = caste, 
                    shape = caste, linetype=caste)) + 
  geom_smooth(method = "lm", size = 1, se = FALSE, col = "black") +
  geom_point(size = 3) + 
  scale_colour_manual(values = c("firebrick", "firebrick")) +
  scale_shape_manual(values = c(16, 1)) +
  labs(x = "ln(body mass)", y = "ln(daily energy expenditure)") + 
  theme_classic()

# Test the two models to compare whether the model with the interaction term provides a better fit
anova(moleRatNoInteractModel, moleRatFullModel)

# Test whether the intercepts are different for the two social classes in the reduced model (no interaction term)
moleRatNoInteractModel <- lm(lnEnergy ~ lnMass + caste, data = moleRat)
anova(moleRatNoInteractModel)

# Check assumptions
# residual plot
plot( residuals(moleRatNoInteractModel) ~ fitted(moleRatNoInteractModel) )
abline(0,0)

# q-q plot
qqnorm(residuals(moleRatNoInteractModel), pch = 16, col = "firebrick", 
       las = 1, ylab = "Residuals", xlab = "Normal quantile", main = "")



########################################
# Questions
########################################

# See https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Correlation_Regression.html

# 1. The ends of chromosomes are called telomeres. These telomeres are shortened a bit during each cell cycle as DNA is replicated. 
# One of their purposes is to protect more valuable DNA in the chromosome from degradation during replication. 
# As people get older and their cells have replicated more often, their telomeres shorten. 
# There is evidence that these shortened telomeres may play a role in aging. 
# Telomeres can be lengthened in germ cells and stem cells by an enzyme called telomerase, 
# but this enzyme is not active in most healthy somatic cells. (Cancer cells, on the other hand, usually express telomerase.)
# 
# Given that the length of telomeres is biologically important, it becomes interesting to know whether telomere length varies 
# between individuals and whether this variation is inherited. A set of data was collected by Nordfjäll et al. (2005) 
# on the telomere length of fathers and their children; these data are in the file "telomere inheritance.csv".
# 
# a. Create a scatter plot showing the relationship between father and offspring telomere length.
# 
# b. Do the data require any transformation before analysis using linear regression?
#   
# c. Estimate an equation that predicts the offspring telomere length from its father's. 
#    Is there evidence that the father's telomere length predicts his offspring's value? 
#    How much variability in the offsprings' telomere length is explained by the regression?
#    State the null and alternative hypotheses for the test.

# d. Check the assumptions of your model using appropriate plots.

# e. When you are happy with the model, plot the regression line on top of a scatter plot. Include confidence intervals for the slope. 

# 2. Opfer and Segler (2007) asked second- and fourth-grade school children to mark on a number line where a given number would fall. 
# Each child was given a drawing of a number line with two ends marked at 0 and 1000, and was then asked to make an X on that line where 
# a number, for example 150, should be placed. They asked each child to place several different numbers on the number lines, 
# each on a fresh new piece of paper.

# The researchers then measured the placement of each mark on a linear scale. 
# The results, averaged over all 93 kids for each group, are given in the file "numberline.csv".

# a. Plot the fourth graders' guesses against the true value. Is this relationship linear? 
# If not, find a transformation of X or Y that converts the relationship into an approximately linear one.

# b. Plot the second-graders' guesses against the true value. Is this relationship linear? If not, 
# find a transformation of X or Y that converts the relationship into an approximately linear one. 
# Fit a linear regression to both the transformed and untransformed data. 
# Examine the residual plots for both the transformed and untransformed data.

# c. Assume that the difference between the shapes of these curves is real. 
# What would you conclude about the difference in the way 2nd graders and 4th graders perceive numbers?

# 3. Larger animals tend to have larger brains. But is the increase in brain size proportional to the increase in body size? 
# A set of data on body and brain size of 62 mammal species was collated by Allison and Cicchetti (1976), 
# and these data are in the data set "mammals.csv". The file contains columns giving the species name, 
# the average body mass (in kg) and average brain size (in g) for each species. 
# These are the same data used in the second half of the app about residuals that you used in the activities earlier in this lab.

# a. Plot brain size against body size. Is the relationship linear?

# b. Find a transformation (for either or both variables) that makes the relationship between these two variables linear.

# c. Is there statistical evidence that brain size is correlated with body size? Assume that the species data are independent.

# d. What line best predicts (transformed) brain size from (transformed) body size?

# e. Based on your answer in (d), what is the predicted change in log-brain size accompanying an increase of 3 units of log- body size?

# f. Make a residual plot using the regression fitted to the transformed variables. 
# Do the data look like they match the assumptions of linear regression?


# 4. Does light environment have an influence on the development of colour vision? 
# The accompanying data, from Fuller et al (2010), are measurements of the relative abilities of bluefin killifish
# from two wild populations to detect short wavelengths of light (blue light in our own visible colour spectrum). 
# One population was from a swamp, whose tea-stained water fileters out blue wavelengths, whereas the other
# population was from a clear-water spring. Fish were crossed and raised in the lab under two light conditions
# simulating those in the wild: clear and tea-stained. Sensitivity to blue light was measured as the relative expression of the SWS1
# opsin gene in the eyes of the fish (as a proportion of total expression of all opsins). Opsin proteins in eyes
# detect ligth of specific wavelengths; SWS1 is so named because it is shortwave sensitive. The data are from
# a single individual from each of 33 families. Because the fish were raised in a common lab environment,
# population differences are likely to be genetically based, whereas differences between 
# fish under different water clarity conditions are environmentally induced.

# a. Read and inspect the data.

# b. How many factors are included in this experiment? Identify them.

# c. Generate an interaction plot of the data. Looking at the graph, which effects do you think will be significant in the model?
#    Do the genetic and environmentally-induced effects on SWS1 opsin expression appear to be in the same direction?

# d. Provide a word statement of a full linear model to fit the data.

# e. Fit the model and examine the output.

# f. What null hypotheses are being tested?

# g. Which effects were statistically significant?

# h. Check the assumptions of the model using appropriate graphs. 

# 5. Were Neanderthals smaller-brained than modern humans? Estimates of cranial capacity from fossils indicate that Neanderthals
# had large brains, but also that they had a large body size. The accompanying data include log-transformed brain and body sizes of 
# Neanderthal specimens and early modern humans. The goal of the analysis was to determine whether humans and Neanderthals have different
# brain and body sizes once their differences in body size are taken into account. 

# a. Read and inspect the data.

# b. Use an appropriate modeling framework to determine whether the brain sizes of Neanderthals and early modern humans differ
#    after adjusting for body size. 

# c. Graph the results of your model.

# d. Check the assumptions. 


########################################
# Answers
########################################

# 1.  
# a. Create a scatter plot showing the relationship between father and offspring telomere length.
telomeres <- read.csv("telomere inheritance.csv")
plot(telomeres$father_telomere_length,telomeres$offspring_telomere_length)

# 
# b. Do the data require any transformation before analysis using linear regression?
hist(telomeres$father_telomere_length)
hist(telomeres$offspring_telomere_length)
#   
# c. Estimate an equation that predicts the offspring telomere length from its father's. 

t1<-lm(offspring_telomere_length~father_telomere_length,data=telomeres)

#    Is there evidence that the father's telomere length predicts his offspring's value? 
# Yes. t = 4.4, p<0.001

#    How much variability in the offsprings' telomere length is explained by the regression?
# r-squared = 0.3353

#    State the null and alternative hypotheses for the test.
# H0: Slope of the relationship between father telomere length and son telomere length = 0 (Beta = 0)
# H1: Slope of the relationship between father telomere length and son telomere length is not 0 (Beta != 0)

# d. Check the assumptions of your model using appropriate plots.
plot(t1)

# e. When you are happy with the model, plot the regression line on top of a scatter plot. Include confidence intervals for the slope. 
visreg(t1, points.par = list(pch = 16, cex = 1.2, col = "firebrick"))
# You can do the same thing using ggplot
ggplot(telomeres, aes(x=father_telomere_length, 
                      y=offspring_telomere_length)) +
  geom_point() +
  theme_minimal() +
  xlab("Father's Telomere Length") +
  ylab("Offspring's Telomere Length") +    
  geom_smooth(method = "lm", se = TRUE)


# 2. 
numberLine <- read.csv("numberline.csv")

# a. Plot the fourth graders' guesses against the true value. Is this relationship linear? 
# If not, find a transformation of X or Y that converts the relationship into an approximately linear one.

plot(numberLine$true_value,numberLine$fourth_graders_guess)
# yes, fairly linear

# b. Plot the second-graders' guesses against the true value. Is this relationship linear? If not, 
# find a transformation of X or Y that converts the relationship into an approximately linear one. 
# Fit a linear regression to both the transformed and untransformed data. 
# Examine the residual plots for both the transformed and untransformed data.

plot(numberLine$true_value,numberLine$second_graders_guess)
hist(numberLine$second_graders_guess)
hist(log(numberLine$true_value))
plot(log(numberLine$true_value),numberLine$second_graders_guess)

nL1<-lm(numberLine$true_value~numberLine$second_graders_guess) 
plot(nL1)
hist(nL1$residuals)

nL2<-lm(log(numberLine$true_value)~numberLine$second_graders_guess)
plot(nL2)
hist(nL2$residuals)

# c. Assume that the difference between the shapes of these curves is real. 
# What would you conclude about the difference in the way 2nd graders and 4th graders perceive numbers?

plot(numberLine$true_value,numberLine$second_graders_guess,type="l")
lines(numberLine$true_value,numberLine$fourth_graders_guess)
# 2nd graders tend to overestimate where numbers should be placed along a number line for numbers between 0 and 600

# 3. Larger animals tend to have larger brains. But is the increase in brain size proportional to the increase in body size? 
mammals<-read.csv("mammals.csv")

# a. Plot brain size against body size. Is the relationship linear?
plot(mammals$brain_mass_g,mammals$body_mass_kg)

# b. Find a transformation (for either or both variables) that makes the relationship between these two variables linear.
plot(log(mammals$brain_mass_g),log(mammals$body_mass_kg))

# c. Is there statistical evidence that brain size is correlated with body size? Assume that the species data are independent.
cor.test(log(mammals$brain_mass_g),log(mammals$body_mass_kg))
# yes. rho = 0.959, t = 26.2, df = 60, p<0.001

# d. What line best predicts (transformed) brain size from (transformed) body size?
mammals<-lm(log(mammals$brain_mass_g)~log(mammals$body_mass_kg))
summary(mammals)
# logbrain mass = 2.13 + 0.755*logBody mass

# e. Based on your answer in (d), what is the predicted change in log-brain size accompanying an increase of 3 units of log- body size?
# The regression coefficient (slope) for log(brain mass) gives the change in y for a single unit change in x. 
# The change in y for three units change in x would be 3*the slope (3*0.7545). 
# You can test it by plugging values into the equation for the regression line: 
y1<-2.1272+0.7545*3
y<-2.1272+0.7545*0
y1-y
# [1] 2.2635
.7545*3
# [1] 2.2635


# f. Make a residual plot using the regression fitted to the transformed variables. 
plot(mammals)
hist(mammals$residuals)

# Do the data look like they match the assumptions of linear regression?
# yes, looks fine.

# 4. Opsins in killifish
# a. Read and inspect the data.
opsins<-read.csv("chap18q17OpsinExpression.csv")
head(opsins)
nrow(opsins)

# b. How many factors are included in this experiment? Identify them.

# Two factors: Population and Water Clarity Treatment

# c. Generate an interaction plot of the data. Looking at the graph, which effects do you think will be significant in the model?
#    Do the genetic and environmentally-induced effects on SWS1 opsin expression appear to be in the same direction?

ggplot(data = opsins, aes(x = waterClarity, y = relativeExpressionOfSWS1,
                          fill = population, color = population, shape = population)) + 
  geom_jitter(size = 3, position = position_dodge2(width = 0.7)) +
  scale_colour_manual(values = c("firebrick", "black")) +
  scale_shape_manual(values = c(16, 15)) +
  stat_summary(aes(group = population, y = fit1), fun.y = mean, geom = "line",
               linetype="dashed", size = 1) +
  labs(x = "Water clairty Treatment", y = "Relative expression of SWS1") +
  theme_classic()


# d. Provide a word statement of a full linear model to fit the data.

# SWS1Expression = Constant + Population + Treatment + Population * Treatment

# e. Fit the model and examine the output.


opsinsFullModel <- lm(relativeExpressionOfSWS1 ~ population * waterClarity, data = opsins, 
                             contrasts = list(population = contr.sum, waterClarity = contr.sum))
anova(opsinsFullModel) 

# f. What null hypotheses are being tested?

# H0: There is no difference between population means. H0: There is no effect of
# treatment. H0: There is no interaction between population and treatment

# g. Which effects were statistically significant?

#Yes, population and treatment main effects are statistically significant, whereas the
#interaction is not statistically significant.

# h. Check the assumptions of the model using appropriate graphs. 

plot(residuals(opsinsFullModel) ~ fitted(opsinsFullModel) )
abline(0,0)

# Q-Q plot
qqnorm(residuals(opsinsFullModel), pch = 16, col = "firebrick", 
       las = 1, ylab = "Residuals", xlab = "Normal quantile", main = "")


# 5.Neanderthals

# a. Read and inspect the data.
neanderthals<-read.csv("chap18q12NeanderthalBrainSize.csv")
head(neanderthals)
nrow(neanderthals)

# b. Use an appropriate modeling framework to determine whether the brain sizes of Neanderthals and early modern humans differ
#    after adjusting for body size. 

# Use ANCOVA
# Fit model with interaction term
neanderthalsFullModel2 <- lm(lnBrain ~ lnMass + species + lnMass*species, data = neanderthals)
# Test the two models to compare whether the model with the interaction term provides a better fit
summary(neanderthalsFullModel2)
# P-value for the interaction term of lnMass*species is not significant (p = 0.303)
# We can drop the interaction term and proceed to test for differences in brain size between species

# Fit model without interaction term first
neanderthalsNoInteractModel <- lm(lnBrain ~ lnMass + species, data = neanderthals)
summary(neanderthalsNoInteractModel)

# We can also compare models using an ANOVA
# The test is not significant, suggesting that the model with an interaction term is not significantly better than the model without the interaction term
anova(neanderthalsNoInteractModel, neanderthalsFullModel2)


# c. Graph the results of your model.

# Store predicted values
neanderthals$fit0 <- predict(neanderthalsNoInteractModel)

# re-plot the data and include the model fit (geom_line)
ggplot(neanderthals, aes(lnMass, lnBrain, colour = species, 
                    shape = species, linetype=species)) + 
  geom_line(aes(y = fit0), size = 1, color = "black") +
  geom_point(size = 3) + 
  scale_colour_manual(values = c("firebrick", "firebrick")) +
  scale_shape_manual(values = c(16, 1)) +
  labs(x = "ln(body mass)", y = "ln(brain size)") + 
  theme_classic()


# d. Check the assumptions. 
# Check assumptions
# residual plot
plot(residuals(neanderthalsNoInteractModel) ~ fitted(neanderthalsNoInteractModel))
abline(0,0)

# q-q plot
qqnorm(residuals(neanderthalsNoInteractModel), pch = 16, col = "firebrick", 
       las = 1, ylab = "Residuals", xlab = "Normal quantile", main = "")
