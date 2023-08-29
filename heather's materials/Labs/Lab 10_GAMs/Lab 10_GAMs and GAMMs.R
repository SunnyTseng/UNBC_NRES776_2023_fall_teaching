####################################
# NRES 776
# Lab 10: Generalized additive models
####################################

#############################
# Topics
#############################
# Fit a quadratic curve
# Fit a cubic smoother
# Build general and generalized additive models with fixed and random effects
#########################

###############################################
# Materials used in this lab
###############################################
# Chapter 5: barn owl data
# Zuur et al. 2009. Generalized Mixed Effects Models in R
###############################################

#############################
# Relevant R Datacamp Modules
#############################
# Nonlinear Modeling in R with GAMs
#############################

#############################################################
# Set the working directory
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 10_GAMs")
##############################################################

###############################################
# Load required packages
library(MuMIn)
library(nlme)
library(lattice)
library(mgcv)
library(MASS)
library(lme4)
#install.packages("ggformula")
library(ggformula)
library(ggplot2)
###############################################

###############################################################
# Dealing with non-linearity using a quadratic term
# Example 17.8-2 from Whitlock and Schluter
# Number of plant species present in ponds (response) and pond productivity (x-axis)
pondProductivity <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17f8_2PondPlantsAndProductivity.csv"), stringsAsFactors = FALSE)
head(pondProductivity)

# plot the data
plot(pondProductivity$productivity,pondProductivity$species)

# what happens if we fit a linear model?
productivityLine <- lm(species ~ productivity, data = pondProductivity)
abline(productivityLine)
# check model assumptions
plot(productivityLine)

# fit a quadratic function
# the I makes sure the ^2 on productivity operates as an arithmetic operator
productivityCurve <- lm(species ~ productivity + I(productivity^2), data = pondProductivity)
# Now there are coefficient estimates for both the term productivity and productivity squared
summary(productivityCurve)
# check assumptions
plot(productivityCurve)

# generate a plot with a quadratic curve
ggplot(pondProductivity, aes(productivity, species)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2), 
              color = "black", se = FALSE) +
  geom_point(size = 3, col = "firebrick") + 
  labs(x ="Productivity (g/15 days)", y = "Number of species") + 
  theme_classic()

####################################################################################
# Fit a smoothing curve (spline)
# Example 19.9-1 from Whitlock and Schluter
# Data are measurements of body length (cm) taken on female fur seals of different ages (in days)

shrink <- read.csv(url("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter17/chap17e8ShrinkingSeals.csv"), stringsAsFactors = FALSE)
head(shrink)
# How many data points are in the data set?
# What are the minmum and maximum values of female fur seal size and age?

# Make a scatter plot with some jitter to reduce overlap of points
ggplot(shrink, aes(ageInDays, length)) + 
  geom_jitter(size = 0.3, col = "firebrick", height = 0.5) + 
  labs(x = "Female age (days)", y = "Body length (cm)") + 
  theme_classic()
# What kind of line would be appropriate to fit to these data?

# Add a spline
# df sets lambda, the smoothing parameter
ggplot(shrink, aes(ageInDays, length)) + 
  geom_jitter(size = 0.3, col = "firebrick", height = 0.5) + 
  geom_spline(col = "black", df = 40) +
  labs(x = "Female age (days)", y = "Body length (cm)") + 
  theme_classic()

# what happens when you change the df in the geom_spline?
# try df = 2992 and df = 10

#################
# Barn owl data
#################

# Is sibling negotiation related to food treatment, arrival time, and/or sex of the parent?
Owls<-read.delim("Owls.txt")
Owls$LogNeg<-log10(Owls$NegPerChick+1)

# Fit a model with a random intercept term for nest
# Use the lme function in nlme package
bestMod <- lme(LogNeg~ArrivalTime+FoodTreatment,random=~1|Nest,method="REML",data=Owls) # Fit best model
summary(bestMod)

# Model validation part II
# Plot residuals against arrival time for each treatment level and sex of the parent
E2<-resid(bestMod,type="normalized")
xyplot(E2~ArrivalTime|SexParent*FoodTreatment,data=Owls,
       ylab="Residuals",xlab="Arrival time (hours)",
       panel=function(x,y){
         panel.grid(h=-1, v= 2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5,col=1,lwd=2)})
# Plot shows possible patterns in the residuals

# Fit a generalized additive model
bestMod.gamm <- gamm(LogNeg ~ FoodTreatment + s(ArrivalTime),random=list(Nest=~1),data=Owls)

# Look at model output
# summary command doesn't give much information because gamm objects have a gam component and a lme component
# Detailed output on smoothers and parametric terms in the model
summary(bestMod.gamm$gam)
# More compact presentation of results
anova(bestMod.gamm$gam)
# Plot the smoothers
plot(bestMod.gamm$gam)
# Plot the normalised residuals versus fitted values to assess homogeneity
plot(bestMod.gamm$lme)

# Is the GAMM better than the GLMM model?
# re-fit the GLMM top model with the gamm function
best.Modglmm <- gamm(LogNeg ~ FoodTreatment + ArrivalTime,random=list(Nest=~1),data=Owls)
# compare AIC
AIC(bestMod.gamm)
AIC(best.Modglmm)


##############
# Questions
##############

# 1. How does the isolation of an island and its area influence the probability that the island is occupied by a study species?
# The data set is "isolation.txt" and "incidence" is the occurrence of the species of interest.
# Read and inspect the data
# What family would be appropriate for modeling these data?
# Fit a gam and plot the results as follows:
model1<-gam(incidence~s(area)+s(isolation),family=binomial, data=isolation)
par(mfrow=c(1,2))
plot.gam(model1,residuals=T,pch=16)
# Compare this first model with models that include linear relationships with area and isolation, and with each variable separately
# which model(s) are most supported?

# 2. In previous examples, we looked at data collected on amphibian roadkills along a road in Portugal. 
# We looked at several different models and ended up using a negative binomial model to look at the relationship between the number of amphibians killed along different 
# sections of roads and several different habitat variables. We could also apply a GAM to the same data.
# Read in and inspect the data
# Create a gam with family=nb(link="log") using the variables OPEN.L and D.PARK
# Build additional models with only OPEN.L or D.Park, and with and without smoothers
# Which model has the lowest AIC?
# Inspect the output of the top model
# Plot the output of the top model

#####################
# Answers
#####################

# Question 1
isolation<-read.delim("isolation.txt")
model1<-gam(incidence~s(area)+s(isolation),family=binomial, data=isolation)
model2<-gam(incidence~area+s(isolation),family=binomial, data=isolation)
model3<-gam(incidence~s(area)+isolation,family=binomial, data=isolation)
model4<-gam(incidence~area+isolation,family=binomial, data=isolation)
model5<-gam(incidence~s(area),family=binomial, data=isolation)
model6<-gam(incidence~area,family=binomial, data=isolation)
model7<-gam(incidence~s(isolation),family=binomial, data=isolation)
model8<-gam(incidence~isolation,family=binomial, data=isolation)
AIC(model1)
AIC(model2)
AIC(model3)
AIC(model4)
AIC(model5)
AIC(model6)
AIC(model7)
AIC(model8)
# Models 1, 2, 3, 7 have similar AIC scores (<2)
summary(model7)

# Question 2
RK<-read.delim("RoadKills.txt")
m1 <- gam(TOT.N ~ s(OPEN.L) + s(D.PARK), family=nb(link="log"), data = RK)
m2 <- gam(TOT.N ~ s(OPEN.L), family=nb(link="log"), data = RK)
m3 <- gam(TOT.N ~ s(D.PARK), family=nb(link="log"), data = RK)
m4 <- gam(TOT.N ~ s(OPEN.L) + (D.PARK), family=nb(link="log"), data = RK)
m5 <- gam(TOT.N ~ (OPEN.L) + s(D.PARK), family=nb(link="log"), data = RK)
m6 <- gam(TOT.N ~ (OPEN.L) + (D.PARK), family=nb(link="log"), data = RK)
m7 <- gam(TOT.N ~ (OPEN.L), family=nb(link="log"), data = RK)
m8 <- gam(TOT.N ~ (D.PARK), family=nb(link="log"), data = RK)
AICs<-c(AIC(m1),
        AIC(m2),
        AIC(m3),
        AIC(m4),
        AIC(m5),
        AIC(m6),
        AIC(m7),
        AIC(m8)
)
min(AICs)
# Model 1 has the lowest AIC
summary(m1)
plot.gam(m1,residuals=T,pch=16)