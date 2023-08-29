
####################################
# NRES 776
# Lab 9: Generalized Linear mixed effects models
####################################

#############################
# Topics
#############################
# Incorporate random effects terms into general and generalized linear models
#############################

###############################################
# Materials used in this lab
###############################################
# Chapter 5: RIKZ and barn owl data
# Zuur et al. 2009. Generalized Mixed Effects Models in R
# Crawley. 2006. The R book
###############################################

#############################
# Relevant R Datacamp Modules
#############################
# Hierarchical and mixed effects models in R
#############################

#############################################################
# Set the working directory
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 9_GLMMs")
##############################################################

###############################################
# Load required packages
###############################################
library(MuMIn)
library(nlme)
library(lattice)
library(mgcv)
library(MASS)
library(lme4)
###############################################

###############################################
# Fit mixed effects models
###############################################

# Different types of random effects (random intercept, random intercept and slope)

#Read RIKZ data from a file
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ$fBeach <- factor(RIKZ$Beach)

# Fit random intercept model for RIKZ data
Mlme1 <- lme(Richness ~ NAP, random = ~1 | fBeach,data=RIKZ)
summary(Mlme1)

# Plot random intercept model
F0<-fitted(Mlme1,level=0)
F1<-fitted(Mlme1,level=1)
I<-order(RIKZ$NAP)
NAPs<-sort(RIKZ$NAP)
plot(NAPs,F0[I],lwd=4,type="l",ylim=c(0,22),
     ylab="Richness",xlab="NAP")
for (i in 1:9){
  x1<-RIKZ$NAP[RIKZ$Beach==i]
  y1<-F1[RIKZ$Beach==i]
  K<-order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZ$NAP,RIKZ$Richness,RIKZ$Beach,cex=0.9)

# Fit random slope and intercept model for RIKZ data
Mlme2 <- lme(Richness ~ NAP,random = ~1 + NAP | fBeach, data = RIKZ)
summary(Mlme2)

# Plot random slope and intercept model
F0<-fitted(Mlme2,level=0)
F1<-fitted(Mlme2,level=1)
I<-order(RIKZ$NAP)
NAPs<-sort(RIKZ$NAP)
plot(NAPs,F0[I],lwd=4,type="l",ylim=c(0,22),
     ylab="Richness",xlab="NAP")
for (i in 1:9){
  x1<-RIKZ$NAP[RIKZ$Beach==i]
  y1<-F1[RIKZ$Beach==i]
  K<-order(x1)
  lines(sort(x1),y1[K])
}
text(RIKZ$NAP,RIKZ$Richness,RIKZ$Beach,cex=0.9)

# Test whether the random slope or the random slope and intercept model is better
anova(Mlme2,Mlme1)

# Barn owl data
# Is sibling negotiation related to food treatment, arrival time, and/or sex of the parent?
Owls<-read.delim("Owls.txt")

# Data exploration
boxplot(NegPerChick~Nest,data=Owls)
boxplot(NegPerChick~FoodTreatment,data=Owls)
boxplot(NegPerChick~SexParent,data=Owls)
plot(x=Owls$ArrivalTime,y=Owls$NegPerChick)

# Look at a histogram of the response variable (SiblingNegotiation)
hist(Owls$SiblingNegotiation)
# log-transform sibling negotiation
Owls$LogNeg<-log10(Owls$NegPerChick+1)

# Fit a model with a random intercept term for nest
# Use the lme function in nlme package
# Full model with all variables
M1.lme=lme(LogNeg~SexParent*FoodTreatment+SexParent*ArrivalTime,random=~1|Nest,method="REML",data=Owls)
summary(M1.lme)

# Use AIC to select top model
# Ideally would build a candidate set of models
# For convenience, we will use Dredge from the muMin package to look at all possible combinations
# Note that we use maximum likelihood here "ML" instead of restricted maximum likelihood "REML" for model comparison
options(na.action = "na.fail")
full <- lme(LogNeg~SexParent*FoodTreatment+SexParent*ArrivalTime,random=~1|Nest,method="ML",data=Owls)
dredge<-dredge(full,rank = "AICc")
# View the model table with dAIC <2
print(dredge[dredge$delta < 2])  
# Extract the best model
bestMod<-get.models(dredge, subset = 1)[[1]] 
bestMod
# Refit the top model using restricted maximum likelihood
bestModOutput <- lme(LogNeg~ArrivalTime+FoodTreatment,random=~1|Nest,method="REML",data=Owls) # Fit best model
summary(bestModOutput)

# Model validation part 1
E2<-resid(bestModOutput,type="normalized")
F2<-fitted(bestModOutput)
op<-par(mfrow=c(2,2),mar=c(4,4,3,2))
MyYlab="Residuals"
# plot residuals versus fitted values
plot(x=F2,y=E2,xlab="Fitted values",ylab=MyYlab)
# Plot response against variables included and excluded from the model
boxplot(E2~SexParent,data=Owls,main="Sex of parent",ylab=MyYlab)
boxplot(E2~FoodTreatment,data=Owls,main="Food treatment",ylab=MyYlab)
plot(x=Owls$ArrivalTime,y=E2,main="Arrival time",ylab=MyYlab,xlab="Time (hours)")
par(op)

# Model validation part II
# Plot residuals against arrival time for each treatment level and sex of the parent
xyplot(E2~ArrivalTime|SexParent*FoodTreatment,data=Owls,
       ylab="Residuals",xlab="Arrival time (hours)",
       panel=function(x,y){
         panel.grid(h=-1, v= 2)
         panel.points(x,y,col=1)
         panel.loess(x,y,span=0.5,col=1,lwd=2)})
# Plot shows possible patterns in the residuals

# Check whether residuals of the random effect are normally distributed
hist(bestModOutput$coefficients$random$Nest)

#################################
# Generalized mixed effects model
#################################

# Researchers wanted to know whether a treatment helped cure a bacterial infection.
# Patients were tested for the presence of a bacterial infection on several occasions (weeks).
# The response variable is binary (infected or not infected), so the appropriate family is binomial.
# The explanatory variable (fixed effect) is treatment with three levels (drug, drug plus supplement, and placebo)
# load the data (from package MASS)
attach(bacteria)
# look at the column names
names(bacteria)
# y is the response variable coded as 'n' or 'y'
# first need to convert y to a vector of 1s and 0s
y<-1*(y=="y")
table(y,trt)
# fit a mixed effects model using glmer in lme4 package
model1<-glmer(y~trt+(week|ID),family=binomial,data=bacteria)
summary(model1)


##############
# Questions
##############

# 1. Researchers are interested in examining the effects of a fertilizer on plant growth.
# They measure root growth as a response variable.
# There are six replicate plants in each treatment, with each plant measured five times (after 2, 4, 6, 8, or 10 weeks of growth)
# Data are in the file fertilizer.txt
# First, read and inspect the data
# Make a plot as follows:
fertilizer<-groupedData(root~week|plant,outer = ~ fertilizer,fertilizer)
plot(fertilizer) # gives results for each plant separately
plot(fertilizer,outer=T) # gives results for all plants on the same plot for the two treatment groups
# Build a linear mixed effects model using lme where the fixed effects are root~fertilizer and the random effect ~week|plant (nested random effect)
# Inspect the model output. What is the mean reduction in root length in the control group?

# 2. In this example, researchers were looking at the relationship between plant size and point measurements of soil nitrogen 
# at five places on each of 24 farms
# These data are in "farms.txt"
# Read and inspect the data. Make sure the variable 'farm' is a factor.
# Make a scatterplot of nitrogen versus plant size
# Build a model with fixed effects of nitrogen and farm. Include farm as a random intercept.
# Compare the first model with models that include the interaction between nitrogen and farm as well as models with either Nitrogen or farm.
# Make sure to fit the models with maximum likelihood.
# Using the AIC, which model is better?
# Re-fit the top model with REML.
# What is the effect of nitrogen on plant size?



#####################
# Answers
#####################

# Question 1
fertilizer<-read.delim("fertilizer.txt")
# Build Model
model<-lme(root~fertilizer,random=~week|plant, data=fertilizer)
summary(model)
# Mean reduction in root length
# 1.039

# Question 2
yields<-read.delim("farms.txt")
plot(yields$N,yields$size,pch=16,col=yields$farm,xlab="Nitrogen amount",ylab="Yield")
m1 <- lme(size ~ N + farm, random = ~1|farm,method="ML", data=yields)
m2 <- lme(size ~ N * farm, random = ~1|farm,method="ML", data=yields)
m3 <- lme(size ~ N, random = ~1|farm,method="ML", data=yields)
m4 <- lme(size ~ farm, random = ~1|farm,method="ML", data=yields)
AIC(m1)
AIC(m2)
AIC(m3)
AIC(m4)
m5 <- lme(size ~ N, random = ~1|farm,method="REML", data=yields)
summary(m5)

