
#############################
# NRES 776
# Lab 7: Multiple Regression and Model Selection
#############################

#############################
# Topics
#############################
# Multiple Regression
# Model Selection
  # Develop a set of candidate models
  # Rank them using AIC
  # Determine which models to interpret
  # Calcualte model-averaged coefficients
##############################


###############################
# Load required packages
###############################
library(dplyr)
library(ggplot2)
library(car)
library(tidyr)
library(nlme)
library(dotwhisker)
library(broom)
library(MuMIn)
# Load custom package to create pairplots and dotplots
source("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 7 Model Selection/MyLibrary.R")
################################

#############################
# Relevant R Datacamp Modules
#############################
# Multiple and Logistic Regression in R (Chapters 1-3)
# Intermediate Regression in R (Chapters 1-3)
# Introduction to Statistical Modeling in R
#############################

###############################################
# Materials used in this lab
# Analyzing Ecological Data Chapters 4 and 5
# https://highstat.com/index.php/analysing-ecological-data
###############################################

####################################
# Set working directory
####################################
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 7 Model Selection")
####################################

########################################
# Multiple regression
########################################

# Data on invertebrate species at 45 sites in Holland in the intertidal
# Question: what are effects of abiotic factors on invertebrate species richness?

#Read RIKZ data from a file
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)

# Start with a model that has three variables
# Richness is number of species
# NAP is a measure of time a site is underwater
# grainsize is size of sand grains at the site
# week is the week when data were collected (factor with four levels 1-4)

# Initial data exploration
# Check for outliers using dotplots of each continuous variable
# x-axis shows value at each data point
# y-axis shows the identity of data points based on order of entry into the spreadsheet (higher y-values represent points at the end of the spreadsheet)
# Note: this won't necessarily detect multivariate outliers, but can give an initial idea of potentially problematic data points
dotchart(RIKZ$NAP, main = "NAP")
dotchart(RIKZ$grainsize, main = "Grain size")

# Check for multicolinearity
# pairplot
Z <- cbind(RIKZ$NAP,RIKZ$grainsize,
           RIKZ$week)
pairs(Z,lower.panel=panel.cor,
      labels = c("NAP","grainsize","week"))

# create a model with the three variables and look at the output
m1<-lm(Richness ~ NAP+grainsize+factor(week), data = RIKZ)
summary(m1)$coefficients
summary(m1)
anova(m1)

# standardize the continuous predictors NAP and grainsize
# centre to mean zero and standard deviation of 1
# this makes the magnitudes comparable
# can also help with colinearity, especially when interation terms are included
RIKZ$NAPscale<-scale(RIKZ$NAP)
# check that it worked
round(mean(RIKZ$NAPscale),0)
sd(RIKZ$NAPscale)
RIKZ$grainsizescale<-scale(RIKZ$grainsize)

# re-run the model with the scaled variables
m1<-lm(Richness ~ NAPscale+grainsizescale+factor(week), data = RIKZ)
summary(m1)$coefficients
summary(m1)
anova(m1)

# Coefficient plot
dwplot(m1)
dwplot(m1) %>%
relabel_predictors(c(NAPscale = "NAP", grainsizescale = "Grain Size")) +
theme_bw() + xlab("Coefficient") + ylab("") +
geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
ggtitle("Species Richness at Beaches") +
theme(plot.title = element_text(face = "bold"),
      legend.position = c(.995, .99),
      legend.justification = c(1, 1),
      legend.background = element_rect(colour="grey80"),
      legend.title.align = .5) +
scale_colour_grey(start = .4, end = .8)

# Check variance inflation factors for evidence of multicolinearity in the model
# if <5 = low correlation among predictor variables, 5-10 = moderate, >10 = strong correlation
# vif values often useful to look at if there are interaction terms in the model
vif(m1)


#########################################
# Model Selection
########################################

# Using the RIKZ data again
# Question: what are effects of abiotic factors on invertebrate species richness?

# This time we will start with a model that has 6 variables
# Richness is number of species
# NAP is a measure of time a site is underwater (height of sampling station relative to the mean tidal level)
# Humus is the amount of organic material
# grainsize is size of sand grains at the site
# week is the week in June when data were collected (factor with four levels 1-4)
# angle1 is the angle of the sampling station
# Temperature

# Check for multicolinearity
# pairplot
Z <- cbind(RIKZ$NAP,RIKZ$grainsize,RIKZ$week,RIKZ$humus,RIKZ$angle1, RIKZ$temperature)
pairs(Z,lower.panel=panel.cor,labels = c("NAP","grainsize","week","humus","angle1","temperature"))
# Are there any issues with colinearity here?

# Create a subset of data with variables of interest
# get column indices of variables we want
vars<-c("NAP","grainsize","week","humus","angle1","temperature","Richness")
RIKZ2<-RIKZ[,vars]

# standardize the continuous predictors
# centre to mean zero and standard deviation of 1
# this makes the magnitudes comparable
# can also help with colinearity, especially when interation terms are included
RIKZ2$NAPscale<-scale(RIKZ2$NAP)
RIKZ2$grainsizescale<-scale(RIKZ2$grainsize)
RIKZ2$humusscale<-scale(RIKZ2$humus)
RIKZ2$temperaturescale<-scale(RIKZ2$temperature)
RIKZ2$anglescale<-scale(RIKZ2$angle1)

# Run model with the scaled variables
m1<-lm(Richness ~ NAPscale+grainsizescale+humusscale+factor(week)+anglescale+temperaturescale, data = RIKZ2)

# extract the AIC
AIC(m1)

# How does this compare with the reduced model we looked at above?
m2<-lm(Richness ~ NAPscale+grainsizescale+factor(week), data = RIKZ2)
AIC(m2)

# What are the delta AIC values (dAIC)?
dAIC <- c(AIC(m1), AIC(m2)) - min(AIC(m1), AIC(m2))
dAIC
# What does this difference mean in terms of support for these two models?

# Calculate model weights
w <- exp(-0.5 * dAIC)
w/sum(w)
# what do these weights mean?

# Now let's select from a larger list of candidate models
# create a list of plausible models (based on a-priori hypotheses that you have thought about long and hard)
m0<-lm(Richness ~ 1, data = RIKZ2) # Null model
m1<-lm(Richness ~ NAPscale+grainsizescale+humusscale+factor(week)+anglescale+temperaturescale, data = RIKZ2)
m2<-lm(Richness ~ NAPscale+grainsizescale+humusscale+factor(week)+anglescale, data = RIKZ2)
m3<-lm(Richness ~ NAPscale+grainsizescale+humusscale+factor(week), data = RIKZ2)
m4<-lm(Richness ~ NAPscale+grainsizescale+humusscale, data = RIKZ2)
m5<-lm(Richness ~ NAP+grainsize+factor(week), data = RIKZ2)
m6<-lm(Richness ~ NAP+factor(week), data = RIKZ2)
m7<-lm(Richness ~ grainsize+factor(week), data = RIKZ2)
m8<-lm(Richness ~ NAP, data = RIKZ2)
m9<-lm(Richness ~ grainsize, data = RIKZ2)
m10<-lm(Richness ~ factor(week), data = RIKZ2)

m6<-lm(Richness ~ NAP+factor(week), data = RIKZ2)
m6i<-lm(Richness ~ NAP+factor(week)+NAP*factor(week), data = RIKZ2)
m6i<-lm(Richness ~ NAP*factor(week), data = RIKZ2)

# Create a list of the models to help with generating an AIC table
model.list=list(m0,m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
model.names = list("null","NAP+gs+hm+wk+an+tmp","NAP+gs+hm+wk+an","NAP+gs+hm+wk","NAP+gs+hm","NAP+gs+wk","NAP+wk","gs+wk","NAP","gs","wk")
ms.mumin <- model.sel(model.list)
AIC.table.full = data.frame(Model=unlist(model.names[as.numeric(rownames(ms.mumin))]),
                            df=ms.mumin$df, logLik=round(ms.mumin$logLik,2), AICc=round(ms.mumin$AICc,2),delta.AICc=round(ms.mumin$delta,2), weight=round(ms.mumin$weight,2))

AIC.table.full
# Export model comparison table:
write.table(AIC.table.full,"RIKZModelComparisonTable.csv", sep=",", row.names=FALSE)

# R also has tools to automatically select models
# Note you need to be honest if you use these tools, as they are forms of "data dregding"
# We will use "dredge" from the muMin package
options(na.action = "na.fail")
# we will only look at main effects here
# if you want to look at interaction terms, then you would add ^2 (two-way interaction terms) or ^3 (three-way interaction terms)
# for example: 
# full<-lm(Richness ~ (NAPscale+grainsizescale+humusscale+factor(week)+anglescale+temperaturescale)^2,data=RIKZ2)
# Full model without main effects only
full<-lm(Richness ~ (NAPscale+grainsizescale+humusscale+factor(week)+anglescale+temperaturescale),data=RIKZ2)
full<-lm(Richness ~ (NAPscale+grainsizescale+humusscale+factor(week)+anglescale+temperaturescale)^2,data=RIKZ2)
dredge<-dredge(full,rank = "AICc")


# view the full model selection table
print(dredge)   

# View the model table with dAIC <4
print(dredge[dredge$delta < 4])  

# View models in the 95% confidence set
# First extract model weights
w<-Weights(dredge)
# Caclulate number of models in the 95% confidence set
length(cumsum(w)[cumsum(w)< 0.95]) + 1

# Extract the best model
bestMod<-get.models(dredge, subset = 1)[[1]]            
# View output of best model
bestModOutput <- lm(bestMod, data = RIKZ2) # Fit best model
summary(bestModOutput)

# Generate a new, averaged model for prediction
summary(model.avg(dredge, subset = delta < 2))
# full average averages coefficients over all models, assuming the coefficient is 0 if the term does not occur in the model
# conditional average only averages over the models where the parameter occurs


#########################################
# Practice Questions
#########################################

# 1. Read in the data set "beetle.txt". (hint: use read.csv and check that the data read in properly) 
# What type of variables are eggmass, condit, disper, and elytra? What are the unique values of each?
# Create a lm for "eggmass" that includes the predictors "condit","disper","elytra" but no interaction terms
# use AIC(model) to calculate the AIC value for this model
# 
# 2. For the response variable "eggmass", construct all posible model combinations using "condit", "disper" and "elytra".
# Make sure to incldue a null model in your candidate model set.
# Calculate AIC values for all models.
# Which model is your top model? What is the dAIC of the null model?
# Determine which models can be disregarded and which should be kept for evaluation based on model weights.

# 3. Create a lm for "eggmass" that includes the predictors "condit","disper","elytra" and all 2-way interactions
# Use the function dredge() to choose model structure using AIC
# What is the best model? What terms are in the top models? 
# How many models have dAIC<2? How models are in the 95% confidence interval set?
# Calculate an averaged model based on the subset with dAIC<2


################
# Answers
################

# 1
# read in data
beetle <- read.csv(file = "beetle.txt",header = TRUE)
# look at data structure
str(beetle)
# condit, disper are integers. elytra, eggmass are numeric.

# unique values of each variable
unique(beetle$condit)
unique(beetle$disper)
unique(beetle$elytra)
unique(beetle$eggmass)

# Create a lm for "eggmass" that includes the predictors "condit","disper","elytra" but no interaction terms
mod1<-lm(eggmass~condit+disper+elytra,data=beetle)
# use AIC(model) to calculate the AIC value for this model
AIC(mod1)

# 2. For the response variable "eggmass", construct all posible model combinations using "condit", "disper" and "elytra".
# Make sure to incldue a null model in your candidate model set.
m0<-lm(eggmass~1,data=beetle) # this is the null model
m1<-lm(eggmass~elytra+condit+disper,data=beetle)
m2<-lm(eggmass~elytra+condit,data=beetle)
m3<-lm(eggmass~elytra+disper,data=beetle)
m4<-lm(eggmass~condit+disper,data=beetle)
m5<-lm(eggmass~elytra,data=beetle)
m6<-lm(eggmass~condit,data=beetle)
m7<-lm(eggmass~disper,data=beetle)

# Calculate AIC values for all models.

model.list=list(m0,m1,m2,m3,m4,m5,m6,m7)
model.names = list("null","1","2","3","4","5","6","7")
ms.mumin <- model.sel(model.list)
AIC.table.full = data.frame(Model=unlist(model.names[as.numeric(rownames(ms.mumin))]),
                            df=ms.mumin$df, logLik=round(ms.mumin$logLik,2), AICc=round(ms.mumin$AICc,2),delta.AICc=round(ms.mumin$delta,2), weight=round(ms.mumin$weight,2))
AIC.table.full

# Which model is your top model? What is the dAIC of the null model?
# Model 1 (elytra+condit+disper)
# Model 2 with elytra and condit has a dAIC of 1.19, suggesting it is a similarly probable model as model 1
# dAIC of the null model is 242.01, suggesting the top models explain substantially more variation in egg mass than the null model with no variables

# Determine which models can be disregarded and which should be kept for evaluation based on model weights.
# Model 1 and 2 have weights of 0.65 and 0.35, respectively. All other models have weights of 0 and can be disregarded.

# 3. Create a lm for "eggmass" that includes the predictors "condit","disper","elytra" and all 2-way interactions

full<-lm(eggmass ~ (elytra+condit+disper)^2,data=beetle)

# Use the function dredge() to choose model structure using AIC
# What is the best model? What terms are in the top models? 
# How many models have dAIC<2? How models are in the 95% confidence interval set?

options(na.action = "na.fail")
dredge<-dredge(full,rank = "AICc")
print(dredge[dredge$delta < 2]) 
w<-Weights(dredge)
# Caclulate number of models in the 95% confidence set
length(cumsum(w)[cumsum(w)< 0.95]) + 1
# there are three models with dAIC <2 and 4 models in the top 95% confidence interval set

# Best model
bestMod<-get.models(dredge, subset = 1)[[1]]            
# View output of best model
bestModOutput <- lm(bestMod, data = beetle) # Fit best model
summary(bestModOutput)
# best model has condit, disper, elytra, and condit:disper

# Calculate an averaged model based on the subset with dAIC<2
# Generate a new, averaged model for prediction
summary(model.avg(dredge, subset = delta < 2))
