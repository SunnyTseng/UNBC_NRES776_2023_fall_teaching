
####################################
# NRES 776
# Lab 8: Generalized Linear Models
####################################

#############################
# Topics
#############################
# Analyze count data using poisson and negative binomial distributions
# Analyze presence-absence data using a binomial distribtuion (aka Logistic Regression)
# Analyze proportional data using a binomial distribution (aka Logistic Regression)
#########################

###############################################
# Materials used in this lab
# Chapter 9: GLM and GAM for Count Data (Amphibian Road Kill Data)
# Chapter 10: GLM and GAM for Absence-presence and proportional data
# From Zuur et al. 2009. Generalized Mixed Effects Models in R
###############################################

#############################
# Relevant R Datacamp Modules
#############################
# Generalized linear models in R
# Multiple and Logistic Regression in R (Chapter 4 is on Logistic Regression)
#############################

#############################################################
# Set the working directory
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 8_GLMS")
##############################################################


###############################################
# Load required packages
###############################################
library(MuMIn)
library(car)
library(MASS)
# Load custom package to create pairplots and dotplots
source("MyLibrary.R")
###############################################


########################################################
# Count Data (Poisson, quasi-poisson, negative binomial)
########################################################

# Amphibian roadkill Data 
# Data are counts of the number of species of amphibians killed at 52 locations along a road in Portugal
# Predictor variables are characteristics of the locations where the animals were killed, including D.PARK, which is the distance to the nearest park
# The authors completed the surveys along a stretch of road that was 27 km long every 2 weeks between March 1995 and March 1997
# Road was divided into 500-m segments
# They calculated a number of landscape-level variables using landscape data from aerial images within a 2000-m strip centred on each segment

# read in the data
RK<-read.delim("RoadKills.txt")

# What is the relationship between # roadkills and distance to the park?
plot(RK$D.PARK,RK$TOT.N,xlab="Distance to park",ylab="Road kills")

# Build a model of roadkills as a function of distance to park
# Note: use glm instead of lm for generalized linear models
# We will use a poisson family (count data are often poisson distributed)
# Poisson family uses a logarithm link function
M1<-glm(TOT.N~D.PARK,family=poisson,data=RK)

# look at the results
summary(M1)

# look at how well the model fits the data
# need to transform the predicted values to get them on the scale of the original data
# create a new dataframe that covers the range of distance to park and increases in increments of 1000 m
MyData=data.frame(D.PARK=seq(from=0,to=25000,by=1000))
# use model to predict new values of y for each value of x
G<-predict(M1,newdata=MyData,type="link",se=T)
# transform predicted values using inverse of logarithm function (exp)
F<-exp(G$fit)
# caluclate upper confidence interval for predicted values of y
FSEUP<-exp(G$fit+1.96*G$se.fit)
# calculate lower confidence interval for predicted values of y
FSELOW<-exp(G$fit-1.96*G$se.fit)
# add lines to the plots
lines(MyData$D.PARK,F,lty=1)
lines(MyData$D.PARK,FSEUP,lty=2)
lines(MyData$D.PARK,FSELOW,lty=2)

# Now we'll try looking at a few more variables and apply model selection
# We'll square root transform some of these variables due to large values
RK$SQ.POLIC<-sqrt(RK$POLIC) # Area used for polyculture, a mixed-species type of agriculture (ha)
RK$SQ.WATRES<-sqrt(RK$WAT.RES) # Area of water reservoirs (ha)
RK$SQ.URBAN<-sqrt(RK$URBAN) # Amount of urban area (ha)
RK$SQ.OLIVE<-sqrt(RK$OLIVE) # Olive groves (ha)
RK$SQ.LPROAD<-sqrt(RK$L.P.ROAD) # Paved road length (km)
RK$SQ.SHRUB<-sqrt(RK$SHRUB) # Area of shrubs (ha)
RK$SQ.DWATCOUR<-sqrt(RK$D.WAT.COUR) # Distance to water courses

# Now we'll fit a more complex model with nine variables
full<-glm(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
          SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
          SQ.DWATCOUR+D.PARK,family=poisson,data=RK)
summary(full)

# We can use AIC scores to find the top model set
# Ideally would build a candidate set of models
# For convenience, we will use Dredge from the muMin package to look at all possible combinations
options(na.action = "na.fail")
dredge<-dredge(full,rank = "AICc")
# View the model table with dAIC <2
print(dredge[dredge$delta < 2])  

# Extract the best model
bestMod<-get.models(dredge, subset = 1)[[1]]            
# View output of best model
bestModOutput <- glm(bestMod, family=poisson, data=RK) # Fit best model
summary(bestModOutput)

# Check for over-dispersion
# Dispersion = D/(n-p) where D = residual deviance, n-p is the number of samples minus the number of parameters
272.5/43
# Since ratio is >1, it suggests there is overdispersion

# First we'll try a quasi-poisson model to deal with the overdispersion
# We'll use the same link function (logarithm)
# but will estimate a dispersion parameter that scales the variance 
quasiPoissonModel<-glm(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
                         SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
                         SQ.DWATCOUR+D.PARK,family=quasipoisson,data=RK)
summary(quasiPoissonModel)

# Model selection (no AIC!)
drop1(quasiPoissonModel,test="F")

# Drop the SQ.DWATCOUR variable and re-fit the model
quasiPoissonModel2<-glm(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
                         SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
                         D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel2,test="F")

# Drop the OPEN.L variable and re-fit the model
quasiPoissonModel3<-glm(TOT.N~MONT.S+SQ.POLIC+
                          SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
                          D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel3,test="F")

# Drop the SQ.WATRES variable and re-fit the model
quasiPoissonModel4<-glm(TOT.N~MONT.S+SQ.POLIC+
                          SQ.SHRUB+L.WAT.C+SQ.LPROAD+
                          D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel4,test="F")

# Drop the SQ.LPROAD variable and re-fit the model
quasiPoissonModel5<-glm(TOT.N~MONT.S+SQ.POLIC+
                          SQ.SHRUB+L.WAT.C+
                          D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel5,test="F")

# Drop the SQ.SHRUB variable and re-fit the model
quasiPoissonModel6<-glm(TOT.N~MONT.S+SQ.POLIC+
                          L.WAT.C+
                          D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel6,test="F")

# Drop the SQ.POLIC variable and re-fit the model
quasiPoissonModel7<-glm(TOT.N~MONT.S+
                          L.WAT.C+
                          D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel7,test="F")

# Drop the MONT.S variable and re-fit the model
quasiPoissonModel8<-glm(TOT.N~
                          L.WAT.C+
                          D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel8,test="F")

# Drop the L.WAT.C+ variable and re-fit the model
quasiPoissonModel9<-glm(TOT.N~D.PARK,family=quasipoisson,data=RK)
drop1(quasiPoissonModel9,test="F")

# Model validation
M5 <- glm(TOT.N ~ D.PARK, family = quasipoisson, data = RK)
EP=resid(M5,type="pearson")
ED=resid(M5,type="deviance")
mu=predict(M5,type="response")
E=RK$TOT.N-mu
EP2=E/sqrt(7.630148*mu)
op <- par(mfrow=c(2,2))
plot(x=mu,y=E,main="Response residuals") # ordinary residuals that you would use for a linear model (not appropriate here)
plot(x=mu,y=EP,main="Pearson residuals") # residuals scaled by the square root of the mean
plot(x=mu,y=EP2,main="Pearson residuals scaled") # scaled by dispersion parameters
plot(x=mu,y=ED,main="Deviance residuals") # similar to Pearson's residuals, default used by R for poinsson glm (not appropriate if lots of zeros)
par(op)

# The plot shows patterns in the residuals so we will try a negative binomial model
M6<-glm.nb(TOT.N~OPEN.L+MONT.S+SQ.POLIC+
             SQ.SHRUB+SQ.WATRES+L.WAT.C+SQ.LPROAD+
             SQ.DWATCOUR+D.PARK,link="log",data=RK)

# Model selection
# Could use AIC for negative binomial (MuMin or stepAIC(M6))
# Could also use drop1(M6, test = "Chi")

# Using the drop1 command, you get:
M8<-glm.nb(TOT.N~OPEN.L+D.PARK,link="log",data=RK)
summary(M8)
drop1(M8,test="Chi")
par(mfrow=c(2,2))

# Model validation
plot(M8)

##################################
# Binomial GLM
##################################
# Parasites in cod
# Cod can be infected by trypanosome blood parasites
# Researchers Hemmingsen et al (2005) examined the prevalence of these blood parasites in cod (n=1254) along the coast of Finnmark in Northern Norway

# Variables
# Prevalence of trypanosome parasites (0 for absent or 1 for present)
# year, area, length, interaction of year and length

# Read and inspect the data
ParasiteCod<-read.delim("ParasiteCod.txt")
head(ParasiteCod)
# How many different years are included in the dataset?
# How many areas?

# Convert area and year to factors
ParasiteCod$fArea <- factor(ParasiteCod$Area, levels=c(4,3,2,1))
ParasiteCod$fYear <- factor(ParasiteCod$Year)

# Remove cases with missing data for Length (missing data means you can't do model comparison using AIC)
ParasiteCod<-ParasiteCod[!is.na(ParasiteCod$Length),]

# Look at the data
table(ParasiteCod$Prevalence,ParasiteCod$fArea)
table(ParasiteCod$Prevalence,ParasiteCod$fYear)
boxplot(ParasiteCod$Length~ParasiteCod$Prevalence)

# Fit full model
P1 <- glm(Prevalence ~ fArea * fYear + Length, family = binomial, data = ParasiteCod)
summary(P1)

# Model Selection
# We can use model selection here using AIC
# We can use AIC scores to find the top model set
# Ideally would build a candidate set of models
# For convenience, we will use Dredge from the muMin package to look at all possible combinations
options(na.action = "na.fail")
full <- glm(Prevalence ~ (fArea + fYear + Length)^2, family = binomial, data = ParasiteCod)
dredge<-dredge(full,rank = "AICc")
# View the model table with dAIC <2
print(dredge[dredge$delta < 2])  
# Extract the best model
bestMod<-get.models(dredge, subset = 1)[[1]]            
# View output of best model
bestModOutput <- glm(bestMod, family=binomial, data=ParasiteCod) # Fit best model
summary(bestModOutput)

# Model Validation
# Luckily we don't have to worry about over-dispersion with Bernouli distribution (presence-absence data)
# Residuals show two clear bands, so difficult to say anything about model fit from graphs
plot(bestModOutput)

##########################################################
# Proportional Data

# Researchers in Spain looked at the prevalence of parasite Elaphostrongylus cervi in red deer (elk)
# They recorded the data as the proportion of deer infected per farm
# They looked at whether each farm was fenced or not, the percentage of open land, shrubs, 
# the number of quercus plants per area, the number of quercus trees per area, a wild boar abundance index, a red deer abundance index,
# and farm size (ha)

# Read in the data
Tbdeer<-read.delim("Tbdeer.txt")

# Drop missing values (farms where no deer were sampled, or where predictor variables are missing data)
Tbdeer2<-na.omit(Tbdeer)

# look at correlations between predictor variables
Z <- cbind(Tbdeer$OpenLand,Tbdeer$ScrubLand,Tbdeer$QuercusPlants,Tbdeer$QuercusTrees,
           Tbdeer$ReedDeerIndex,Tbdeer$EstateSize,Tbdeer$Fenced)
pairs(Z,lower.panel=panel.cor,labels = c("OpenLand","Scrubland","QuercusPlants","QuercusTrees","ReedDeerIndex","EstateSize","Fenced"))
# any issues with colinearity here?


# Next we will fit the model
# Response variable has two columns; number of negatives and number of positives
Tbdeer2$DeerNegCervi <- Tbdeer2$DeerSampledCervi - Tbdeer2$DeerPosCervi
Tbdeer2$fFenced <- factor(Tbdeer2$Fenced)
Deer1=glm(cbind(Tbdeer2$DeerPosCervi,Tbdeer2$DeerNegCervi)~
            OpenLand+ScrubLand+QuercusPlants+QuercusTrees+
            ReedDeerIndex+ EstateSize+fFenced,
          family=binomial, data = Tbdeer2)
summary(Deer1)

# Another way to fit the same model
Tbdeer2$DeerPosProp <- Tbdeer2$DeerPosCervi/Tbdeer2$DeerSampledCervi
Deer2 <- glm(DeerPosProp ~ OpenLand + ScrubLand +
               QuercusPlants + QuercusTrees +
               ReedDeerIndex + EstateSize + fFenced,
             family = binomial, data = Tbdeer2,
             weights = DeerSampledCervi)

# Checking model assumptions
# Can have overdispersion in a binomial GLM with proportions
# Check for overdispersion
152.79/15
# 10.186 --this result indicates overdispersion

# We'll to deal with overdispersion with a quasibinomial model
Deer3<-glm(cbind(DeerPosCervi,DeerNegCervi)~ OpenLand+ScrubLand+QuercusPlants+QuercusTrees+ReedDeerIndex+ EstateSize+fFenced,family=quasibinomial, data = Tbdeer2)
summary(Deer3)

# Model Selection
# Can't easily use AIC (or QAIC, quasi-AIC), so we will use drop 1 command
drop1(Deer3,test="F")

Deer4<-glm(cbind(DeerPosCervi,DeerNegCervi) ~
                    OpenLand, data = Tbdeer2,
                    family = quasibinomial)
drop1(Deer4, test = "F")

# Plot model output
MyData <- data.frame(OpenLand = seq(from = min(Tbdeer$OpenLand),
                                    to = max(Tbdeer2$OpenLand),by=0.01))
P1 <- predict(Deer4, newdata = MyData, type = "link", se = TRUE)
plot(MyData$OpenLand,exp(P1$fit)/(1+exp(P1$fit)),
     type="l",ylim=c(0,1),
     xlab="Percentage open land",
     ylab="Probability on E. cervi")
lines(MyData$OpenLand,exp(P1$fit+1.96*P1$se.fit)/
        (1+exp(P1$fit+1.96*P1$se.fit)),lty=2)
lines(MyData$OpenLand,exp(P1$fit-1.96*P1$se.fit)/
        (1+exp(P1$fit-1.96*P1$se.fit)),lty=2)
points(Tbdeer2$OpenLand,Tbdeer2$DeerPosProp)

# Model Validation
# Plot the Pearson or Deviance residuals against fitted values
# Plot residuals versus each explanatory variable in the model, including those that were dropped
# Model validation
EP=resid(Deer4,type="pearson")
ED=resid(Deer4,type="deviance")
mu=predict(Deer4,type="response")
E=Tbdeer2$DeerPosProp-mu
EP2=E/sqrt(9.773897*mu) # dispersion parameter from estimated by quasibinomial model
op <- par(mfrow=c(2,2))
plot(x=mu,y=E,main="Response residuals") # ordinary residuals that you would use for a linear model (not appropriate here)
plot(x=mu,y=EP,main="Pearson residuals") # residuals scaled by the square root of the mean
plot(x=mu,y=EP2,main="Pearson residuals scaled") # scaled by dispersion parameters
plot(x=mu,y=ED,main="Deviance residuals") # similar to Pearson's residuals, default used by R for poinsson glm (not appropriate if lots of zeros)
par(op)


##############
# Questions
##############

# 1. Researchers collected data on the presence or absence of tuberculosis-like lesions in wild boar in south-central spain
# They want to know whether boars with tuberculosis are larger, as measured by body length. 
# The variable body length is denoted LengthCT (where CT is for cabeza-tronco, Spanish for head-body)
# TB is a vector of 1s and 0s, representing presence or absence of Tb
# Read and inspect the data (boar.txt)
# Make boxplot showing body length for boars with and without TB. Does it look like boars with TB are larger?
# Fit a glm with family = binomial
# Is there a significant relationship between LengthCT and infection status?
# Calculate and interpret the odds ratio of the relationship between LengthCT and infection status.(Hint: the odds ratio is exp(Coefficient))
# Make a plot showing the raw data and the predicted values of the relationship between Length CT and the probability of having TB

# 2. Read in the data set "beetle.txt" (hint: use read.csv and check that the data read in properly)
# Make sure "condit" and "disper" are recognized as factors (i.e. categorical variables, not continuous)
# In the "beetle' data set, the number of mates each female attracted was also recorded.
# What distribution likely best represents the number of mates?
# Run linear first order models (i.e, no interaction terms) on beetle$nummates using first only the categorical variables, and then the categorical and continuous variables
# Evaluate the overall fits of these models
# Specify glm models that have the same structure as the liner model used above, using an appropriate linker function
# Evaluate the fit of the glm models compared to the linear models
# Check whether there is over-dispersion. If so, what could you do?
# Use a model selection approach to choose an optimal model


#####################
# Answers
#####################

# Question 1
boar<-read.delim("boar.txt")
# remove missing cases
boar2<-boar[!is.na(boar$LengthCT)&!is.na(boar$Tb),]
boxplot(boar2$LengthCT~boar2$Tb)
boarMod1 <- glm(Tb ~ LengthCT, family = binomial, data = boar2)
summary(boarMod1)
exp(0.031606)
# Plot model output
MyData <- data.frame(LengthCT = seq(from = min(boar2$LengthCT),
                                    to = max(boar2$LengthCT),by=0.01))
P1 <- predict(boarMod1, newdata = MyData, type = "link", se = TRUE)
plot(MyData$LengthCT,exp(P1$fit)/(1+exp(P1$fit)),
     type="l",ylim=c(0,1),
     xlab="Length CT",
     ylab="Probability of Tb Infection")
lines(MyData$LengthCT,exp(P1$fit+1.96*P1$se.fit)/
              (1+exp(P1$fit+1.96*P1$se.fit)),lty=2)
lines(MyData$LengthCT,exp(P1$fit-1.96*P1$se.fit)/
              (1+exp(P1$fit-1.96*P1$se.fit)),lty=2)
points(boar2$LengthCT,boar2$Tb)

# Question 2
beetle<-read.csv("beetle.txt")
beetle$condit<-factor(beetle$condit)
beetle$disper<-factor(beetle$disper)
# Run linear first order models (i.e, no interaction terms) on beetle$nummates using first only the categorical variables, and then the categorical and continuous variables
m1<-lm(nummates~condit+disper,data=beetle)
m2<-lm(nummates~condit+disper+elytra+mass+chamber+eggnum+eggmass,data=beetle)

# Evaluate the overall fits of these models
plot(m1)
plot(m2)

# Specify glm models that have the same structure as the liner model used above, using an appropriate linker function
m3<-glm(nummates~condit+disper,family=poisson, data=beetle)
m4<-glm(nummates~condit+disper+elytra+mass+chamber+eggnum+eggmass,family=poisson,data=beetle)

# Evaluate the fit of the glm models compared to the linear models
plot(m3)
plot(m4)

# Can also look at Pearson residuals
# Model validation
EP=resid(m4,type="pearson")
ED=resid(m4,type="deviance")
mu=predict(m4,type="response")
E=beetle$nummates-mu
EP2=E/sqrt(1*mu)
op <- par(mfrow=c(2,2))
plot(x=mu,y=E,main="Response residuals")
plot(x=mu,y=EP,main="Pearson residuals")
plot(x=mu,y=EP2,main="Pearson residuals scaled")
plot(x=mu,y=ED,main="Deviance residuals")
par(op)

# Check whether there is over-dispersion. If so, what could you do?
# Dispersion = D/(n-p) where D = residual deviance, n-p is the number of samples minus the number of parameters
# for model 4, residual deviance from summary(m4) is 166.96 and n-p is 142
166.96/142
# Since ratio is not much greater than 1, there is little evidence of overdispersion

# Use a model selection approach to choose an optimal model
# We can use AIC scores to find the top model set
# Ideally would build a candidate set of models
# For convenience, we will use Dredge from the muMin package to look at all possible combinations
options(na.action = "na.fail")
dredge<-dredge(m4,rank = "AICc")
# View the model table with dAIC <2
print(dredge[dredge$delta < 2])  

# Extract the best model
bestMod<-get.models(dredge, subset = 1)[[1]]            
# View output of best model
bestModOutput <- glm(bestMod, family=poisson, data=beetle) # Fit best model
summary(bestModOutput)
