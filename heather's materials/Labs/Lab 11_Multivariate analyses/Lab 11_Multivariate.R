# For next time: Look up a more interesting example for the NMDS that shows patterns
# More implications of a PCA analysis
# Prepare a couple more example questions at the end
# Maybe replace Discriminant function analysis with something more interesting (e.g., CART)
# Lab is currently long enough


####################################
# Lab 11
# Multivariate Analyses
####################################

########################
# Topics:
########################
# PCA
# NMDS
# Discriminant function analysis
##########################

###############################################
# Materials used in this lab
# Analysing Ecological Data (Zuur et al): Chapters 12, 14
# NMDS Example: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
###############################################

#############################
# Relevant R Datacamp Modules
#############################
# Multivariate Probability Distributions in R (includes a chapter on PCA and NMDS)
# Machine Learning with Tree-Based Models in R (Classification and Regression Trees)
# Tree-based models in R (These last two Chapters have similar content)
# Supervised Learning in R: Classification (Classification trees, logistic regression)
#############################

#############################################################
# Set the working directory
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 10_Multivariate analyses")
##############################################################

###############################################
# Load required packages
library(lattice)
library(mgcv)
library(MASS)
library(vegan)
library(ade4)
source("MyLibrary.R")
###############################################

###############################################
# PCA
###############################################

# data on seven morphological variables measured on >1000 sparrows
# data from Chris Elphick, University of Connecticut, USA
# Can we reduce the number of variables that explain the variation in the data?
sparrows<-read.table("Sparrows2.txt", header=T)

# look at correlations between variables
Z <- cbind(sparrows$wingcrd,sparrows$flatwing,sparrows$tarsus,sparrows$head,sparrows$culmen,sparrows$nalospi,sparrows$wt)
pairs(Z,lower.panel=panel.cor,labels = c("Wing chord","Flat Wing","Tarsus","Head","Culmen","Nalopsi","Weight"))

# Run the PCA
# First standardize the variables using the "standardize" option of decostand function (vegan package)
sparrows_norm<-decostand(sparrows[,1:7], "standardize")
# Princomp runs the PCA
spar_PCA<-princomp(sparrows_norm)

# Screeplot
# Decide which axes to interpret
plot(spar_PCA$sdev^2/sum(spar_PCA$sdev^2), type="h", lwd=3, 
     xlab="Axes", ylab="Scaled Eigenvalues")

# Look at the Eigenvalues to see what percentage of the variation in the data is explained by each component
CumProp <- 0
for (i in 1:4) {
  cat(paste("Axis", i, sep = ""), "\t", 
      "EigScaled", round(spar_PCA$sdev[i]^2/sum(spar_PCA$sdev^2), 3), "\t",
      "Eig", round(spar_PCA$sdev[i]^2, 3), "\t",  
      "CumProp%", round(CumProp <- CumProp+spar_PCA$sdev[i]^2/ 
                          sum(spar_PCA$sdev^2)*100, 2), "\n")
}

# Plot the results using a biplot
biplot(spar_PCA, scale=1, cex=c(0.4, 1), arrow.len=0, main="Correlation biplot")
abline(h=0, lty=2)
abline(v=0, lty=2)

# Look at loadings for each variable to determine what the axes mean
# These tell you how strongly each variable is correlated with each principal component (Comp.1, Comp.2, etc)
spar_PCA$loadings



#############################################
# Non-metric multidimensional scaling (NMDS)
#############################################

# Code from: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
# Please see the original post for more detailed explanation

# Generate a dataset that has data from 10 communities in rows 1-10 and numbers of different species in columns 2-30
set.seed(2)
community_matrix=matrix(
  sample(1:100,300,replace=T),nrow=10,
  dimnames=list(paste("community",1:10,sep=""),paste("sp",1:30,sep="")))
head(community_matrix)

# This next line of code runs an NMDS on our made-up data
# We have to specify how many dimensions we want in our ordination
# Often people only interpret the first two axes of variation (there are up to 30 potential axes of variation in this dataset, 1 for each species)
example_NMDS=metaMDS(community_matrix, # Our community-by-species matrix
                     k=2,#The number of reduced dimensions
                     distance = 'bray', # distance measure (defaults to bray-curtis, which is recommended for abundance data)
                     trymax = 100) # trymax is the number of default iterations used to find a solution

# And we can look at the NMDS object
example_NMDS # metaMDS has automatically applied a square root 
# transformation and calculated the Bray-Curtis distances for our 
# community-by-site matrix
# Stress reflects the discrepancy between the fitted curve and the real distances among points
# Stress <0.05 is excellent, stress above 0.1 means the output should be interpreted with caution, and stress >0.2 indicates that you may need to interpret more axes of variation

# Let's examine a Shepard plot, which shows scatter around the regression
# between the interpoint distances in the final configuration (distances 
# between each pair of communities) against their original dissimilarities
stressplot(example_NMDS)
# Large scatter around the line suggests that original dissimilarities are
# not well preserved in the reduced number of dimensions

#Now we can plot the NMDS
plot(example_NMDS)
# It shows us both the communities ("sites", open circles) and species 
# (red crosses), but we  don't know which are which!

# We can use the functions `ordiplot` and `orditorp` to add text to the 
# plot in place of points
ordiplot(example_NMDS,type="n")
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",cex=1.25,air=0.01)

# There are some additional functions that might of interest
# Let's suppose that communities 1-5 had some treatment applied, and 
# communities 6-10 a different treatment
# We can draw convex hulls connecting the vertices of the points made by
# these communities on the plot
# First, let's create a vector of treatment values:
treat=c(rep("Treatment1",5),rep("Treatment2",5))
# Next, create a vector of color values corresponding of the 
# same length as the vector of treatment values
colors=c(rep("red",5),rep("blue",5))
ordiplot(example_NMDS,type="n")
#Plot convex hulls with colors baesd on treatment
for(i in unique(treat)) {
  ordihull(example_NMDS$point[grep(i,treat),],draw="polygon",
           groups=treat[treat==i],col=colors[grep(i,treat)],label=F) } 
orditorp(example_NMDS,display="species",col="red",air=0.01)
orditorp(example_NMDS,display="sites",col=c(rep("green",5),
                                            rep("blue",5)),air=0.01,cex=1.25)


#################################
# Discriminant Function Analysis
#################################

# What if we want to know whether there are differences among pre-defined groups for multiple response variables?
# Discriminant Function Analysis can be useful to answer this question
# We will return to the sparrow dataset
Y<-read.table("SparrowDA.txt", header = T)
names(Y)
attach(Y)

# Morphological measurements were made by multiple observers (10 different people)
# Count of ovservation per observer
# numbering of observers goes from 0 to 9
table(observer)
# Count of observations is between 9 and 297
# so we have serious problem with unequal group sizes.
# Need to drop the observers: No. 0 and No. 9
Y<-Y[(observer!= 0)&(observer!= 9), ]
detach(Y)
attach(Y)

# Look for differences among observers for the variable flatwing
boxplot(flatwing ~ observer, data = Y, outline = T, width = table(observer)^0.5, 
        xlab = "flatwing")
#      Other boxplots
par(mfrow = c(3, 2))
boxplot(wingcrd ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "wingcrd")
boxplot(tarsus ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "tarsus")
boxplot(head ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "head")
boxplot(culmen ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "culmen")
boxplot(nalospi ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "nalospi")
boxplot(wt ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "wt")
par(mfrow = c(1, 1))

#       Correlation and pairplot of variables
plot(Y[, 1:7], lower.panel = panel.cor)
# One of wingcrd and flatwing should be removed
# because of high correlation (0.99)
# we decided to drop wingcrd  (first column).

# Data preparation
# De-seasonalisation of weight variable
Y$wtnew<-residuals(lm(wt~factor(Month), data=Y))
#Normalisation of data
detach(Y)
Y$flatwing<-(Y$flatwing-mean(Y$flatwing))/sd(Y$flatwing)
Y$tarsus<-(Y$tarsus-mean(Y$tarsus))/sd(Y$tarsus)
Y$head<-(Y$head-mean(Y$head))/sd(Y$head)
Y$culmen<-(Y$culmen-mean(Y$culmen))/sd(Y$culmen)
Y$nalospi<-(Y$nalospi-mean(Y$nalospi))/sd(Y$nalospi)
Y$wtnew<-(Y$wtnew-mean(Y$wtnew))/sd(Y$wtnew)
attach(Y)

discrim2<-lda(observer ~ flatwing + tarsus + head + culmen + nalospi + wtnew, data = Y)
discrim2
# Coefficients of linear discriminants for LD1: 
# give coefficients of first dicriminant function (14.4)

# Plot the discriminant functions 1 and 2
plot(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], type = "n", 
     xlab = "Discriminant function 1", ylab = "Discriminant function 2")
text(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], labels = observer)

# Plot 90% tolerance intervals for each observer
group_average.1<-tapply(predict(discrim2)$x[, 1], observer, mean)
group_average.2<-tapply(predict(discrim2)$x[, 2], observer, mean)
plot(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], type = "n", 
     xlab = "Discriminant function 1", ylab = "Doscriminant function 2")
# points(group_average.1, group_average.2, pch = 17, cex = 2)
text(group_average.1, group_average.2, 1:8,cex = 1)
Radius <- 2.15
#NG = number of groups
NG <- 8
for (i in 1:NG) {
  x3 <- vector(length=72)
  y3<- vector(length=72)
  t1 <- 0
  for (j in 1:72) {
    x3[j] <- Radius * sin(t1)+ group_average.1[i]
    y3[j] <- Radius * cos(t1)+ group_average.2[i]
    t1 <- t1 + 3.1416/36
  }
  lines(x3,y3)
}
abline(0,0)
abline(h=0,v=0)


# Plot correlation coefficients between first two axes and each of the original variables
correlation<-cor(x = Y[, c(2:6,11)], y = predict(discrim2)$x)
plot(c(-0.8, 0.8), c(-0.8, 0.8), type = "n", 
     xlab = "Discriminant function 1", ylab = "Discriminant function 2")
lines(c(-1, 1), c(0, 0), lty = 2)
lines(c(0, 0), c(-1, 1), lty = 2)
s.arrow(correlation[, 1:2], grid = F, add.plot = T, addaxes = T, clabel = 1.5)
correlation
# Correlation coefficients between DA axis 
# and each of the original variables

# Next steps: there are hypothesis testing procedures you could use to figure out which observers are significantly different
# and which variables observers differ on. See Zuur et al. Chapter 14 for more details.

#############
# Questions
#############

# 1. Use the iris data (in R) to perform a PCA. You can load the iris data using attach(iris). 
# Inspect the data to make sure it loaded properly
# Perform a PCA on standardized data
# Look at the eigenvalues and a screeplot to decide how many axes of variation to interpret.
# Look at the Eigenvalues to see what percentage of the variation in the data is explained by each component
# Use a biplot and loadings to interpret what the axes (Comp.1 and Comp.2) mean


#############
# Answers
#############

# 1. Simple PCA on iris data
attach(iris)
head(iris)
str(iris)
summary(iris)
dim(iris)
iris_norm<-decostand(iris[,1:4], "standardize")

# Run the PCA
PCA_iris <- princomp(iris_norm)

# Screeplot
# Decide which axes to interpret
plot(PCA_iris$sdev^2/sum(PCA_iris$sdev^2), type="h", lwd=3, 
     xlab="Axes", ylab="Scaled Eigenvalues")

# Look at the Eigenvalues to see what percentage of the variation in the data is explained by each component
CumProp <- 0
for (i in 1:4) {
  cat(paste("Axis", i, sep = ""), "\t", 
      "EigScaled", round(PCA_iris$sdev[i]^2/sum(PCA_iris$sdev^2), 3), "\t",
      "Eig", round(PCA_iris$sdev[i]^2, 3), "\t",  
      "CumProp%", round(CumProp <- CumProp+PCA_iris$sdev[i]^2/ 
                          sum(PCA_iris$sdev^2)*100, 2), "\n")
}

# Plot the results using a biplot
biplot(PCA_iris, scale=1, cex=c(0.4, 1), arrow.len=0, main="Correlation biplot")
abline(h=0, lty=2)
abline(v=0, lty=2)

# Look at loadings for each variable to determine what the axes mean
# These tell you how strongly each variable is correlated with each principal component (Comp.1, Comp.2, etc)
PCA_iris$loadings

# Increasing Comp 1 represents plants with greater sepal length, petal length, and petal width
# Comp 1 represents plants with greater sepal width
