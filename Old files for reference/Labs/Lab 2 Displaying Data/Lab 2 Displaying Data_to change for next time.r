# To change for next time
# Simplify!
# Introducing too many concepts
# Simplify the datasets, make them all CSV files
# Only work with variables needed
# Read in the data in the same way (read.csv)
# Make sure packages are installed ahead of time (people got lost installing packages)
# Shorten
# Take out the ggplots part and focus on base R?

#############################
# NRES 776
# Lab 2: Displaying Data
#############################

# We will be using base R and ggplot2 to make graphs today
# Base R is more flexible for customizing figures
# ggplot2 a graphics package that is used a lot and you can make figures quickly once you are up and running

#############################
# Topics
# Installing and loading packages
# Reading data into R
# Frequency plots & Tables
# Bar plots & histograms
# Scatter plots, strip plots, and violin plots
#############################

#############################
# Relevant R Datacamp Modules
# Note that R Datacamp modules are supplemental to the lab
# Completing Datacamp modules is not necessary or required for the course, but
# depending on your interests and familiarity with R, the modules may provide additional practice and/or learning opportunities
# Data Visualization in R (especially CH 1 & 2)
# Introduction to Data Visualization with ggplot2 (especially CH 1-3)
#############################

#############################
# R Graphics Resources
#############################

# Here are some graphics resources:
# Cheatsheets
# https://www.rstudio.com/resources/cheatsheets/
# http://publish.illinois.edu/johnrgallagher/files/2015/10/BaseGraphicsCheatsheet.pdf
# http://rpubs.com/SusanEJohnston/7953
# https://graphicsprinciples.github.io/

# Colorblind safe + Print version in black & white
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

# Sean Anderson's website: seananderson.ca
# Lots of useful content, including graphing workshops specifically:
# https://seananderson.ca/ggplot2-fish554/
# https://seananderson.ca/courses/11-multipanel/multipanel.pdf

# For inspiration, here is a great TED talk about data vizualization
# https://www.ted.com/talks/hans_rosling_the_best_stats_you_ve_ever_seen#t-473008

###################################################################################################

##################################
# Installing and loading packages
##################################

# R uses outside packages to perform many different tasks (one of the powerful aspects of R)
# R packges are groups of functions that perform related tasks
# New packages must first be downloaded, and then installed in "libaries" before the functions can be used
# For example the package "ggplot2" contains functions for creating a variety of graphics
# Packages must be loaded each session
# You only need to install the packages once. After that you just need to load them. 
# You can also load packages and see which ones are loaded in the "Packages" window (bottom right)
# Select "Install", type the package you would like to install

# Using the command line:
# install.packages("ggplot2") # Downloads and installs the package in the local uses R library (library location can be modified)
library(ggplot2) # Finds the package in the library and loads the functions into the R environment
# install.packages("png")
library(png)
# install.packages("RCurl")
library(RCurl)

############################
# Read in data
############################

# We will start by working with the Elton Traits database (https://esajournals.onlinelibrary.wiley.com/doi/10.1890/13-1917.1)
# These data are publicly available and have been compiled from literature
# The database contains information about the body size, diet, and foraging strategies of 9995 bird and 5400 mammal species
# We will work with the bird data today

# Start by setting your working directory
setwd("H:/NRES776Labs/Lab 2 Displaying Data 2021")
# Check that the setwd() function worked (Hint: getwd())
# Check what files are in your working directory (Hint: use dir())

#install.packages("rstudioapi")

library(rstudioapi)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Fix problem from last week
# salmon body mass data from example in lecture
# the variable "sex" reads in as logical where r interprets "F" as False
salmonData<-read.csv("chap02f2_5SalmonBodySize.csv", header = TRUE) 
# the colClasses command tells r to read the sex column as a character variable so F remains F
salmonData<-read.csv("chap02f2_5SalmonBodySize.csv", header = TRUE, colClasses = c(sex="character"))

# Read in first dataset for today's lab (Hint: the data must be stored in your working directory)
dat<-read.delim("EltonTraits_BirdFuncDat.txt",sep = "\t")

# Always check your data after reading it in to make sure there's aren't major problems
# in the Global Environment window (on left in R), you can see what data are loaded in R and the structure of the data
# There are other things to look at using the command line
head (dat) #will give you the first few rows of data
str(dat) # tells you which variables are integer, factor (i.e., categorical), etc
nrow(dat) #will give you the number of rows in your data set
ncol(dat) #will give you the number of columns in your data set
fix(dat) #will open the datafram in another window. 
##Note: you can edit values in the data when the fix window is open. You also can't continue working unless you close the window
View(dat) #will open data in another tab of your R window

##############################################
# Frequency Plots and Tables (Categorical Data)
###############################################

# First, let's look at a frequency table for the number of species in each of 5 diet categories (categorical variable with 5 levels)
# To see what the diet categories are, let's look at the levels of the factor "Diet.5Cat"
# First make sure diet is a factor
dat$Diet.5Cat<-factor(dat$Diet.5Cat)
levels(dat$Diet.5Cat)

# Now we'll use a frequency table to see how many bird species occur in each diet category
table(dat$Diet.5Cat)
# Sort by frequency so it's easier to see the highest to lowest values
sort(table(dat$Diet.5Cat), decreasing = TRUE)
sort(table(dat$Diet.5Cat), decreasing = F)
# Transform the frequency table so that it's vertical instead of horizontal
data.frame((sort(table(dat$Diet.5Cat), decreasing = T)))
# There are two missing values for the diet variable
# We will drop those rows from the data frame (Note: if you need to keep those cases for other analyses, you would have to deal with this problem in a different way)
dat2<-subset(dat, dat$Diet.5Cat!="")
# The command below drops the empty factor level from the data (otherwise the blank factor level remains even if there are no values in it)
dat2$Diet.5Cat<-factor(dat2$Diet.5Cat)
data.frame((sort(table(dat2$Diet.5Cat), decreasing = T)))

# Now let's look at a bar plot of feeding strategy
freq<-(sort(table(dat2$Diet.5Cat), decreasing = T))
# xlab and ylab set the axis labels
barplot(freq, xlab="Foraging Strategy",ylab="Number of Bird Species")

# Let's have some fun and add silhouette images to the plot in preparation for submission to Science
# We'll bring in some images from the phylopic.org website, which has a wide selection of species
caterpillar<-"http://phylopic.org/assets/images/submissions/b7ef1f85-e91c-43d4-addc-0308290ada03.original.png"
lizard<-"http://phylopic.org/assets/images/submissions/bad1295c-4349-4992-af66-5ef7bdf83e7d.512.png"
nectar<-"http://phylopic.org/assets/images/submissions/5ad96bfe-2d79-4909-a1b0-556569d7715d.512.png"
plant<-"http://phylopic.org/assets/images/submissions/b2fed6ab-ea5d-4670-ad18-3d663aec68e4.original.png"
blenny<-"http://phylopic.org/assets/images/submissions/85158918-ac63-4f3b-b23d-5a6a4e6dd5ca.512.png"
# combine urls into a string
urls <- c(caterpillar, lizard, nectar, plant, blenny)
# create a list with the images
logos <- list()
for (i in 1:length(urls))
{
  logos[[i]] <- readPNG(getURLContent(urls[i]))
}

# Load a function that will make adding the images easier (we will look at functions in a later lab, so don't worry about the structure for now)
# utility function for embedding png images at specified fractional sizes in R plots
# places the logo centred on a specified fraction of the the usr space, 
# and sizes appropriately (respects aspect ratio)
logoing_func<-function(logo, x, y, size){
  dims<-dim(logo)[1:2] #number of x-y pixels for the logo (aspect ratio)
  AR<-dims[1]/dims[2]
  par(usr=c(0, 1, 0, 1))
  rasterImage(logo, x-(size/2), y-(AR*size/2), x+(size/2), y+(AR*size/2), interpolate=TRUE)
}

# Re-generate the plot with images
barpos<-barplot(freq, xlab="Foraging Strategy",ylab="Number of Bird Species",ylim=c(0,5500))
# Convert x and y coordinates of the original plot to those of the logos so they can be placed above the bars
# Use the grconvert function for this
logoX <- grconvertX(barpos, from="user", to="npc")
logoY <- grconvertY(freq, from="user", to="npc") + 0.05
# loop through the logos and place them above each bar
for (i in 1:length(logos))
{
  logoing_func(logos[[i]], x=logoX[i], y=logoY[i], size=0.1)
}

# There different ways of exporting figures from R
# For example, you can use the Export command in the Plots window 
# For quality figures, however, it's much better to write your figure to a file as a PDF, JPG, TIFF (often depending on journal specifications)
# Here we will write our figure as a PDF file
pdf("Frequency plot.pdf", width = 7, height = 6,pointsize = 10,bg = "white",useDingbats=F)
# Re-generate the plot with images
barpos<-barplot(freq, xlab="Foraging Strategy",ylab="Number of Bird Species",ylim=c(0,5500))
# Convert x and y coordinates of the original plot to those of the logos so they can be placed above the bars
# Use the grconvert function for this
logoX <- grconvertX(barpos, from="user", to="npc")
logoY <- grconvertY(freq, from="user", to="npc") + 0.05
# loop through the logos and place them above each bar
for (i in 1:length(logos))
{
  logoing_func(logos[[i]], x=logoX[i], y=logoY[i], size=0.1)
}
dev.off()

# Open the file
# It would need some work to make it look better, but that's a start!


################################################
# Histograms (Continuous Variables)
################################################

# Next we'll look at body mass, a numerical continous variable
# Let's start with a simple histogram
hist(dat$BodyMass.Value)
# That's not going to work!
# let's log those data
# There are two ways (the following approach is quickest and works if you are only exploring)
hist(log(dat$BodyMass.Value), xlab="Body mass (g)",main="Histogram of bird body masses")
# If you will be doing further analysis on the logged variable, then it's best to create a new "logged" version of the variable
dat2$logBodyMass<-log(dat2$BodyMass.Value)
# The default bin size looks ok to me, but let's play with a few different ones
hist(dat2$logBodyMass, breaks = 22)
hist(dat2$logBodyMass, breaks = 5)
# add some shading
hist(dat2$logBodyMass, col="grey")
# change the title
hist(dat2$logBodyMass, col="grey", main="Bird Body Mass")
# Make the x-axis label more informative
hist(dat2$logBodyMass, col="grey", main="Bird Body Mass", xlab="Log of Body Mass (kg)")

#############################################################
# Bar Plots (Associations between 2 categorical variables)
#############################################################

# Now let's start looking at some associations between variables
# First, let's see if foraging strategy is associated with birds being nocturnal
# The variable "Nocturnal" defines whether or not a bird species is noturnal (1) or is diurnal (0)
# We can use a contingency table to take an initial look at the relationship
contTable<-table(dat2$Nocturnal,dat2$Diet.5Cat)
# hard to see patterns, except that both nocturnal and diurnal species tend to feed on invertebrates
# let's look at proportions instead
propTable<-prop.table(contTable, 1) # row percentages
# note: prop.table(contTable, 2) gives column percentages
# We can make a grouped barplot
# Notes: beside = T places the groups beside each other, rather than stacked
#        Space controls the space between bars, with the first number controling space between treatments and the second between groups 
#         ylim controls the height of the y-axis
barpos<-barplot(propTable, beside = T, col = c("light grey","black"), space = c(0.2,1), xlab="Foraging Strategy", ylab="Proportion of Bird Species",ylim=c(0,0.75))
# add a legend
# Notes:
#       You can remove the box from around the legend using the bty command
#       cex controls the size of the legend, and can be used to control the size of other graphical parameters too
legend(9,0.6, c("Diurnal","Nocturnal"), fill=c("light grey","black"), bty="n",cex = 0.8)
# let's add sample sizes above the bars, just for fun
# barpos is the location of the labels (centre of each bar)
# propTable gives the height of the bars (plus 0.02 to make room for the text)
# contTable gives the counts in each category
text(barpos,propTable+0.02,contTable,cex=0.8)

#############################################################
# Scatter Plots (Associations between 2 continuous variables)
#############################################################

# Now let's look at the relationship between two numeric variables
# We'll use a database on Sharks downloaded from FishBase
# Is there a relationship between body weight and vulerability to extinction in sharks?
# We already know that body weight is a continuous numeric variable
# What kind of variable is vulnerability? What are the minimum and maximum values of vulnerability?
load("C:/Users/Katelyn/Downloads/Lab 2 Displaying Data 2021/chondTraitsWithTaxonomy.RData")
nrow(chondTraits)
# head(chondTraits)
# note that pch sets the symbols and col can be used to set the colour of the symbols
plot(log(chondTraits$Weight),(chondTraits$Vulnerability), pch=15, col="Dark Green")
# what if we want to differentiate between the two sub-classes of shark? (CHONDRICHTHYES and ELASMOBRANCHII--rays, )
# Let's make a scatter plot with different colours for the two classes
# first set up the plot. The pch "" will make the plot a blank plot
plot(log(chondTraits$Weight),(chondTraits$Vulnerability), pch="",xlab="Log Body Weight (kg)",ylab="Vulnerability Score")
# now we add points for two major orders of sharks
# CARCHARHINIFORMES--ground sharks and RAJIFORMES--rays, skates, sawfish
chondTraits$logWeight<-log(chondTraits$Weight)
points(chondTraits$logWeight[chondTraits$orderName=="CARCHARHINIFORMES"],chondTraits$Vulnerability[chondTraits$orderName=="CARCHARHINIFORMES"],pch=17,col="Orange",cex=2)
points(chondTraits$logWeight[chondTraits$orderName=="RAJIFORMES"],chondTraits$Vulnerability[chondTraits$orderName=="RAJIFORMES"],pch=15,col="Blue")
legend("bottomright", c("Caracharhiniformes","Rajiformes"), pch=c(17,15), col=c("Orange","Blue"),bty="n",cex = 1.5)

#############################################################
# Boxplots, strip plots, violin plots, and multiple histograms
# (Associations between one categorical and one continuous variable)
#############################################################

# Next, let's ask the question
# Are larger sharks more vulnerable to extinction?
# It's usually not a great idea to categorize continous variables (unless there are clear breakpoints and a good reason)
# we're going to do it as an example here
# First we'll cut the shark vulnerability variable into three categories (low, medium, and high) using quantiles
quantile<-quantile(chondTraits$Vulnerability,probs=c(0.33,0.66))
# now we'll make a new variable with low, medium, and high vulnerability
chondTraits$VulnerabilityCat<-mat.or.vec(nrow(chondTraits),1)
chondTraits$VulnerabilityCat[chondTraits$Vulnerability<=quantile[1]]<-"Low"
chondTraits$VulnerabilityCat[chondTraits$Vulnerability>quantile[1]&chondTraits$Vulnerability<=quantile[2]]<-"Medium"
chondTraits$VulnerabilityCat[chondTraits$Vulnerability>quantile[2]]<-"High"
chondTraits$VulnerabilityCat<-factor(chondTraits$VulnerabilityCat,levels=c("Low","Medium","High"))

# We can use a boxplot to show these data
plot(chondTraits$VulnerabilityCat,log(chondTraits$Weight),xlab="Vulnerability to Extinction",ylab="log Body Mass (kg)")
chondTraits$logWeight<-log(chondTraits$Weight)

# Or a strip plot with jitter
# we will use ggplot2 for this
ggplot(data = chondTraits, aes(x = VulnerabilityCat, y = logWeight)) +
  geom_jitter(color = "firebrick", shape = 1, size = 3, width = 0.15) +
  labs(x = "Vulnerability to Extinction", y = "log Body Mass (kg)") + 
  theme_classic()

# Violin plot
ggplot(data = chondTraits, aes(y = logWeight, x = VulnerabilityCat)) + 
  geom_violin(fill = "goldenrod1", col = "black") + 
  labs(x = "Vulnerability to Extinction", y = "log Body Mass (kg)") + 
  stat_summary(fun.y = mean,  geom = "point", color = "black") +
  theme_classic()

# Multiple histogram
ggplot(data = chondTraits, aes(x = logWeight)) + 
  geom_histogram(fill = "firebrick", binwidth = 1, col = "black", 
                 boundary = 0, closed = "left") +
  labs(x = "Vulnerability to Extinction", y = "log Body Mass (kg)") + 
  theme_classic() +
  facet_wrap( ~ VulnerabilityCat, ncol = 1, scales = "free_y", strip.position = "right")


###############################
# Exercises
###############################

# 1. Using the shark data, examine the variable "LongevityWild" using a histogram. 
#    It doesn't look very good! What can you do to make the data easier to see?
#    Next, examine whether there is a relationship between body weight and longevity (variable LongevityWild) in sharks using a scatter plot. 
#    What does the relationship look like from the data?
#    Try adding colour to the scatter plot and changing the axis labels

# 2. Now look at the "Dangerous" variable in the shark data using a frequency table. 
#    Note that the "Harmless category is spelt two different ways. How could you fix that error in the code so there is only one category for harmless?
#    Once you have fixed your table, how many sharks are harmless versus traumatogenic? 
#    Plot the frequencies of the Dangerous categories using a simple bar plot.

# 3. Next, look at the "Electrogenic" variable in the shark data set. How many sharks have electrosensing ability versus strongly discharging?
#    Similarly as above, you will need to correct for the two "electrosensing only" categories
#    Evaluate whether there appears to be an association between "Electrogenic" ability and depth (DepthRangeDeep) using an appropriate graph.

# 4. Does the relationship between litter size (litter_clutch_size) and body mass (body_mass_median) differ by taxa? 
#    Let's consider 2 mammalian orders: DIPROTODONTIA and DIDELPHIMORPHIA
#    We will use data from Cooke et al. 2019 (traitsWithTaxonomy.RData)
#    First, you will need to subset the data to these two orders
#    Let's treat litter size as a continuous variable in this case
#    Now use a scatter plot to show the relationship between the variables
#    Your scatter plot should show data from the two orders using different plot symbols (set using pch) and colours (set using col="")
#    Make sure you add a legend to your plot



###################
# Answers
###################

# 1. Using the shark data, examine the variable "LongevityWild" using a histogram. 

setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 2 Displaying Data/")
load("chondTraitsWithTaxonomy.RData")
hist(chondTraits$LongevityWild)

#    It doesn't look very good! What can you do to make the data easier to see?

chondTraits$lnLongevity<-log(chondTraits$LongevityWild)
hist(chondTraits$lnLongevity)

#    Next, examine whether there is a relationship between body weight and longevity (variable LongevityWild) in sharks using a scatter plot. 

chondTraits$logWeight<-log(chondTraits$Weight)
plot(log(chondTraits$Weight),log(chondTraits$LongevityWild))

#    What does the relationship look like from the data?
#    Try adding colour to the scatter plot and changing the axis labels

plot(log(chondTraits$Weight),log(chondTraits$LongevityWild), xlab="Log Body Weight",ylab="Log Longevity in the Wild",col="blue",pch=16)


# 2. Now look at the "Dangerous" variable in the shark data using a frequency table. 
#    Note that the "Harmless category is spelt two different ways. How could you fix that error in the code so there is only one category for harmless?
#    Once you have fixed your table, how many sharks are harmless versus traumatogenic? 
#    Plot the frequencies of the Dangerous categories using a simple bar plot.

table(chondTraits$Dangerous)
chondTraits$Dangerous[chondTraits$Dangerous == "Harmless"] <- "harmless"
table(chondTraits$Dangerous)
# OR
chondTraits$Dangerous2 <- recode(chondTraits$Dangerous, "Harmless" = "harmless" , "poisonous to eat" = "poisonous")

# how to change axis labels so that they all display properly
# Now let's look at a bar plot of feeding strategy
freq<-(sort(table(chondTraits$Dangerous), decreasing = T))
# xlab and ylab set the axis labels
x<-barplot(freq, xlab="",ylab="Number of Species",xaxt="n")
labs <- paste(names(freq))
text(cex=1, x=x-.25, y=-20, labs, xpd=TRUE, pos = 2, srt=45, adj=1)
#text((barpos[1,]+3.5),par("usr")[3]-ymax/30, labels = BinLabels10, srt = 45, pos = 2, xpd = TRUE,adj=1, cex=0.6)


# 3. Next, look at the "Electrogenic" variable in the shark data set. How many sharks have electrosensing ability versus strongly discharging?
#    Similarly as above, you will need to correct for the two "electrosensing only" categories
table(chondTraits$Electrogenic)
chondTraits$Electrogenic[chondTraits$Electrogenic == "Electrosensing only"] <- "electrosensing only"
table(chondTraits$Electrogenic)

# Evaluate whether there appears to be an association between "Electrogenic" ability and depth (DepthRangeDeep) using an appropriate graph.
# Need to remove NAs
chondTraits$Electrogenic<-factor(chondTraits$Electrogenic)
chondTraits <- chondTraits[!is.na(chondTraits$DepthRangeDeep), ]
chondTraits <- chondTraits[!is.na(chondTraits$Electrogenic), ]
# 2 methods for boxplot
boxplot(chondTraits$DepthRangeDeep~chondTraits$Electrogenic)
ggplot(chondTraits) + geom_boxplot(aes(x = Electrogenic, y = DepthRangeDeep))

# Could also use a Bean plot
ggplot(data = chondTraits, aes(y = DepthRangeDeep, x = Electrogenic)) + 
  geom_violin(fill = "goldenrod1", col = "black") + 
  labs(x = "Vulnerability to Extinction", y = "log Body Mass (kg)") + 
  stat_summary(fun.y = mean,  geom = "point", color = "black") +
  theme_classic()


# 4. Does the relationship between litter size (litter_clutch_size) and body mass (body_mass_median) differ by taxa? 
#    Let's consider 2 mammalian orders: DIPROTODONTIA and DIDELPHIMORPHIA
#    We will use data from Cooke et al. 2019 (traitsWithTaxonomy.RData)
#    First, you will need to subset the data to these two orders
#    Let's treat litter size as a continuous variable in this case
#    Now use a scatter plot to show the relationship between the variables
#    Your scatter plot should show data from the two orders using different plot symbols (set using pch) and colours (set using col="")
#    Make sure you add a legend to your plot

# Question 4
# Load the data
load("traitsWithTaxonomy.RData")
# Subset the data (2 methods)
# Method 1: use subset
traits<-subset(cooke2019,cooke2019$orderName=="DIPROTODONTIA" | cooke2019$orderName == "DIDELPHIMORPHIA")
# Method 2: use indexing
traits2<-cooke2019[cooke2019$orderName=="DIPROTODONTIA" | cooke2019$orderName == "DIDELPHIMORPHIA",]
# both give the same result
nrow(traits)
nrow(traits2)

# Plot the data
plot(log(traits$litter_clutch_size),log(traits$body_mass_median),pch="")
points(log(traits$litter_clutch_size[traits$orderName=="DIPROTODONTIA"]),log(traits$body_mass_median[traits$orderName=="DIPROTODONTIA"]),col="Blue",pch=8)
points(log(traits$litter_clutch_size[traits$orderName=="DIDELPHIMORPHIA"]),log(traits$body_mass_median[traits$orderName=="DIDELPHIMORPHIA"]),col="Orange",pch=15)
legend("topright", c("DIPROTODONTIA","DIDELPHIMORPHIA"), col=c("blue","orange"), pch=c(8,15),bty="n",cex = 1.5)


          
      
      
      
          
          
