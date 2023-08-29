
#############################
# NRES 776
# Lab 3: Summarizing Data
#############################

#############################
# Topics:
# Descriptive statistics on vectors (mean, sd, se, mode, median, interquartile range, se, 95% CI)
# Summarizing data by group (summary, dplyr)
# write data to .csv files
# make boxplots and error bar plots
# Write a csv file
# Dealing with missing values
#############################

#############################
# Relevant R Datacamp Modules
# Note that R Datacamp modules are supplemental to the lab
# Completing Datacamp modules is not necessary or required for the course, but
# depending on your interests and familiarity with R, the modules may provide additional practice and/or learning opportunities
# Data Manipulation with dplyr
# Working with data in the tidyverse
#############################

####################################
# Set working directory
###################################
# Start by setting your working directory
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 3 Summarising Data/")


#####################################
# Install and Load Packages
#####################################

# install.packages("dplyr") 
library(dplyr)
# install.packages("ggplot2")
library("ggplot2")

# Packages also contain help files for all of their functions
# main dplyr help
help(dplyr)
# OR
?dplyr
# You can also look up help files for specific functions within packages
# The select function is a function in R
?select

######################################################################################################
# Descriptive statistics on a single vector (mean, sd, variance, median, range, min, max, interquartile range)
#######################################################################################################

# Load the data set iris
data(iris)  # load the built-in iris data set
# To do: what are the first commands your should run when you load data? (recall last week's lab)
# Try the following commands:
ls(iris)
summary(iris)
# What do these commands do?
# What are the levels of the Species variable in the iris dataset?

# Now let's do some summary statistics
# Mean of a single variable
mean(iris$Sepal.Length)
# Variance
var(iris$Sepal.Length)
# Standard deviation of a single variable
sd(iris$Sepal.Length)
# Median
median(iris$Sepal.Length)
# Interquartile range
IQR(iris$Sepal.Length)
# To do: double check that the IQR makes sense using the quantile() function on iris$Sepal.length. 
# If it makes sense, then the IQR should be the value for the 75% quantile minus the 25% quantile.
# range
range(iris$Sepal.Length)
# minimum value
min(iris$Sepal.Length)
# maximum value
max(iris$Sepal.Length)

####################################################
# Use functions to calculate standard error and mode
####################################################

# Getting the standard error and the mode is a little trickier. We're going to write functions!
# The generic structure for creating a new function is...
# functionName <- function(arg1, arg2..){
#	function calculations
# }
# Create a function for standard error
se <- function(x) sqrt(var(x)/length(x))
se(iris$Sepal.Length)

# create a function for getting the mode
getmode <- function(v) {
  uniqv <- unique(v) #get unique values of vector v
  nRepeat <- tabulate(match(v, uniqv)) # make a new vector with the frequencies of each unique value in the vector
  uniqv[which(nRepeat == max(nRepeat))] # return the values of vector v that occur with the highest frequency (may be more than one)
}

# now we can apply our new function to get the mode of Sepal.Length
getmode(iris$Sepal.Length)

# To do: try creating a function that calculates the square root of the median

###################################################
# Confidence Intervals of the Mean
###################################################

# CI = mean plus or minus z(alpha/2)*standard error
# If using the Z distribution, you can get the z value for a specified level of alpha using qnorm()
# For a 95% confidence interval:
qnorm(0.975)
qnorm(0.025)
# you can then calculate the margin of error as:
error <- qnorm(0.975)*se(iris$Sepal.Length)
lower95CI_z<-mean(iris$Sepal.Length)-error
upper95CI_z<-mean(iris$Sepal.Length)+error

# you could also use a t-distribution to calculate 95% confidence intervals
n<-length(iris$Sepal.Length)
errort <- qt(0.975,df=n-1)*se(iris$Sepal.Length)
lower95CI_t<-mean(iris$Sepal.Length)-errort
upper95CI_t<-mean(iris$Sepal.Length)+errort

# How similar are the confidence intervals calculated using the t and z distributions?
# How would you calculate a 90% confidence interval?

#################################################
# Summary statistics by group
#################################################

# Usually we are interested in summarizing data for multiple variables and/or by group rather than for a single variable
# There are many different ways to do that in R

# summary() gives the mean,median,25th and 75th quartiles,min,max for all variables in the data
summary(iris)

# sapply is a function that gives various summaries for variables in a dataframe
# possible functions include mean, sd, var, min, max, median, range, and quantile
# the [,1:4] specifies to use all rows but only columns 1-4 (the 5th column is categorical so it throws an error if included in the command)
# na.rm = TRUE excludes missing values
sapply(iris[,1:4], mean, na.rm=TRUE) # column means for all rows (n=150)
sapply(iris[1:50,1:2], mean, na.rm=TRUE) # column means for the first 50 rows and first two columns in the dataframe

# the dplyr package is a powerful tool for many types of data manipulation, including summaries
# it uses what is called pipes, i.e., the %>% to tell R to do multiple steps
# Here we will calculate the mean sepal length for the different species
iris %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Sepal.Length),
            sdev = sd(Sepal.Length))

# How would you add petal length to the above summary?
# How would you add variance as well as sdev?

# You can also assign dplyr objects, such as to a table
myTable<-iris %>% 
  group_by(Species) %>% 
  summarize(mean = mean(Sepal.Length),
            sdev = sd(Sepal.Length))

# Write a csv file with the summaries
write.csv(myTable,"irisSummaries.csv")
# Open the csv and take a look

######################################################################
# Make barplots and error bar plots to show the iris data by species
######################################################################

# Here is a simple boxplot using base R
boxplot(iris$Sepal.Length~iris$Species, xlab="Species",ylab="Sepal Length",main="Iris Data")

# Here is an error bar plot using ggplot2 showing the scatter of the data relative to mean and standard error
ggplot(iris, aes(Species, Sepal.Length)) +
  geom_jitter(color = "firebrick", size = 3, width = 0.1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.1, position=position_nudge(x = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", 
               size = 3, position=position_nudge(x = 0.2)) +
  labs(x = "Species", y = "Sepal Length") + 
  theme_classic()

# There different ways of exporting figures from R
# For example, you can use the Export command in the Plots window 
# For quality figures, however, it's much better to write your figure to a file as a PDF, JPG, TIFF (often depending on journal specifications)
# Here we will write our figure as a PDF file
pdf("Error bar plot.pdf", width = 5, height = 5,pointsize = 10,bg = "white",useDingbats=F)
ggplot(iris, aes(Species, Sepal.Length)) +
  geom_jitter(color = "firebrick", size = 3, width = 0.1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               width = 0.1, position=position_nudge(x = 0.2)) +
  stat_summary(fun.y = mean, geom = "point", 
               size = 3, position=position_nudge(x = 0.2)) +
  labs(x = "Species", y = "Sepal Length") + 
  theme_classic()
dev.off()

# Open the file
# It would need some work to make it look better, but that's a start!

#################################################
# Dealing with missing values
#################################################

# NAs can be a pain in almost any dataset
# Dealing with NAs in R can be complicated (and frustrating)
# Here we will look at how you can deal with missing values if you are using a function
# Let's randomly remove some values from the Sepal.Length variable in the iris data set
iris$Sepal.Length2<-iris$Sepal.Length
n<-3
inds<-round(runif(n,min=1,max=length(iris$Sepal.Length2)), digits=0)
iris$Sepal.Length2[inds]<-NA
# Now if you try taking the mean of Sepal.Length2, the command will return NA
mean(iris$Sepal.Length2)
# To solve this problem, add na.rm = TRUE
mean(iris$Sepal.Length2, na.rm=TRUE)

############################################
# Exercises
############################################

# 1. The file "caffeine.csv" contains data on the amount of caffeine in a 16 oz. cup of coffee obtained from various vendors. 
# For context, doses of caffeine over 25 mg are enough to increase anxiety in some people, 
# and doses over 300 to 360 mg are enough to significantly increase heart rate in most people. 
# A can of Red Bull contains 80mg of caffeine.
# a) What is the mean amount of caffeine in 16 oz. coffees?
# b) What is the 95% confidence interval for the mean?
# c) Plot the frequency distribution of caffeine levels for these data in a histogram. 
# Is the amount of caffeine in a cup of coffee relatively consistent from one vendor to another? 
# What is the standard deviation of caffeine level? What is the coefficient of variation?
# d) The file "caffeineStarbucks.csv" has data on six 16 oz. cups of Breakfast Blend coffee sampled on six different days from a Starbucks location. 
# Calculate the mean (and the 95% confidence interval for the mean) for these data. 
# Compare these results to the data taken on the broader sample of vendors in the first file. Describe the difference.

# 2. A confidence interval is a range of values that are likely to contain the true value of a parameter. 
# Consider the "caffeine.csv" data again.
# a) Calculate the 99% confidence interval for the mean caffeine level.
# b) Compare this 99% confidence interval to the 95% confidence interval you calculate in question 1b. 
#    Which confidence interval is wider (i.e., spans a broader range)? Why should this one be wider?
# c) Let's compare the quantiles of the distribution of caffeine to this confidence interval. 
#    Approximately 95% of the data values should fall between the 2.5% and 97.5% quantiles of the distribution of caffeine. 
#    We can use R to calculate the 2.5% and 97.5% quantiles with a command like the following.
#    quantile(datavector, c(0.025, 0.975), na.rm =TRUE)
#   (Replace "datavector" with the name of the vector of your caffeine data.)
#   Are these the same as the boundaries of the 95% confidence interval? 
#   If not, why not? Which should bound a smaller region, the quantile or the confidence interval of the mean?

# 3. Use the data on countries of the world, in "countries.csv".
#    Plot the distributions for ecological_footprint_2000, cell_phone_subscriptions_per_100_people_2012, and life_expectancy_at_birth_female.
#    a) For each variable, plot a histogram of the distribution. Is the variable skewed? If so, in which direction?
#    b) For each variable, calculate the mean and median. Are they similar? 
#       Match the difference in mean and median to the direction of skew on the histogram. Do you see a pattern?



#########################
# Answers
#########################

# From demonstration Code
# Create a function that calculates the square root of the median
sqrtMed <- function(x) sqrt(median(x))

# Change dplyr commands
iris %>% 
  group_by(Species) %>% 
  summarize(meanSL = mean(Sepal.Length),meanPL = mean(Petal.Length),
            sdevSL = sd(Sepal.Length), sdevPL = sd(Petal.Length), varSL = var(Sepal.Length),varPL = var(Petal.Length))

iris %>% 
  group_by(Species) %>% 
  summarize(meanSL = mean(Sepal.Length),meanPL = mean(Petal.Length),
            sdevSL = sd(Sepal.Length), sdevPL = sd(Petal.Length), varSL = var(Sepal.Length), varPL = var(Petal.Length))

# Exercises
# 1. The file "caffeine.csv" contains data on the amount of caffeine in a 16 oz. cup of coffee obtained from various vendors. 
# For context, doses of caffeine over 25 mg are enough to increase anxiety in some people, 
# and doses over 300 to 360 mg are enough to significantly increase heart rate in most people. 
# A can of Red Bull contains 80mg of caffeine.
# a) What is the mean amount of caffeine in 16 oz. coffees?

setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 3 Summarising Data/")
caffeine<-read.csv("caffeine.csv")
# look at the data
fix(caffeine)
mean(caffeine$caffeine_mg_16oz)
# [1] 188.0643

# b) What is the 95% confidence interval for the mean?

# Create a function for standard error
se <- function(x) sqrt(var(x)/length(x))
# 95% confidence interval
n<-length(caffeine$caffeine_mg_16oz)
errort <- qt(0.975,df=n-1)*se(caffeine$caffeine_mg_16oz)
lower95CI_t<-mean(caffeine$caffeine_mg_16oz)-errort
upper95CI_t<-mean(caffeine$caffeine_mg_16oz)+errort
lower95CI_t
upper95CI_t

# c) Plot the frequency distribution of caffeine levels for these data in a histogram. 

hist(caffeine$caffeine_mg_16oz)

# Is the amount of caffeine in a cup of coffee relatively consistent from one vendor to another? 
# What is the standard deviation of caffeine level? What is the coefficient of variation?

sd(caffeine$caffeine_mg_16oz)
cv<-sd(caffeine$caffeine_mg_16oz)/mean(caffeine$caffeine_mg_16oz)*100
cv

# d) The file "caffeineStarbucks.csv" has data on six 16 oz. cups of Breakfast Blend coffee sampled on six different days from a Starbucks location. 
# Calculate the mean (and the 95% confidence interval for the mean) for these data. 
# Compare these results to the data taken on the broader sample of vendors in the first file. Describe the difference.

starbucks<-read.csv("caffeineStarbucks.csv")
mean(starbucks$caffeine_mg_16oz)
# 95% confidence interval
n<-length(caffeine$caffeine_mg_16oz)
errort <- qt(0.975,df=n-1)*se(starbucks$caffeine_mg_16oz)
sb_lower95CI_t<-mean(starbucks$caffeine_mg_16oz)-errort
sb_upper95CI_t<-mean(starbucks$caffeine_mg_16oz)+errort
sb_lower95CI_t
sb_upper95CI_t

# mean 371, 95% CI is 261-483 mg of caffeine in starbucks dataset (n=6)
# mean 188, 95% CI is 78-300 mg of caffeine in brader sample of vendors (n=14)
# Starbucks coffees have more caffeine on average compared with other vendors
# CI is similarly wide for coffees made at starbucks compared with coffees made at different vendors


# 2. A confidence interval is a range of values that are likely to contain the true value of a parameter. 
# Consider the "caffeine.csv" data again.
# a) Calculate the 99% confidence interval for the mean caffeine level.
# b) Compare this 99% confidence interval to the 95% confidence interval you calculate in question 1b. 
#    Which confidence interval is wider (i.e., spans a broader range)? Why should this one be wider?

# 99% confidence interval
n<-length(caffeine$caffeine_mg_16oz)
errort <- qt(0.995,df=n-1)*se(caffeine$caffeine_mg_16oz)
lower99CI_t<-mean(caffeine$caffeine_mg_16oz)-errort
upper99CI_t<-mean(caffeine$caffeine_mg_16oz)+errort

lower95CI_t
#[1] 167.5237
lower99CI_t
#[1] 159.4238
upper95CI_t
#[1] 208.6049
upper99CI_t
#[1] 216.7047

# 99% CI wider because it represents the interval within which 99% of sample means will fall
# vs only 95% of sample means fall within the 95% confidence interval (more of the means will fall outside this interval, so it is narrower)

# c) Let's compare the quantiles of the distribution of caffeine to this confidence interval. 
#    Approximately 95% of the data values should fall between the 2.5% and 97.5% quantiles of the distribution of caffeine. 
#    We can use R to calculate the 2.5% and 97.5% quantiles with a command like the following.
#    quantile(datavector, c(0.025, 0.975), na.rm =TRUE)
#   (Replace "datavector" with the name of the vector of your caffeine data.)
#   Are these the same as the boundaries of the 95% confidence interval? 
#   If not, why not? Which should bound a smaller region, the quantile or the confidence interval of the mean?

quantile(caffeine$caffeine_mg_16oz, c(0.025, 0.975), na.rm =TRUE)
#2.5%   97.5% 
#  144.765 254.685 
#95% CIs
# 167.5, 216.7
# 95% CI is narrower than the quantiles. It represents precision of the mean
# Quantiles represent the percentage of observations less than or equal to it, representing spread in the original data.

# 3. Use the data on countries of the world, in "countries.csv".
#    Plot the distributions for ecological_footprint_2000, cell_phone_subscriptions_per_100_people_2012, and life_expectancy_at_birth_female.
#    a) For each variable, plot a histogram of the distribution. Is the variable skewed? If so, in which direction?
#    b) For each variable, calculate the mean and median. Are they similar? 
#       Match the difference in mean and median to the direction of skew on the histogram. Do you see a pattern?
countries<-read.csv("countries.csv")
# ecological footprint
hist(countries$ecological_footprint_2000) # right skewed
mean<-mean(countries$ecological_footprint_2000[!is.na(countries$ecological_footprint_2000)])
median<-median(countries$ecological_footprint_2000[!is.na(countries$ecological_footprint_2000)])
abline(v=mean,col=2,lwd=3)
abline(v=median,col=1,lwd=3)
legend("topright",c("mean","median"),col=c(2,1),lty=1,lwd=3,bty='n')
# distribution skewed right; median lower than the mean

# cell phone subscriptions
hist(countries$cell_phone_subscriptions_per_100_people_2012)
mean<-mean(countries$cell_phone_subscriptions_per_100_people_2012[!is.na(countries$cell_phone_subscriptions_per_100_people_2012)])
median<-median(countries$cell_phone_subscriptions_per_100_people_2012[!is.na(countries$cell_phone_subscriptions_per_100_people_2012)])
abline(v=mean,col=2,lwd=3)
abline(v=median,col=1,lwd=3)
legend("topright",c("mean","median"),col=c(2,1),lty=1,lwd=3,bty='n')
# distribution only slightly skewed left; median slightly higher than the mean

# Life expectancy at birth
hist(countries$life_expectancy_at_birth_female)
mean<-mean(countries$life_expectancy_at_birth_female[!is.na(countries$life_expectancy_at_birth_female)])
median<-median(countries$life_expectancy_at_birth_female[!is.na(countries$life_expectancy_at_birth_female)])
abline(v=mean,col=2,lwd=3)
abline(v=median,col=1,lwd=3)
legend("topright",c("mean","median"),col=c(2,1),lty=1,lwd=3,bty='n')
# distribution more strongly skewed left than cell phone data; mean relatively lower than median



