
#############################
# NRES 776
# Lab 4: Comparing two means
#############################


#############################
# Topics
#############################
# Comparing two means (parametric)
#   Two-sample comparison of means
#   Welch's approximate t-test
#   Paired t-test
#   Levene's test for equal variances
# Non-parametric methods for two-sample data
#   Sign test
#   Mann-Whitney U Test
#   Permutation test
#############################


###############################################
# Materials
###############################################
# For today's lab we will be using materials from our textbook "The Analysis of Biological Data" by Whitlock and Schluter (2015)
# The first part of the Lab can be found online here:
# https://whitlockschluter3e.zoology.ubc.ca/RLabs/R_tutorial_Comparing_means_of_2_groups.html#questions
# Note that the online lab has some details that you may find useful for review
# In addition, The "Activities" allows you to examine the robustness of the t-test to violations of the assumptions
# We will also be drawing on source code from Chapter 13 of Whitlock and Schluter
# https://whitlockschluter3e.zoology.ubc.ca/RExamples/Rcode_Chapter_13.html
###############################################


#############################
# Relevant R Datacamp Modules
#############################
# Hypothesis Testing in R: This module contains relevant information (e.g., t-tests) that we will be covering in lecture and lab. 
#     Note that this module also gets into tests we don't focus on in this course, but that would be familiar from other stats courses
#     Ex: chi-square tests. The module also introduces ANOVA, which we'll look at next week.
# Introduction to Statistics in R: This module covers the basics of probability and p-values, which are fundamental concepts in stats
#     It provides a good review of earlier stats courses! The module also goes into topics (e.g., correlation) that we'll be looking at in subsequent labs
#############################


####################################
# Set working directory
####################################
# Start by setting your working directory
setwd("C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 4 Comparing two means/")
####################################


###############################################
# Load required packages
###############################################
library(dplyr)
library(ggplot2)
# Note that you may have to install the car package before you can load it
install.packages("car")
library(car)
###############################################


################################################
# Two-sample t-test
################################################

# We will work with data on people who did and didn't survive the sinking of the Titanic
# We'll use the data to ask "Is there a difference in age between those who survived and those who didn't?"

# Load the titanic data
# Note that the titanic data are in 'long format'--there is one colum of age for both survival categories ('yes' and 'no')
titanicData<-read.csv("titanic.csv")

# Make sure the "survive" variable is a factor
titanicData$survive<-factor(titanicData$survive)
is.factor(titanicData$survive)

# First, plot the data to see if there are visible differences in age of survivors
# Strip charts
ggplot(titanicData, aes(x = survive, y = age)) + geom_jitter(position = position_jitter(0.05)) + 
  ggtitle("Age differences in Titanic survival") + theme_minimal()
# Change font of all text elements (including title)
ggplot(titanicData, aes(x = survive, y = age)) + geom_jitter(position = position_jitter(0.05)) + 
  ggtitle("Age differences in Titanic survival") + theme_minimal() + theme(text = element_text(size = 20))
# Change font of axis text only
ggplot(titanicData, aes(x = survive, y = age)) + geom_jitter(position = position_jitter(0.05)) + 
  ggtitle("Age differences in Titanic survival") + theme_minimal() + theme(axis.text = element_text(size = 20))
# other options: axis.text.x changes only the size of the x axis (no in our example)
#                axis.title changes the axis title (survive and age in our example)
#                axis.title.x changes the axis title on the x axis only (survive in our case)
#                plot.title changes the size of the plot title only (Age differences in Titanic survival)

# Multiple histograms
ggplot(titanicData, aes(x = survive, y = age)) + geom_jitter(position = position_jitter(0.05)) + theme_minimal()
ggplot(titanicData, aes(x = age)) +   
  geom_histogram() + 
  facet_wrap(~ survive, ncol = 1)

# Violin plots
ggplot(titanicData, aes(x=survive, y=age, fill = survive)) + 
  geom_violin() +
  xlab("Survival") + ylab("Age") + 
  theme_classic()+scale_fill_manual(values=c("#FFB531","#BC211A"))+ 
  stat_summary(fun.y=mean,  geom="point", color="black")+ 
  theme(legend.position="none")+ 
  theme(aspect.ratio=1)

# run a two-sample t-test with equal variances
t.test(age ~ survive, data = titanicData, var.equal = TRUE)

# Welch's approximate t-test
t.test(age ~ survive, data = titanicData, var.equal = FALSE)

# Check whether variances are equal
leveneTest(age ~ survive, titanicData, center=mean)


################################################
# Paired t-test
################################################

# We will look at the blackbird data that we discussed in class
# Is there a difference in mean log antibody production before and after the testosterone implant?
# Note that the data are in 'wide' format (separate columns for antibody production before and after the testosterone implant)
blackbird <- read.csv("chap12e2BlackbirdTestosterone.csv")


t.test(blackbird$logAfterImplant, blackbird$logBeforeImplant, paired = TRUE)
# Result gives mean difference of 0.56 (mean of after-mean of before)
mean(blackbird$logAfterImplant)-mean(blackbird$logBeforeImplant)
# The mean difference becomes negative if you switch the order of the variables in the t-test
t.test(blackbird$logBeforeImplant, blackbird$logAfterImplant, paired = TRUE)


################################################
# Sign Test
################################################

# For this example, we will look at the sexual conflict example we discussed last class
# In this example, the authors were comparing numbers of species of insect taxa with and without multiple matings
# They want to know if sexual conflict drives speciation
# The data are paired
# n = 25 

# Read and inspect data
conflict <- read.csv("chap13e4SexualConflict.csv", stringsAsFactors = FALSE)
head(conflict)

# Create a histogram
hist(conflict$difference, right = FALSE, col = "firebrick", breaks = 50, xlab = "Difference in species number", las = 1)

# The outlier is a major problem so it's clear these data can't be tested with a t-test, but let's look at a Q-Q plot anyway
qqnorm(conflict$difference, col = "firebrick", pch = 16, las = 1,
       datax = TRUE, xlab = "Difference")
qqline(conflict$difference, datax = TRUE, lwd = 2)

# We could also use a Shapiro-Wilk test to evaluate whether the differences are normally distributed
shapiro.test(conflict$difference)

# Clearly the data are non-normal so we will use a sign test
# The first step in the sign test is to assign each difference as being above or below zero
conflictZero <- cut(conflict$difference, breaks = c(-Inf, 0, 1, Inf), 
                    labels = c("below","equal","above"), right = FALSE)
# Then count the number of negatives and positives in each group
table(conflictZero)

# Now we can perform a binomial test using the frequency data
# In a sample of 25, what are the chances that 7 or fewer differences would fall below the median 
binom.test(7, n = 25, p = 0.5)


#################################################
# Mann-Whitney U Test
#################################################
# Now we'll look at the Cricket cannibalism example that we discussed in class
# Here we compare the time to mating (in hours) of starved and fed female sagebrush crickets

# read and inspect the data
cannibalism <- read.csv("chap13e5SagebrushCrickets.csv")
head(cannibalism)

# Generate histograms of the two groups
# First re-order the levels of the feeding Status variable so that the starved group comes before fed
# The default for factors is for levels to be ordered alphabetically
cannibalism$feedingStatus <- factor(cannibalism$feedingStatus, levels = c("starved", "fed"))

# Look at the data to see whether we could use a parametric test
ggplot(cannibalism, aes(x = timeToMating)) + 
  geom_histogram(fill = "firebrick", col = "black", binwidth = 20, 
                 boundary = 0, closed = "left") +
  facet_wrap( ~ feedingStatus, ncol = 1, scales = "free_y") +
  labs(x = "Time to mating (hours)", y = "Frequency") + 
  theme_classic()

# Now perform a Wilcoxon rank-sum test (aka Mann-Whitney U-test)
wilcox.test(timeToMating ~ feedingStatus, data = cannibalism)


#################################################
# Permutation test
#################################################
# An alternative to a Mann-Whitney U test is a permutation test
# Here we will use a permutation test to test the differences in mean times to mating

# First calculate the observed difference between means (starved minus fed)
cricketMeans <- summarize(group_by(cannibalism, feedingStatus), timeToMating = mean(timeToMating, na.rm = TRUE))
cricketMeans <- as.data.frame(cricketMeans)
cricketMeans
diffMeans <- cricketMeans$timeToMating[1] - cricketMeans$timeToMating[2]
diffMeans

# Set the number of permutations
nPerm <- 10000

# Create a loop to permute the data many times (the number of times determined by nperm). 
# In the following loop, i is a counter that climbs from 1 to nPerm by 1. 
# In each iteration, the permuted difference is saved in the object permResult.
permResult <- vector() # initializes
for(i in 1:nPerm){
  # step 1: permute the times to mating
  cannibalism$permSample <- sample(cannibalism$timeToMating, replace = FALSE)
  # step 2: calculate difference betweeen means
  permSampleMean <- as.data.frame(summarize(group_by(cannibalism, feedingStatus), 
                                            permMean = mean(permSample, na.rm = TRUE)))
  permResult[i] <- permSampleMean$permMean[1] - permSampleMean$permMean[2]
}

# look at a histogram of the results from the permutations
hist(permResult, right=F, breaks=100)

# Alternatively, you could use ggplot to achieve the same thing
ggplot(data.frame(permResult), aes(permResult)) + 
  geom_histogram(fill = "white", col = "black", binwidth = 1, 
                 boundary = 0, closed = "left") + 
  labs(x = "Permuted difference in treatment means (hours)", y = "Frequency") + 
  theme_classic()

# Now use the null distribution to calculate an approximate p-value
# We do this by determining the proportion of permuted means that fall below the observed difference in means (-18.25734)

# Number of permuted means that fall below diffMeans
sum(as.numeric(permResult <= diffMeans))

# Proportion of permuted means that fall below diffMeans
sum(as.numeric(permResult <= diffMeans)) / nPerm

# Multiply by two to get the two-tailed p-value
2 * ( sum(as.numeric(permResult <= diffMeans)) / nPerm )


#################################################
# Practice Exercises
#################################################

# 1. In 1898, Hermon Bumpus collected data on natural selection. Immediately following a bad winter storm, 136 English house 
#    sparrows were collected and brought indoors. Of these, 72 subsequently recovered, but 64 died. Bumpus made several measurements
#    on all of the birds, and he was able to demonstrate strong natural selection on some of the traits as a result of this storm. 
#    Natural selection has occurred if traits are different between the survivors and non-survivors.
#    Bumpus published all of his data, and they are given in the file "bumpus.csv." 
#    Let's examine whether there was natural selection in body weight from this storm event, by comparing the weights of the birds 
#    that survived to those that died.

# a. Plot a graph with multiple histograms for body weight (called "weight_g" in the bumpus.csv data set), 
#    comparing the surviving and nonsurviving groups (given in the variable "survival"). 
#    Do the distributions look approximately normal? Do the variances look approximately equal?
# b. Use t.test() to do a Welch's t-test to look for a difference in mean body weight between the surviving and dying birds.
# c. What is the 95% confidence interval for the difference in mean weight of surviving and dying birds?
# d. Use a Levene's test to ask whether the surviving and dying birds had the same variance in weight.
# 
# 
# 2. We will use data about ecological footprint in the data set "countries.csv". The ecological footprint of a country is 
#    a measure of the average amount of resources consumed by a person in that country (as measured by the amount of land 
#    required to produce those resources and to handle the average person's waste). For many countries in the data set, 
#    information is available on the ecological footprint in the year 2000 as well as in 2012. 
#    Use a paired t-test to ask whether the ecological footprint has changed over that time period.

# a. Plot a histogram showing the difference in ecological footprint between 2012 and 2000. 
#    (Note, you will probably need to create a new vector containing the differences, and use hist() to make the graph.)
# b. Use t.test() to do a paired t-test to determine whether the ecological footprint changed significantly between 2000 and 2012.
# c. Interpret your result. Is there evidence that the ecological footprint changed over that time? 
#    If so did it increase or decrease?
#   
# 3. A common belief is that shaving causes hair to grow back faster or coarser than it was before. 
#    Is this true? Lynfield and McWilliams (1970) did a small experiment to test for an effect of shaving. 
#    Five men shaved one leg weekly for several months while leaving the other leg unshaved. 
#    (The data from the shaved leg has the word "test" in the variable name; the data from the unshaved leg is labeled with "control.")
#    At the end of the experiment, the researchers measured the difference in leg hair width and the hair growth rate on each 
#    of the legs. These data are given in "leg shaving.csv".
#
# a. Perform a suitable hypothesis test to determine whether shaving affects hair thickness.
# b. How big is the difference? Find the 95% confidence interval for the difference between shaved and unshaved legs 
#    for hair thickness.

# 4.The nocturnal dung beetle (Scarabaeus satyrus) is known to use the Milky Way for orientation.
#   After sculpting a fresh dung ball, an individual beetle rolls it away from the dung pile to be burried some distance away.
#   On moonless nights, the beetles use the Milky Way as a cue to maintain straight-line travel away from the dung pile.
#   Foster et al. (2017) simulated patterns of illumination in experimental arenas to investigate which features of the Milky Way were used as orientation cues.
#   Indivdidual beetles were randomly assigned to one of two treatments, each with ethe same overall level of illumination.
#   In the 'milky way' treatment, the beetles were exposed to artificial illumination in the pattern of the Milky Way
#   In a control treatment, the light in the arena was of equal intensity in all directions.
#   The researchers recorded each beetle's 'orientation error' 
#   (the difference in a beetle's two directions away from a dung pile, relative to stimulus orientation, when tested twice)
#   Stimulus orientation was changed between the two trials for each beetle

# a. Read and inspect the data (mlky way_complete.csv). 
#    Make Q-Q plots of the Equal-intensity and Milky Way treatments. Do these data meet the assumption of normality for a parametric test?
# b. Use a Mann-Whitney U-test to test whether the distribution of orientation errors differs between beetle groups under simulated Milky Way
#    and equal-intesntiy illumination conditions.Make sure to state your hypotheses, statistical conclusion, and biological interpretation.

# 5. The pseudoscorpion Cordylochernes scorpioides lives in tropical forests, where it rides on the backs of harlequin beetles to reach
#    decauing fig trees in which they live. Females of the species mate with multiple males over their short lifetimes, which is puzzling
#    because mating just once provides all the sperm she needs to fertilize her eggs. A possible advantage is that by mating multiple times,
#    a female increases the chances of mating with at least one sperm-compatible male, if incompatibilities are present in the population.
#    To investiate, Newcomer et al. (1999) recorded the number of successful broods by female pseudoscorpions randomly assigned to one of two treatments.
#    Females were each mated to two different males (DM treatment), or they were each mated twice to the same male (SM). This design
#    provided the same total amount of sperm to females in both treatments, but DM females received genetically more diverse sperm than
#    did SM females. The number of successful broods of offspring for each female is listed in the data file:
#    (https://whitlockschluter3e.zoology.ubc.ca/Data/chapter13/chap19q27Pseudoscorpions.csv). 

# a. Read and inspect the data. Are the data normally distributed in the two groups?
# b. Conduct a permutation test for the null hypothesis of no difference in the mean number of broods between treatments.State your hypotheses, 
#    statistical conclusion, and biological interpretation.
# c. Repeat the permutation test using the median instead of the means.



#######################################
# Answers
#######################################


# 1.  Bumpus Data
bumpus <- read.csv("bumpus.csv")

# a. Plot a graph with multiple histograms for body weight (called "weight_g" in the bumpus.csv data set), 
#    comparing the surviving and nonsurviving groups (given in the variable "survival").
#    Do the distributions look approximately normal? Do the variances look approximately equal?
ggplot(bumpus, aes(x = survival, y = weight_g)) + geom_jitter(position = position_jitter(0.05)) + theme_minimal()
ggplot(bumpus, aes(x = weight_g)) +   
  geom_histogram() + 
  facet_wrap(~ survival, ncol = 1)

# b. Use t.test() to do a Welch's t-test to look for a difference in mean body weight between the surviving and dying birds.

t.test(weight_g ~ survival, data = bumpus, var.equal = FALSE)

# c. What is the 95% confidence interval for the difference in mean weight of surviving and dying birds?

# 0.1351376 1.1339597

# d. Use a Levene's test to ask whether the surviving and dying birds had the same variance in weight.
leveneTest(weight_g ~ survival, bumpus, center=mean)

# 2. Ecological footprint data
footprint <- read.csv("countries.csv")

# a. Plot a histogram showing the difference in ecological footprint between 2012 and 2000. 
#    (Note, you will probably need to create a new vector containing the differences, and use hist() to make the graph.)

# Create a new variable with the difference in human fooprint between 2020 and 2012
footprint$diff<-footprint$ecological_footprint_2012-footprint$ecological_footprint_2000
# plot a histogram
hist(footprint$diff)

# b. Use t.test() to do a paired t-test to determine whether the ecological footprint changed significantly between 2000 and 2012.
# paired t-test on the difference
t.test(footprint$ecological_footprint_2012, footprint$ecological_footprint_2000, paired = TRUE)  

# c. Interpret your result. Is there evidence that the ecological footprint changed over that time? 
#    If so did it increase or decrease?
# decreased
mean(footprint$diff,na.rm=T)

# 3. Leg shave
shave<-read.csv("C:/Users/Heather/UNBC/NRES 776 2020/Labs/ABDLabs/ABDLabs/DataForLabs/leg shaving.csv")

# a. Perform a suitable hypothesis test to determine whether shaving affects hair thickness.
t.test(shave$hair_width_change_test,shave$hair_growth_change_control,paired=TRUE)

# b. How big is the difference? Find the 95% confidence interval for the difference between shaved and unshaved legs 
#    for hair thickness.
# # 95 percent confidence interval:
# -28.15082  12.94282
# sample estimates:
#   mean of the differences 
# -7.604

# 4. Dung Beetles
# a. Read and inspect the data (mlky way_complete.csv). 
#    Make Q-Q plots of the Equal-intensity and Milky Way treatments. Do these data meet the assumption of normality for a parametric test?

milkyWaybeetles <- read.csv("chap13q32milkyWayBeetles20.csv", stringsAsFactors = FALSE)
# Equal intensity treatment
qqnorm(milkyWaybeetles$OrientationError[milkyWaybeetles$Condition=="Equal_Intensity"], col = "firebrick", pch = 16, las = 1,
       datax = TRUE, xlab = "OrientationError")
qqline(milkyWaybeetles$OrientationError[milkyWaybeetles$Condition=="Equal_Intensity"], datax = TRUE, lwd = 2)
# Milky way treatment
qqnorm(milkyWaybeetles$OrientationError[milkyWaybeetles$Condition=="Milky_Way"], col = "firebrick", pch = 16, las = 1,
       datax = TRUE, xlab = "OrientationError")
qqline(milkyWaybeetles$OrientationError[milkyWaybeetles$Condition=="Milky_Way"], datax = TRUE, lwd = 2)

# (a) The deviation from normal is greatest for the Milky Way orientation group, whose
# quantiles depart most strongly from the straight line expected under the normal
# distribution. 

# b. Use a Mann-Whitney U-test to test whether the distribution of orientation errors differs between beetle groups under simulated Milky Way
#    and equal-intesntiy illumination conditions.Make sure to state your hypotheses, statistical conclusion, and biological interpretation.

# H0: The distribution of orientation errors is the same in the populations
# of the two groups; HA: The distribution of orientation errors is not the same between the
# two populations. Ranks of the two groups, accounting for ties: Equal intensity: 7.0 9.5
# 11.0 12.0 14.5 14.5 17.0 18.0 19.0 20.0; Milky Way: 1.5 1.5 3.0 4.5 4.5 6.0 8.0 9.5
# 13.0 16.0. n1 = 10, n2 = 10, U = 87.5, U0.05(2),10,10 = 77. Since U is greater than or equal to
# U0.05(2),10,10, reject H0. Orientation errors differ between the two distributions. 

# Wilcoxon rank-sum test (aka Mann-Whitney U-test)
wilcox.test(OrientationError ~ Condition, data = milkyWaybeetles)


# 5. Scorpions
# a. Read and inspect the data. Are the data normally distributed in the two groups?
scorpions <- read.csv("chap19q27Pseudoscorpions.csv")
hist(scorpions$numberOfSuccessfulBroods[scorpions$matingTreatment=="DM"])
hist(scorpions$numberOfSuccessfulBroods[scorpions$matingTreatment=="SM"])

# b. Conduct a permutation test for the null hypothesis of no difference in the mean number of broods between treatments.State your hypotheses, 
#    statistical conclusion, and biological interpretation.

# H0: The difference between means is zero. HA: The difference between means is not zero.
# The test statistic is the difference between the means. 

# First calculate the observed difference between means 
scorpionMeans <- summarize(group_by(scorpions, matingTreatment), numberOfSuccessfulBroods = mean(numberOfSuccessfulBroods, na.rm = TRUE))
scorpionMeans <- as.data.frame(scorpionMeans)
scorpionMeans
diffMeans <- scorpionMeans$numberOfSuccessfulBroods[2] - scorpionMeans$numberOfSuccessfulBroods[1]
diffMeans
# 1.425 (DM higher than SM)

nPerm <- 10000
# Create a loop to permute the data many times (the number of times determined by nperm). 
# In the following loop, i is a counter that climbs from 1 to nPerm by 1. 
# In each iteration, the permuted difference is saved in the object permResult.
permResult <- vector() # initializes
for(i in 1:nPerm){
  # step 1: permute the times to mating
  scorpions$permSample <- sample(scorpions$numberOfSuccessfulBroods, replace = FALSE)
  # step 2: calculate difference betweeen means
  permSampleMean <- as.data.frame(summarize(group_by(scorpions, matingTreatment), 
                                            permMean = mean(permSample, na.rm = TRUE)))
  permResult[i] <- permSampleMean$permMean[2] - permSampleMean$permMean[1]
}
# look at a histogram of the results from the permutations
hist(permResult, right=F, breaks=100)
# Alternatively, you could use ggplot to achieve the same thing
ggplot(data.frame(permResult), aes(permResult)) + 
  geom_histogram(fill = "white", col = "black", binwidth = 1, 
                 boundary = 0, closed = "left") + 
  labs(x = "Permuted difference in treatment means (hours)", y = "Frequency") + 
  theme_classic()
# Now use the null distribution to calculate an approximate p-value
# We do this by determining the proportion of permuted means that fall below the observed difference in means (-18.25734)
# Number of permuted means that fall below diffMeans
# 98 fall below diffmeans
sum(as.numeric(permResult <= diffMeans))
# Proportion of permuted means that fall below diffMeans
sum(as.numeric(permResult <= diffMeans)) / nPerm
# Multiply by two to get the two-tailed p-value
2 * ( sum(as.numeric(permResult <= diffMeans)) / nPerm )

# The left tail of the null distribution indicates that 98 of 10,000 permuted samples had a
# difference less than or equal to -1.425 (were as extreme or more extreme in this tail).
# Multiply by 2 to get our approximate P-value: P = 2 Pr[difference ??? -1.425] = 
# 2*(98/10000) = 0.0196. Since P < 0.05, reject H0. Mean number of broods is greater in the
# DM treatment than in the SM treatment.


# c. Repeat the permutation test using the median instead of the means.

# First calculate the observed difference between medians 
scorpionMedians <- summarize(group_by(scorpions, matingTreatment), numberOfSuccessfulBroodsMed = median(numberOfSuccessfulBroods, na.rm = TRUE))
scorpionMedians <- as.data.frame(scorpionMedians)
scorpionMedians
diffMedians <- scorpionMedians$numberOfSuccessfulBroods[2] - scorpionMedians$numberOfSuccessfulBroods[1]
diffMedians
# -2 (DM higher than SM)

nPerm <- 10000
# Create a loop to permute the data many times (the number of times determined by nperm). 
# In the following loop, i is a counter that climbs from 1 to nPerm by 1. 
# In each iteration, the permuted difference is saved in the object permResult.
permResult <- vector() # initializes
for(i in 1:nPerm){
  # step 1: permute the times to mating
  scorpions$permSample <- sample(scorpions$numberOfSuccessfulBroods, replace = FALSE)
  # step 2: calculate difference betweeen medians
  permSampleMedian <- as.data.frame(summarize(group_by(scorpions, matingTreatment), 
                                              permMedian = median(permSample, na.rm = TRUE)))
  permResult[i] <- permSampleMedian$permMedian[2] - permSampleMedian$permMedian[1]
}
# look at a histogram of the results from the permutations
hist(permResult, right=F, breaks=100)
# Alternatively, you could use ggplot to achieve the same thing
ggplot(data.frame(permResult), aes(permResult)) + 
  geom_histogram(fill = "white", col = "black", binwidth = 1, 
                 boundary = 0, closed = "left") + 
  labs(x = "Permuted difference in treatment medians (hours)", y = "Frequency") + 
  theme_classic()
# Now use the null distribution to calculate an approximate p-value
# We do this by determining the proportion of permuted medians that fall below the observed difference in medians (-18.25734)
# Number of permuted medians that fall below diffMeans
# 98 fall below diffmedians
sum(as.numeric(permResult <= diffMeans))
# Proportion of permuted medians that fall below diffMeans
sum(as.numeric(permResult <= diffMeans)) / nPerm
# Multiply by two to get the two-tailed p-value
2 * ( sum(as.numeric(permResult <= diffMeans)) / nPerm )

# The left tail of the null distribution indicates that 862 of 10,000 permuted samples had a
# difference less than or equal to -2 (were as extreme or more extreme in this tail).
# Multiply by 2 to get our approximate P-value: P = 2 Pr[difference ??? -2] = 
# 2*(862/10000) = 0.1724. Since P > 0.05, fail to reject H0. Median number of broods similar in the two treatments.

