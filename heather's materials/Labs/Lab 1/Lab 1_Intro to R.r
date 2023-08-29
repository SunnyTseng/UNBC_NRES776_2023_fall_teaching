
#############################
# NRES 776
# Lab 1: Introduction to R
# Credit: This lab was developed by Dr. Che Elkin.
#############################

#############################
# Topics
# Using R Studio
# Global Options in R Studio
# Basic Calculations in R
# Vector Objects
# Matrix Objects
# Dataframes
# Reading data into R
#############################

#############################
# Relevant R Datacamp Modules
# Introduction to R
#############################

#########################################
# Using RStudio
#########################################

# RStudio components
# Script editor, R command line (console), Environment (+ history), Files (+plots, packages, help)

# Starting a new script:
# File, New File, R Script

# Moving commands from the script editor to the console
# Use ctrl+Enter to run a single line of code where the cursor resides
# You can also put your cursor on the code and use the "Run" button in the uppder right corner of the Script Editor
# Can copy and paste code from the script editor into the console
paste("hello world")

# You can run a chunk of code by highlighting the code and then using ctrl+Enter or clicking "Run"
# Hint: use Shift+down arrow to select the lines of code you want to run (Windows)
myMessage<-c("I","love","stats")
paste(myMessage, collapse=" ")

# Add a hashtag in front of a line of code that you don't want to run or where you want to put a comment
# myMessage<-c("I","love","stats")
# paste(myMessage, collapse=" ")

# Specify a data path
# Setting a data path tells R where to find files you want to work with and where to write output files
# Note that your path must be in quotes and you must use forward slashes instead of backwards slashes
dataPath <- "C:/Users/Heather/UNBC/NRES 776 2020/Labs/Lab 1" # This command tells R to look for files in this directory

# You can specify the working directory with setwd()
# Or, use the Rstudio menu: Sessions>Set Working Directory
getwd() # Check the path of the current working directory
setwd(dataPath) # Sets the path to the specified working directory
dir() # Outputs the files in the current directory

# TODO: set the working directory to the folder where you saved Lab 1_Intro to R.r
# TODO: list the files in that directory


#####################################
# Global Options in RStudio
#####################################

# You can customize your R Studio using the Tools>Global Options
# For example, under General, you can set the default initial working directory, and the default CRAN mirror (from where packages are downloaded)
# Under Appearance, you can set font size and visual theme for the console
# There are various other options



#####################################
# Basic Calculations in R
#####################################

# Using R for simple numeric operations (calculator)
3 + 4	# direct operation

# Assigning objects
# Three assignment methods ("<-","->","=")
# Be consistent and only use "<-"
a1 <- 15 # assign numeric scalar to a1
a2 <- 3
b1 <- TRUE # logical, assign b as true (T), no quotation
b2 <- FALSE
d <- "ecology" # assign character string to data
e <- 16/4 # output of operation assigned to object

# + - * / ^ %%

# TODO: what does %% do (e.g. 17 %% 5)
# TODO: what is produced by
	# a1 + b1
	# a1 + b2
	# a1 / b1
	# a1 / b2
	# b2 / a1
	# b2 / b2
	# a2 + d
	
# The function mode() reveals the type of an object
mode(a1) # "numeric"
mode(b2) # "logical"
mode(d) # "character"

# Working directory vs. working environment
# Only objects that are brought into R's working environment are available for use
ls() # list all the objects in the working environment

# Individual objects can be removed from the enviornment with rm()
rm(a1) # removes a1 from the work space
# TODO: what now happens with
	# e <- a1 + a2
	
#####################################
# vector objects
#####################################

# vectors: a set of scalars arranged in a one-dimensional array
# vector assignment: c(), read c as concatenate or combine
v1 <- c(1,3,2,5,4)
v2 <- c(2,3)
v3 <- c(v1,v2,v1)	# vectors can also be combined

length(v1)	# returns the length of the vector
length(v3)

# vector indexing: identifying and operating on vector components
v3
v3[4] # nth element of vector
v3[1:4]  # elements 1 to 4 of vector
v3[c(1,3,5)] # specific elements of vector
v3[v3 > 4] # all elements that are greater than 4
v3[4:length(v3)] # fourth element to the end (length(v3)) of the vector
v3[v3 == 3] # all elements that equal (==) 3
v3[v3 != 3] # all elements that do not equal (!=) 3
v3[v3 != 3 & v3 > 2] # all elelments that don't equal 3 and are greater than 2
v3[-2] # vector excluding element 2
v3[-c(1,4,6)] # vector excluding listed elements

# operating on vectors
a2
v1
nv1 <- v1 + a2
nv2 <- v2 * a2

# altering the sequence of vectors
v4 <- c(1,6,5,3,4,7,2,9,10,8)
v4s <- sort(v4)	# sorts the vector
v4o <- order(v4) # TODO: what is retuned and how does this differ from sort

# creating structured vectors
v4 <- rep(1,10)	# create a vector of 1's that is 10 elements long
v5 <- rep(c(1:5),3) # create a vector of 1:5 replicated 3 times
v6 <- rep(c(1:5), each = 3) # vector of 1:5 with each number sequentially repeated 3 times
v7 <- rep(c(1:5),each = 3, times = 2)
v8 <- c(1:20)
v9 <- seq(1,20,by=2.34)

# factors: vectors where the elements are factors, used to code experiments
v10 <- rep(c("high","med","low","veryLow"),each = 2) # create vector
v10 <- factor(v10) # transform vector in to factor

#####################################
# Matrix objects
#####################################

m1 <- matrix(c(1:12),nrow = 3, ncol = 4) # 3 by 4 matrix filled by column
m2 <- matrix(c(1:12),nrow = 3, ncol = 4, byrow = TRUE) # TODO: how does this differ?

# To determine the size of a matrix
dim(m2)

# Matrix indexing
m1
m1e1 <- m1[2,] # Just row 2
m1e2 <- m1[,3] # Just column 3
m1e3 <- m1[3,1] # Just the element in row 3, column 1

# TODO: when extracting a subset of data from a matrix what is returned?

is.matrix(m1e1)
is.vector(m1e1)
is.numeric(m1e1)

# Matrix operations
s1 <- 3
v1 <- c(1,2,3)
m1 <- matrix(c(1:12),nrow = 3, ncol = 4)

# TODO: what happens with each of these operations
m1a <- m1 + s1
m1b <- m1 * s1
m1c <- m1 + v1
m1d <- m1 * v1

# matrix transformation
m1
m2 <- t(m1)

#####################################
# Data frame objects
#####################################

# Data frames equivalent to a spread sheet, and very common data storage type
# Each column of the data frame is a vector
# Each vector can be of a different mode (e.g. numeric, factor)

f1 <- rep(c("blue","red","green"),each = 3) # create vector of factors
f2 <- c(1,5,3,7,4,8,5,2,4) # create vector of data

df1 <- data.frame(heatType=f1,age=f2) # create a data frame by binding vectors together

dim(df1) # returns the size of the data frame
str(df1) # returns the structure of the data frame
names(df1) # returns the column "names" of the data frame
names(df1)[2] # specific names can be identified by including their number

# indexing data frames as like a matrix
df1[2,] # data frame components from row 2
df1[,2] # data frame components from column 20

# indexing data frames using column names
df1$heatType # "$" operator specifies variables (column names) in the data frame
df1$heatType[3] # specifies the third element from variable heatType

# Saving data frames (and other data objects) from R-intro
# Saving all files as .csv data files is a very robust way to save data

write.csv(df1, file = "myDataTest.csv", row.names = FALSE)

# file = "myData.csv" # By not including a pathway the data is saved to the set working directory
# file = "C:/myData.csv"  # Fully specifying the path can be less risky as you know where you are saving the data

# .csv file can then be read back into the R enviornment with
# read.csv(file, header = TRUE)

df2 <- read.csv("myDataTest.csv", header=TRUE)

#######################################
# Reading data into R
#######################################

# You can read a variety of data types into R, including excel and text files
# csv files are one of the easiest file types for reading into R
# For example:
# dataReadIn <- read.csv(nameOfDataFile.csv,header = TRUE)

# Let's try loading a sample csv file with salmon biomass data
salmonData<-read.csv("chap02f2_5SalmonBodySize.csv")

# Look at the "Data tab" under "Environment (top left)
# Look at the data
fix(salmonData) # quick and easy, but can accidentally make edits and then need to re-load data
View(salmonData) # doesn't show all the data for large dataframes

# Check that the data loaded properly
head(salmonData)
dim(salmonData)
str(salmonData)
nrow(salmonData)
ncol(salmonData)


# Exercises

# TODO: create a data file in excel, 
	# save as a .csv file, 
	# read the file into R,
	# output the size of the data frame,
	# list all of the data frame components,
	# ceate a new data frame that contans a subset of the original data frame,
	# save the new data frame as a .csv file

	