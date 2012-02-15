#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 1 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 

## PART 1 : LOADING & INSPECTING A DATASET
## =======================================

# - [ Importing a dataset in .csv format from a local directory ] -
 dataset.dat <- read.csv("C:/.../dataset.csv", header=TRUE, sep=",")

# - [ Importing a dataset in .csv format from the web ] -
 growth.dat <- read.csv("http://thiloklein.de/R/growth", header=T, sep=",")

# - [ Checking the data was successfully imported ] -
 ls()

# - [ Inspecting the dataset ] -
 growth.dat

# - [ Viewing the first/last 5 observations in the dataset ] -
 head(growth.dat)
 tail(growth.dat)

# - [ Inspecting the structure of the dataset ] -
 str(growth.dat)

# - [ Listing the names of the variables ] -
 names(growth.dat)


## PART 2 : GETTING HELP & INSTALLING PACKAGES
## ===========================================

# - [ Finding help for a function that is already installed ] -
 ?install.packages

# - [ Finding help for a topic or for a function that is not yet installed ] -
 ??skewness

# - [ Installing packages from the command line ] -
 install.packages("timeDate")

# - [ Loading non-default packages ] -
 library(timeDate)


## PART 3 : SUBSET SELECTION & DATA TYPES
## ======================================

# - [ Selecting individual variables from the dataset ] -
 # EITHER
  growth.dat$empgrow
 # OR
  attach(growth.dat)
  empgrow

# - [ Choosing a subset of the data ] -
 # EITHER
  growth.dat[empgrow < 0, ]
  growth.dat[country == "Italy", ]
  growth.dat[c(1:3,11:13), c(1,3)]
 # OR
  subset(growth.dat, empgrow < 0)
  subset(growth.dat, country == "Italy")

# - [ Finding the class of an object ] -
 class(growth.dat)
 class(country)
 class(empgrow)

# - [ Changing the class of an object ] -
 as.factor(empgrow)
 as.numeric(country)


## PART 4 : GENERATING VARIABLES & PERFORMING TRANSFORMATIONS
## ==========================================================

# - [ Generating a new variable, and adding it to the original data frame ] -
 growth.dat$gdptoemp <- growth.dat$GDPgrow / growth.dat$empgrow

# - [ Generating an indicator variable, kept separate from the original data frame ] -
 diverging <- ifelse(growth.dat$gdptoemp < 0, 1, 0)

# - [ Performing a linear transformation ] -
 1 + GDPgrow/100

# - [ Performing a quadratic transformation ] -
 1 + log(GDPgrow) + GDPgrow^2

# Note: Can perform transformations using, e.g. +,-,*,/,log,exp,^


## PART 5 : CLEARING VARIABLES, OBJECTS & THE WORKSPACE
## ====================================================

# - [ Detach variables from dataset so they can no longer be called directly ] -
 detach(growth.dat)
 empgrow

# - [ Remove a variable from a dataset ] -
 growth.dat$GDPgrow <- NULL
 names(growth.dat)

# - [ Remove an object from the workspace ] -
 rm(growth.dat)
 ls()

# - [ Clear the entire workspace of all objects ] -
 rm(list=ls())