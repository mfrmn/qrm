#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 1 - EXERCISE 2 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 growth.dat <- read.csv("http://thiloklein.de/R/growth", header=T, sep=",")
 attach(growth.dat)


## PART 1 : BASIC SUMMARY STATISTICS
## =================================

# - [ Generate summary statistics (min, max, quartiles, median, mean) ] -
 summary(growth.dat)

# - [ Generating individual summary statistics for chosen variables ] -
 min(empgrow); max(empgrow)
 median(empgrow); mean(empgrow)
 #sum(empgrow) / length(empgrow)        # manual calculation of mean
 quantile(empgrow)

# - [ Finding the variance of a chosen variable ] -
 var(empgrow)
 #sum((empgrow-mean(empgrow))^2)/(length(empgrow)-1)        # manual calculation of variance

# - [ Determining the standard deviation of a chosen variable ] -
 sd(empgrow)
 #sqrt(var(empgrow))      # alternatively take sqrt of variance

## PART 2 : MORE ADVANCED SUMMARY STATISTICS
## =========================================

# - [ Ensure all necessary packages installed and loaded ] -
 #install.packages("timeDate")
 library(timeDate)

# - [ Calculating the skewness and kurtosis of a chosen variable ] -
 skewness(empgrow)
 kurtosis(empgrow, method="moment")

# - [ Determining the covariance / correlation between TWO variables ] -
 cov(GDPgrow, empgrow)
 cor(GDPgrow, empgrow)
 

## PART 3 : PROPORTIONS & CONDITIONAL PROBABILITY
## ==============================================

# - [ Showing the total number of observations with a given value ] -
 diverging <- ifelse(empgrow < 0, 1, 0)
 table(diverging)

# - [ Finding the proportion of observations with a given value ] -
 table(diverging) / length(diverging)

# - [ Calculating conditional expectation: E[X|Y] ] -
 by(growth.dat$GDPgrow, diverging, mean)  # mean GDPgrowth for countries where GDPgrowth & empgrow have opposite signs


## PART 4 : INTRO TO DATA VISUALISATION
## ====================================

# - [ Simple 2D plot with observations (x,y) ] -
 plot(x=GDPgrow, y=empgrow, xlab="GDP Growth (%)", ylab="Employment Growth (%)", main="Employment vs. GDP Growth in 25 OECD Countries")

# - [ Add a straight line through the current plot with specified intercept and slope ] -
 abline(lm(empgrow~GDPgrow), col="red")

# - [ Add a smoothed, locally weighted line through the current plot ] -
 lines(lowess(GDPgrow,empgrow), col="blue")
 
# - [ Place a legend on the current plot to distinguish between different elements ] -
 legend("topleft", c("Line of Best Fit","Smoothed Locally Weighted Regression Line"), fill=c("red","blue"), bty="n")

# - [ Produces a matrix of scatterplots, with each variable being plotted against all other variables ] -
 pairs(growth.dat)
 
# - [ Produces a histogram for a chosen variable ] -
 par(mfrow=c(1,2))    							# allows you to place multiple plots in the same window
 hist(empgrow); hist(GDPgrow)

 
## ===============================================
 detach(growth.dat)