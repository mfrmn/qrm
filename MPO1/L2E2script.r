#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 2 - EXERCISE 2 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 
 house.dat <- read.csv("http://thiloklein.de/R/housing", header=T, sep=",")
 attach(house.dat)
 

## PART 1 : SIMPLE LINEAR REGRESSION
## =================================

# - [ Fitting a linear regression model in R ] -
 house.lm <- lm(housing~total, data=house.dat)

# - [ Showing coefficient estimates, std. errors and significance ] -
 summary(house.lm)
 # also returns measures of model fit, e.g. R^2

# - [ Plotting observations and line of best fit ] -
 plot(housing~total, xlim=c(0,3000))
 abline(house.lm, col="red")
 
## PART 2 : SIGNIFICANCE & H-TESTING OF COEFFICIENTS
## =================================================

# - [ Calculating confidence intervals for the coefficients ] -
 totcoef <- coefficients(house.lm)[2]
 totse <- summary(house.lm)$coef[2,2]
 df <- length(housing)-length(coefficients(house.lm))   # appropriate degrees of freedom
 totcoef+c(-1,1)*totse*qt(0.975,df)

# - [ Determining significance of coefficients ] -
 qt(0.975, df)    		# 2-sided, 5% critical
 totcoef/totse			# t-value with null : coeff = 0, alt : coeff != 0

# - [ Checking whether coefficient less than some value (e.g. 0.05) ] -
 qt(0.05, df)					# 1-sided, 5% critical
 (totcoef-0.05)/totse		    # t-value with null : coeff = 0.05, alt : coeff < 0.05

# Alternatively (*2-sided)
 # install.packages("car")
 library(car)
 linearHypothesis(house.lm, c("total"), c(0.05))

## PART 3 : MULTIPLE LINEAR REGRESSION
## ===================================
 
 student.dat <- read.csv("http://thiloklein.de/R/eaef", header=T, sep=",")
 student.dat$ID <- NULL
 attach(student.dat)

# - [ Plotting all the variables with smoothed, locally weighted lines ] -
 pairs(student.dat, panel=panel.smooth)

# - [ Creating multiple linear regression model including all variables ] -
 student.lm <- lm(earnings~age+schooling+height+weight+siblings, student.dat)
 #student.lm <- lm(earnings~., student.dat)
 summary(student.lm)

# - [ Creating multiple linear regression model including all variables, excluding intercept ] -
 student.lm2 <- lm(earnings~.-1, student.dat)
 summary(student.lm2)

 
## ===============================================
 detach(house.dat) ; detach(student.dat)