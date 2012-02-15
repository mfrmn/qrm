#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 3 - EXERCISE 2 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 
 student.dat <- read.csv("http://thiloklein.de/R/eaef", header=T, sep=",")
 attach(student.dat)
 student.lm <- lm(earnings~age+schooling+height+weight+siblings, student.dat)
 

## PART 1 : TRANSFORMATION OF THE DEPENDENT VARIABLE
## =================================================
 
# - [ Can use box-cox plot to suggest transformation ] -
 boxcox(student.lm, plotit=T)
 powerTransform(student.lm)
 # => a lambda close to 0 indicates should try taking log of dependent variable
 
 # - [ Generate a new variable from the original and re-specify model ] -
 lnearnings <- log(earnings)
 student.lm2 <- lm(lnearnings~age+schooling+height+weight+siblings, student.dat)
 
 # - [ Re-check model summary and box-cox plot for new model ] -
 summary(student.lm2)
 boxcox(student.lm2)
 # => a lambda close to 1 indicates no transformation required

## PART 2 : TRANSFORMATION OF THE INDEPENDENT VARIABLE(S)
## ======================================================
 
 # - [ Produce component + residual plots ] -
 # these are another way of checking for linearity of variables in addition to
 # the residual plots of L3E1 Part 2. green line should be approximately straight
 library(car)
 crPlots(student.lm2)
 # => drop off at higher levels of weight may suggest non-linear relationship between
 #    weight and earnings (check this makes sense intuitively)
 
 # - [ Generate a new variable from the original and re-specify model ] -
 weightsq <- weight^2
 student.lm3 <- lm(lnearnings~age+schooling+height+weight+weightsq+siblings, student.dat)
 
 # - [ Re-check model summary and cr-plots for new model ] -
 summary(student.lm3)
 crPlots(student.lm3) # much better

## PART 3 : F-TESTS FOR GOODNESS OF FIT
## ====================================

# - [ Use linearHypothesis to automatically perform finite-sample F-test ] - 
 library(car)
 linearHypothesis(student.lm3, c("age"))
 # Notice that the p-values for the F- and t-tests (found in summary)
 # are equivalent when there is only one variable in the above

# - [ Repeat the above procedure, letting both age and siblings = 0 in the restricted model ] -
 linearHypothesis(student.lm3, c("age","siblings"))
 # => evidence suggests cannot reject hypothesis that age = 0, siblings = 0

# - [ Repeat the above procedure, letting age, siblings, weight & weightsq = 0 in the restricted model ] -
 linearHypothesis(student.lm3, c("age","siblings","weight","weightsq"))
 # => reject hypothesis, at least some of these variables are different from 0

 # - [ Re-specify model and check summary ] -
 student.lm4 <- lm(lnearnings~schooling+height+weight+weightsq, student.dat)
 summary(student.lm4)

## PART 4 : RE-CHECKING THE GAUSS-MARKOV CONDITIONS
## ================================================

 # - [ Produce residual plot ] -
 par(mfrow=c(1,1))
 plot(fitted.values(student.lm4), rstandard(student.lm4))
 lines(lowess(fitted.values(student.lm4), rstandard(student.lm4)), col="blue", lwd=2)
 abline(0,0, col="red", lwd=2, lty=2)
 # => this is much better, no reason to be concerned

 # - [ Check for normality ] -
 par(mfrow=c(1,2))
 grid.x <- seq(min(rstandard(student.lm4)), max(rstandard(student.lm4)), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(rstandard(student.lm4)))*nrow(student.dat)
 hist(rstandard(student.lm4), ylim=c(0, max(grid.y))) ; lines(grid.x, grid.y, col="red")
 qqPlot(student.lm4, simulate=T)
 # => again, much better

 # - [ Check for multicollinearity ] -
 vif(student.lm4)
 # notice the huge VIFs for weight and weightsq. this is not surprising (why not!?)
 # no need to do anything about this
 
 
## ===============================================
 detach(student.dat)