#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 4 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")
 
 student.dat <- read.csv("http://thiloklein.de/R/eaef", header=T, sep=",")
 attach(student.dat)
 student.lm <- lm(earnings~age+schooling+height+weight+siblings, student.dat)
 lnearnings <- log(earnings) ; weightsq <- weight^2
 student.lm4 <- lm(lnearnings~schooling+height+weight+weightsq, student.dat)

 
## PART 1 : Tests for Model Misspecification
## =================================================
 
 # Step 1 : Plot component + residual plots (see L3E2)
 # Step 2 : Formal test (here)
 
# - [ RESET Test ] -
 # H0: model is not misspecified
 reset(student.lm)
 reset(student.lm4)
 # reject H0 for student.lm => evidence of misspecification, should re-specify model
 # accept H0 for student.lm4 => no evidence of misspecification, the re-specification of student.lm performs well
 
 # NOTE : you cannot test formally for omitted variable bias caused by missing variables from
 #        your data set, the RESET test only checks whether any e.g. higher order polynomials
 #        are missing
 
## PART 2 : Tests for Heteroscedasticity
## =================================================
 
# Step 1 : Residual plot (see L3E1)
# Step 2 : Formal tests (here)
 
# - [ Breusch-Pagan Test ] -
 # install.packages("lmtest")
 library(lmtest)
 bptest(student.lm)
 bptest(student.lm4)

# - [ White Test ] -
 bptest(student.lm,~age*schooling*height*weight*siblings+I(age^2)+I(schooling^2)+I(height^2)+I(weight^2)+I(siblings^2))
 bptest(student.lm4,~schooling*height*weight*weightsq+I(schooling^2)+I(height^2)+I(weight^2)+I(weightsq^2))
 # Aside : We have approx. 20 variables and 540 observations => safe to use White Test
 # As rule of thumb, ensure have at least 5 times as many observations as variables
 
# - [ Heteroscedasticity Robust Standard Errors ] -
 # Note : For this course we ask you to ALWAYS use robust standard errors
 source("http://thiloklein.de/R/myfunctions.R")
 summary(student.lm) ; shccm(student.lm)
 summary(student.lm4) ; shccm(student.lm4)

 # NOTE : Should perform RESET test for model misspecification before tests for heteroskedasticity
 
## PART 3 : Normality
## =================================================
 
 # Step 1 : Residual plot (see L3E1)
 # Step 2 : Histogram and QQ-plot of standardised residuals (see L3E1)
 # Step 3 : Formal test (here)
 
# - [ Jarque-Bera Test ] -
 # install.packages("tseries")
 library(tseries)
 jarque.bera.test(student.lm$res)
 jarque.bera.test(student.lm4$res)
 # neither satisfy normality, but this does not mean estimate of regression coefficients are biased
 # notice that using student.lm4 we reduce Chi-Sq value from >6000 to <70
 # => check outliers, these may be causing problems
 # => prefer White Test for heteroscedasticity (robust against non-normality)

## PART 4 : Outliers
## =================================================
 
# - [ Identifying Outliers ] -
 library(car)
 outlierTest(student.lm)
 outlierTest(student.lm4)
 
# - [ Plotting these potential outliers ] -
 par(mfrow=c(1,2))
 plot(rstudent(student.lm)) ; abline(2,0, col="red") ; abline(-2,0, col="red")
 outs <- names(outlierTest(student.lm)$p)
 text(as.integer(outs),rstudent(student.lm)[as.integer(outs)], adj=c(0,1.5), outs)
 
 plot(rstudent(student.lm4)) ; abline(2,0, col="red") ; abline(-2,0, col="red")
 outs <- names(outlierTest(student.lm4)$p)
 text(as.integer(outs),rstudent(student.lm4)[as.integer(outs)], adj=c(1.5,0), outs)
 # Note : 1 in 20 observations should naturally lie outside these limits

## PART 5 : Leverage
## =================================================
 
 # Rule of thumb : For large samples, observations with hat values > 2*avg hat value are noteworthy;
 # For small samples, observations with hat values > 3*avg hat value are noteworthy
 
 # - [ Examine Leverage : Hat Values ] -
 K <- length(coef(student.lm)) ; N <- nrow(student.lm$model)
 avg.hat <- K/N         # Average hat value
 unname(which(hatvalues(student.lm)>2*avg.hat))
 # Notice none of the observations appear to be both outliers and have high leverage
 
 K <- length(coef(student.lm4)) ; N <- nrow(student.lm4$model)
 avg.hat4 <- K/N         # Average hat value
 unname(which(hatvalues(student.lm4)>2*avg.hat4))
 # Notice observation 533 is both an outlier and has high leverage
 
# - [ Plotting the hat values ] -
 par(mfrow=c(1,2))
 plot(hatvalues(student.lm)) ; abline(h=c(2,3)*avg.hat, lty=2, lwd=2, col=c("orange", "red"))
 plot(hatvalues(student.lm4)) ; abline(h=c(2,3)*avg.hat4, lty=2, lwd=2, col=c("orange", "red"))

## PART 6 : Influence
## =================================================
 
 # My rule of thumb : Observations with Cook's D' > 4/sqrt(n-k-1) are noteworthy;
 
 # - [ Examine Influence : Cook's D ] -
 unname(which(cooks.distance(student.lm) > 4/sqrt(student.lm$df.residual)))
 unname(which(cooks.distance(student.lm4) > 4/sqrt(student.lm4$df.residual)))
 # none of the observations appear to have high degree of influence, but can plot to find
 # out which are most influential
 
 # - [ Plot of Cook's D ] -
 par(mfrow=c(1,2))
 plot(student.lm, which=4) ; abline(h=4/sqrt(student.lm$df.residual), lty=2, col="red")
 plot(student.lm4, which=4) ; abline(h=4/sqrt(student.lm4$df.residual), lty=2, col="red")
 
 # - [ Combined Outlier, Leverage & Influence Plots ] -
 par(mfrow=c(1,2))
 plot(student.lm, which=5)
 influencePlot(student.lm, id.method="noteworthy", labels=row.names(student.dat))
 
 par(mfrow=c(1,2))
 plot(student.lm4, which=5)
 influencePlot(student.lm4, id.method="noteworthy", labels=row.names(student.dat))
 

## ===============================================
 detach(student.dat)