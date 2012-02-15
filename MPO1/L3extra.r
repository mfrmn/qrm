 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://thiloklein.de/R/myfunctions.R")
 
 # Load the data set
 eval.dat <- read.csv("http://frmn.me/eval.csv", header=T, sep=",")
 
 # Inspect data set
 str(eval.dat)
 # => observe that prof is integer when should be factor
 
 # Change prof from integer to factor
 eval.dat$prof <- as.factor(eval.dat$prof)
 
 # Fit full model
 eval.lm <- lm(eval~.,eval.dat)
 summary(eval.lm)
 # => observe only profs 40, 48, 55, 75 seem to be significant
 
 # Create indicator functions specifying whether prof is one of above
 eval.dat$prof40 <- ifelse(eval.dat$prof == 40, 1, 0)
 eval.dat$prof48 <- ifelse(eval.dat$prof == 48, 1, 0)
 eval.dat$prof55 <- ifelse(eval.dat$prof == 55, 1, 0)
 eval.dat$prof75 <- ifelse(eval.dat$prof == 75, 1, 0)
 
 # Fit new model excluding all professors except those specified above
 eval.lm2 <- lm(eval~.-prof,eval.dat)
 shccm(eval.lm2)
 
 # Residual plot
 plot(fitted.values(eval.lm2), rstandard(eval.lm2))
 lines(lowess(fitted.values(eval.lm2), rstandard(eval.lm2)), col="blue", lwd=2)
 abline(0,0, col="red", lwd=2, lty=2)
# => no obvious problems
 
 # VIFs
 vif(eval.lm2)
 # => very large VIFs for students and allstudents
 #    not surprising, classes with lots of students are
 #    also likely to have more respondents
 
 # Create new variable = % of respondents
 eval.dat$pcnt <- eval.dat$students / eval.dat$allstudents
 
 # Drop # respondents, add % respondents
 eval.lm3 <- lm(eval~.-prof-students,eval.dat)
 shccm(eval.lm3)
 vif(eval.lm3)
 # => notice that this has significantly reduced VIFs
 
 # Test whether to drop 4 insignificant variables
 linearHypothesis(eval.lm3, c("age","divisionupper","tenureyes","allstudents"))
 # => Do not reject restricted model
 
 # Fit new restricted model
 eval.lm4 <- lm(eval~.-prof-students-age-division-tenure-allstudents,eval.dat)
 shccm(eval.lm4)
 
 # Check for transformations of dependent var
 par(mfrow=c(1,1))
 boxcox(eval.lm4, plotit=T)
 # => lambda close to 2, but no justification to square dependent var
 #    => do not transform
 
 # Check for transformations of independent vars
 crPlots(eval.lm4)
 # => no major deviations from straight line, do not transform
 
 # Check for possible interactions
 eval.lm5 <- lm(eval~minority+gender+credits+beauty+native+prof40+prof48+prof55+prof75+pcnt+gender:beauty,eval.dat)
 shccm(eval.lm5)
 # => suggestions beautiful men get additional benefit

 # Now should try adding additional interactions etc. into the model.