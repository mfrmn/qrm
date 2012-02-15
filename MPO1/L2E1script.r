#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 2 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 

## PART 1 : WORKING WITH DISTRIBUTIONS IN R
## ========================================

# - [ Normal density : returns Pr(X=x) ] -
 dnorm(x=2, mean=3, sd=2)
 
# - [ Normal distribution function : returns Pr(X<x=q) ] -
 pnorm(q=0, mean=0, sd=1)
 
# - [ Normal quantile function : returns value of x st p=Pr(X<x) ] -
 qnorm(p=0.95, mean=0, sd=1)
 # set lower.tail = FALSE to instead return value of x s.t. p=Pr(X>x)

# Note : Can be repeated for other distributions, e.g. t-dist: dt,pt,qt; F-dist: df,pf,qf;

## PART 2 : POINT & INTERVAL ESTIMATION - KNOWN SD
## ===============================================

# - [ Determining the 5% critical value(s) ] -
 qnorm(0.95)      										# 1-sided, > test
 qnorm(0.05)											# 1-sided, < test
 c(qnorm(0.025),qnorm(0.975))							# 2-sided test

# - [ Calculating the z-score for a sample ] -
 xobs <- 9.5; xexp <- 10; xsd <- 0.1/sqrt(30)
 zscore <- (xobs-xexp)/xsd; zscore

# - [ Finding the critical regions ] -
 c(qnorm(0.95)*xsd+xexp, Inf)		                                        # 1-sided, > test
 c(-Inf, qnorm(0.05)*xsd+xexp)		                                        # 1-sided, < test
 rbind(c(-Inf, -qnorm(0.975)*xsd+xexp), c(qnorm(0.975)*xsd+xexp, Inf))		# 2-sided test

# - [ Constructing confidence intervals for the population mean ] -
 qnorm(0.975)*(xsd)*c(-1,1)+xobs

## PART 3 : POINT & INTERVAL ESTIMATION - UNKNOWN SD
## =================================================

# - [ Loading dataset and calculating weight to height ratio ] -
 student.dat <- read.csv("http://thiloklein.de/R/eaef", header=T, sep=",")
 attach(student.dat)
 wtoh <- weight/height

# - [ Determining degrees of freedom and 5% critical value(s) ] -
 df <- length(wtoh)-1
 qt(0.95, df)													# 1-sided, > test
 qt(0.05, df)													# 1-sided, < test
 c(qt(0.025, df),qt(0.975, df))						            # 2-sided test

# - [ Calculating the t-statistic for a sample ] -
 xobs <- NA; xexp <- NA; xsd <- NA
 xobs <- mean(wtoh); xexp <- 2.2; xsd <- sd(wtoh)/sqrt(length(wtoh))
 tstat <- (xobs-xexp)/xsd; tstat

# - [ Constructing confidence intervals for the population mean ] -
 c(qt(0.025, df), qt(0.975, df))*xsd+xobs
 
# Alternatively (*easier)
 t.test(wtoh, mu=2.2, alt="two.sided", conf.level = 0.95) 
 # change alt to "less" or "greater" to specify 1-sided test
 
 
## ===============================================
 detach(student.dat)