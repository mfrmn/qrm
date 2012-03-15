#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 4 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

# - [ Header ] - 
 rm(list=ls()) ; par(mfrow=c(1,1))

 # - [ Useful Packages ] - 
 #install.packages("RCurl")
 library(AER)
 
 # - [ Custom Functions ] -
 source("http://www.thiloklein.de/R/myfunctions.R")
 source_https <- function(url, ...) {
    require(RCurl)
    sapply(c(url, ...), function(u) {
        eval(parse(text = getURL(u,followlocation=TRUE,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))),envir=.GlobalEnv)
    })
 }
 source_https("https://raw.github.com/mfrmn/qrm/master/mfrmnFns.r")

 
## PART 1 : Simple Logit and Probit w/ Outcome Probs
## =================================================
 
# - [ Loading and Storing the Data ] -
 loan.dat <- read.csv("http://thiloklein.de/R/Lent/loanapp.csv", header=T, sep=",")
 str(loan.dat)

# - [ Determine prob of loan approval for whites vs non-whites ] -
 app.mat <- matrix(NA,nrow=2,ncol=3); rownames(app.mat) <- c("white","non-white")
 # ^ matrix to store loan probabilities
 colnames(app.mat) <- c("OLS", "logit", "probit")
 
 # using OLS
 loan.lm <- lm(approve~white, loan.dat)
 shccm(loan.lm)
 app.mat[1,1] <- c(1,1)%*%coef(loan.lm) # whites
 app.mat[2,1] <- c(1,0)%*%coef(loan.lm) # non-whites
 
 # using logit
 loan.lg <- glm(approve~white, family=binomial(link=logit), data=loan.dat)
 summary(loan.lg) # returns some model fit statistics (e.g. AIC)
 coeftest(loan.lg, vcov=hc0) # heteroskedasticity corrected s.e.
 F.log <- function(x){ # logistic function
     1/(1 + exp(-x))
 }
 app.mat[1,2] <- F.log(c(1,1)%*%coef(loan.lg)) # whites
 app.mat[2,2] <- F.log(c(1,0)%*%coef(loan.lg)) # non-whites
 
 # using probit
 loan.pb <- glm(approve~white, family=binomial(link=probit), data=loan.dat)
 summary(loan.pb)
 coeftest(loan.pb, vcov=hc0)
 app.mat[1,3] <- pnorm(c(1,1)%*%coef(loan.pb)) # whites
 app.mat[2,3] <- pnorm(c(1,0)%*%coef(loan.pb)) # non-whites
 
 app.mat
 # - note that ols, logit and probit are equivalent at end points when using a single
 #   binary regressor
 # - based on above, evidence suggests white people have higher loan probabilities
 
## PART 2 : Advanced Logit and Probit w/ Outcome Probs
## ===================================================
 
 # it may be the case that discrimination is not occuring, just that white people
 # exhibit characteristics that make them more likely to be approved for loans
 # e.g. higher income, etc.
 
# - [ Expand the logit and probit models ] -
 loan.lm2 <- lm(approve~white+hrat+obrat+loanprc+unem+male+married+dep 
                 +sch+cosign+chist+pubrec+mortlat1+mortlat2+vr, data=loan.dat)
 loan.lg2 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep 
                 +sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,
                 family=binomial(link=logit), data=loan.dat)
 loan.pb2 <- glm(approve~white+hrat+obrat+loanprc+unem+male+married+dep 
                 +sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,
                 family=binomial(link=probit), data=loan.dat)
 shccm(loan.lm2) ; coeftest(loan.lg2, vcov=hc0) ; coeftest(loan.pb2, vcov=hc0)
 # - white variable still highly significant => evidence that discrimination occurs
 
# - [ Re-calculate probability of loan approval ] - 
 
 # PROBLEM : with multiple variables we have to choose a specific value to multiply
 # by each coefficient. normally we choose the mean value that the variable takes
 # (e.g. the average height). however, if we have binary variables (e.g. male/female)
 # it doesn't make sense to take the average => we must set each binary equal to 0 or
 # 1, all other variables equal to their mean value, and calculate the probabilities.
 
# - [ Detecting which variables are binary ] -
 mm <- model.matrix(loan.pb2) # take model matrix
 mm.means <- sapply(1:ncol(mm), function(i) mean(mm[,i])) # calculate mean of variables
 names(mm.means) <- colnames(mm)
 mm.means.l <- mm.means ; mm.means.u <- mm.means
 bin.v <- sapply(1:ncol(mm), function(i) nrow(mm) == sum(((mm[,i] == 1) + (mm[,i] == 0))))
 # ^ detect if any variables are binary
 bin.loc <- which(bin.v == T) # determine which variables are binary
 
# - [ Replace the mean value of all binary variables by a 1 or 0 ] -
 mm.means.u[bin.loc] <- c(1,1,0,0,0,0,0,0,0,0,0) # 1 in position of white and intercept
 mm.means.l[bin.loc] <- c(1,0,0,0,0,0,0,0,0,0,0) # 0 in position of white, 1 for intercept
 # - replace the mean value of all binary variables by desired value
 # -> since we are interested in difference in probability of white being accepted
 #    vs. non-white, the only difference is between the 1 and 0 in the white column
 
# - [ Calculate the z-values for white and non-white using logit ] -
 zval.u <- mm.means.u%*%coef(loan.lg2) # white
 zval.l <- mm.means.l%*%coef(loan.lg2) # non-white
 
# - [ Calculate probability of loan approval for white and non-white ] -
 F.log(zval.u) ; F.log(zval.l) # using logit
 
# - [ Calculate the z-values for white and non-white using probit ] -
 zval.u <- mm.means.u%*%coef(loan.pb2) # white
 zval.l <- mm.means.l%*%coef(loan.pb2) # non-white
 
# - [ Calculate probability of loan approval for white and non-white ] -
 pnorm(zval.u) ; pnorm(zval.l) # using probit
 
# - [ Repeat for all variations of binary variables ] -
 # can use my custom function, auto.it
 # model : name of model (either logit or probit model)
 # alter : the name of the variable we want to examine effect of change in on prob.
 # lower : the specified lower value of alter
 # upper : the specified upper value of alter
 # marg  = F : if marginal effects is difference between Pr(Upper)-Pr(Lower)
 #       = x : (where x is some number) if want to calculate marginal effect at x
 # type  = "logit" or "probit : should correspond to model type
 
 # logit
 auto.it(model=loan.lg2, alter="white", lower=0, upper=1, marg=F, type="logit")[1,]
 auto.it(loan.lg2, "white", 0, 1, F, "logit")[512,]
 # - P(upper) = P(white getting loan) ; P(lower) = P(non white getting loan)
 # - shown calculations performed when all binary variables equal to 0 or 1
 
 # probit
 auto.it(loan.pb2, "white", 0, 1, F, "probit")[1,]
 auto.it(loan.pb2, "white", 0, 1, F, "probit")[512,]
 
 # => still evidence that whites are more likely to get accepted than non-whites

# - [ Repeat for another variable e.g. married ] -
 auto.it(loan.pb2, "married", 0, 1, F, "probit")
 # => being married increases prob. of loan approval by approx. 10%
 #    under probit, though % increase depends on other circumstances
 

## PART 3 :  Marginal Effects + Likelihood Ratio Test
## ==================================================
 
# - [ Marginal Effects ] -
 
 # - when the variable of interest is binary the marginal effect of a unit increase
 #   in the variable is simply the difference between the probability when
 #   the variable is set equal to 1 vs. set equal to 0. this is given by
 #   the marginal column in the auto.it function.
 # - when the variable is continuous, the marginal effect changes slightly
 #   depending on the value we set the variable equal to. we typically use
 #   the mean.
 
# - [ Binary variable of interest ] -
 pnorm(zval.u) - pnorm(zval.l) # using probit
 # => when all other binary variables are equal to 0, the marginal effect
 #    is to increase the probability of loan acceptance by 16.6%
 
# - [ Continuous variable of interest ] -
 mm.means[bin.loc] <- c(1,0,0,0,0,0,0,0,0,0,0) # all binary variables now fixed at specific values (0 here)
 coef(loan.lg2)[-bin.loc]*dlogis(mm.means%*%coef(loan.lg2)) # logit
 coef(loan.pb2)[-bin.loc]*dnorm(mm.means%*%coef(loan.pb2)) # probit
 # -> these give the effect of a one unit increase in each variable on
 #    the probability of being accepted for a loan
 
# - [ Can use my function to calculate these as well ] -
 mm.means
 auto.it(loan.pb2, "hrat", 0, 1, 24.8000812, "probit")[1,]
 # - can change the 24.8... to calculate marginal effects at different
 #   values of hrat. we are typically interested in the value at the
 #   mean, though
 
# - [ Likelihood ratio test ] -
 
 # Can use to test for evidence of difference in probabilities across factors
 # e.g. is there statistically significant evidence of discrimination against family 
 # status (married and dep)?
 
 # H0: coeff married = 0 AND coef dep = 0
 # H1: EITHER coeff married ≠ 0 OR coeff dep ≠ 0
 
 # re-run the model without dep and marriage
 loan.pb3 <- glm(approve~white+hrat+obrat+loanprc+unem+male+ 
                 +sch+cosign+chist+pubrec+mortlat1+mortlat2+vr,
                 family=binomial(link=probit), data=subset(loan.dat,is.na(dep)==F))
 # note we have to subset the data set since some of dep values are NA and are 
 # dropped from loan.pb2, but since dep isn't in this model these values would
 # not otherwise be dropped in loan.pb3, so our samples would not be the same
 
 # generate the LR statistic and obtain the p-value
 D <- 2*(logLik(loan.pb2)[1] - logLik(loan.pb3)[1])
 pchisq(q=D, df=2, lower.tail=F)
 # => reject null : evidence of discrimination by family status
 
## PART 4 :  Goodness of fit measures
## ==================================================
 
# - [ Proportion of Outcomes Correctly Predicted ] -
 
 # OLS
 success <- ifelse(loan.lm2$fitted > 0.5, "success", "reject")
 t <- table(success, loan.lm2$model$approve)
 t/sum(t)
 # diagonals are correct, off-diagonals are incorrect
 
 # logit
 success <- ifelse(loan.lg2$fitted > 0.5, "success", "reject")
 t <- table(success, loan.lg2$model$approve)
 t/sum(t)
 
 # probit
 success <- ifelse(loan.pb2$fitted > 0.5, "success", "reject")
 t <- table(success, loan.pb2$model$approve)
 t/sum(t)
 
# - [ Psuedo-R^2 ] -
 loan.dat2 <- loan.dat[,c("approve",colnames(model.matrix(loan.pb2))[-1])]
 loan.dat2 <- loan.dat2[complete.cases(loan.dat2),] # remove all rows containing NA
 loan.lg.int <- glm(approve~1, family=binomial(link=logit), data=loan.dat2)
 loan.pb.int <- glm(approve~1, family=binomial(link=probit), data=loan.dat2)
 # Psuedo-R^2 = 1-logFull/logInt
 1- logLik(loan.lg2)[1]/logLik(loan.lg.int)[1] # logit
 1- logLik(loan.pb2)[1]/logLik(loan.pb.int)[1] # probit
 
# - [ Chisq and p-value of lht with all coefficients = 0 ] -
 linearHypothesis(loan.lg2, names(coef(loan.lg2)), white.adjust="hc0")
 linearHypothesis(loan.pb2, names(coef(loan.pb2)), white.adjust="hc0")
 
## PART 5 :  Tobit and Heckit
## ==================================================

 # See exercises 3 and 4 in:
 # http://thiloklein.de/downloads/MPhil/Lent/MPhilLabSessionLent4.pdf
 # with solutions at:
 # http://thiloklein.de/downloads/MPhil/Lent/LabSessionLent4_R.R
 # We do not have time to cover these in detail but the material
 # there should be self-explanatory.
 
 
## ===============================================