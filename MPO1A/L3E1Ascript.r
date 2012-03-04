#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 3 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")

# - [ Useful Packages ] - 
 #install.packages("car")
 #install.packages("ellipse")
 #install.packages("AER")
 library(car) ; library(ellipse) ; library(AER)


## PART 1 : IV-2SLS Estimation Recipe (STEPS 1-2)
## =================================================

# - [ Loading and Storing the Data ] -
 smoke.dat <- read.csv("http://thiloklein.de/R/Lent/smoke.csv", header=T, sep=",")
 str(smoke.dat)

# - [ Estimate Effects of Smoking on Income ] -
 inc.ols <- lm(lincome~cigs+educ+white+age+agesq, smoke.dat)
 shccm(inc.ols)
 # -> estimates that smoking 1 additional cigarette per day increase income by 0.17% (insig)
 #    => should be suspicious of this finding!

# - [ Estimate Effects of Income on Smoking ] -
 smoke.ols <- lm(cigs~lincome+educ+restaurn+lcigpric+white+age+agesq, smoke.dat)
 shccm(smoke.ols)
 # -> estimates 1% additional income increases cigarette consumption by 0.87 per day (insig)
 # -> restaurant smoking ban reduces cigarette consumption, as expected
 # -> however, 1% increase in cigarette price decreases consumption by 0.85 per day (insig)
 #    => should be suspicious of this finding!

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 1: Try to determine whether endogeneity might play a role :
#   - based on above, perhaps income and cigarette consumption are JOINTLY determined
#     - smoking more => have more disposable income
#     - having more disposable income => can smoke more
# ~~ STEP 2: Find required number of instruments :
#    - have one endogenous regressor (cigs) => need at least one instrument
#      - one possible instrument is restaurn
#      - another is cigarette price
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## PART 2 : IV-2SLS Estimation Recipe (STEPS 3-4)
## =================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 3: Check whether the proposed instrument are good :
#    - good instruments should be sufficiently correlated with the regressors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - [ Check coefficients in OLS ] -
 smoke.ols2 <- lm(cigs~educ+restaurn+lcigpric+white+age+agesq, smoke.dat) # drop lincome
 coeftest(smoke.ols2, vcov=hc0)
 # -> restaurn highly significant (indicates good instrument)
 # -> lcigprice highly insignificant (indicates bad instrument)

# - [ F-test for significance ] -
 linearHypothesis(smoke.ols2, c("lcigpric=0", "restaurn=0"), vcov=hc0)
 # -> reject null that reduced model better
 #    => lcigprice and restaurn have significant explanatory power for cigs

# - [ Examine correlations ] -
 corST <- cor(smoke.dat[-c(2,5,9)], use="complete")
 ord <- order(corST[1,])
 xc <- corST[ord, ord]
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
             "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag=T)
 # -> restaurn weakly correlated with cigs (ok), weakly correlated with lincome (not so good)
 # -> lcigprice weakly correlated with cigs (ok), not correlated with lincome (good)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 3 (cont.)
#    - resaurn may be a good instrument, lcigprice not very good
#      - keep both in for now
# ~~ STEP 4 : Check order condition
#    - we have 1 good instrument = # endogenous variables (cigs)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## PART 3 : IV-2SLS Estimation Recipe (STEP 5)
## =================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 5 : Apply 2SLS method and compare results with OLS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - [ Fit a 2SLS model ] -
 inc.iv <- ivreg(lincome~cigs+educ+white+age+agesq 
                 | educ+restaurn+lcigpric+white+age+agesq, data=smoke.dat)
 # - model before | is 2nd stage (i.e. main model)
 # - model after | is list of regressors in 1st stage
 #   - NOTE : regressors in 1st stage = regressors in 2nd stage
 #               MINUS endogenous regressors in 2nd stage
 #               PLUS instruments for 1st stage

# - [ Fit a 2SLS model ] -
 coeftest(inc.iv, vcov=hc0)
 # -> note that the coef of cigs is now negative (as expected) and significant at 10% level
 # -> estimated effect is large : 1 additional cigarette lowers predicted income by 3.9%

# - [ Compare new to original model ] -
 ivolsdiff <- function(ols,iv) {
    ols.summ <- coeftest(ols,vcov=hc0)
    iv.summ <- coeftest(iv,vcov=hc0)
    nvars <- nrow(ols.summ) ; output <- matrix(NA,nrow=nvars,ncol=3)
    rownames(output) <- rownames(ols.summ)
    colnames(output) <- c("coeff (iv-ols)","s.e. (iv-ols)","p-val")
    output[,1] <- iv.summ[,1]-ols.summ[,1]
    output[,2] <- 100*(iv.summ[,2]-ols.summ[,2])/ols.summ[,2]
    for (i in 1:nvars) {
        tmp <- paste(rownames(ols.summ)[i],"=",sep="") ; tmp <- paste(tmp,ols.summ[i,1],sep="")
        output[i,3] <- linearHypothesis(iv, c(tmp), vcov=hc0)$Pr[2]
    }
    output
 }
 ivolsdiff(inc.ols,inc.iv)
 # - coeff gives difference between coefficients expressed as coeff(IV)-coeff(OLS)
 # - s.e. gives % increase in s.e. of IV from OLS
 # - p-val gives the p-value of a hypothesis test that the coefficients in both models are equal


## PART 4 : IV-2SLS Estimation Recipe (STEPS 6-7)
## =================================================

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 6 : Check whether the proposed instruments are indeed exogenous
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - [ Sargan Test ] -
 aux.lm <- lm(inc.iv$res~educ+restaurn+lcigpric+white+age+agesq, data=smoke.dat)
 # - regress 2SLS variables on all variables from 1st stage

 R2 <- summary(aux.lm)$r.squared
 LM <- length(aux.lm$res) * R2
 # - calculate LM statistic

 pchisq(q=LM, df=2-1, lower.tail=FALSE)
 # - degrees of freedom = #instruments (2 here : resaurn and lcigprice) 
 #                        MINUS #endogenous variables (1 here: cigs)
 # => do NOT reject the null that the instruments are valid

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 7 : Investigate the suspected endogeneity of regressors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - [ Hausman Test ] -
 dwh.test <- function(model.ols, model.iv){
     cf_diff <- coef(model.iv) - coef(model.ols)
     vc_diff <- vcovHC(model.iv, "HC0") - vcovHC(model.ols, "HC0")
     x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
     pchisq(q = x2_diff, df = dim(vc_diff)[1], lower.tail = FALSE)
 }
 dwh.test(inc.ols, inc.iv)
 # => do NOT reject the null of exogeneity


 # And that's all there is to it!
 # NOTE :  Our final model included some insignificant variables we might
 #         like to try and remove, but we already saw how to do that last
 #         term. Also, we of course should check the other least square
 #         assumptions.


## ===============================================