#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 3 - EXERCISE 2 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")

# - [ Useful Packages ] - 
 #install.packages("car")
 #install.packages("ellipse")
 #install.packages("AER")
 library(car) ; library(ellipse) ; library(AER)

 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 1: Try to determine whether endogeneity might play a role
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
# - [ Loading and Storing the Data ] -
 crime.dat <- read.csv("http://thiloklein.de/R/Lent/crime.csv", header=T, sep=",")
 str(crime.dat)
 # - regress crime on a constant and police
 open.dat <- read.csv("http://thiloklein.de/R/Lent/openness.csv", header=T, sep=",")
 str(open.dat)
 # - determine whether more open countries (those with higher avg share of imports in GDP)
 #   have lower inflation rates
 # - inf~open+lpcinc+oil (lpcinc = log per capita income, oil = 1 if country has oil)
 # - open~inf+lpcinc+lland+oil (lland = log of land area)
 
 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 2: Find required number of instruments
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 3: Check whether the proposed instrument are good
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - [ Check coefficients in OLS ] -
 smoke.ols2 <- lm(cigs~educ+restaurn+lcigpric+white+age+agesq, smoke.dat)
 coeftest(smoke.ols2, vcov=hc0)
 

# - [ F-test for significance ] -
 linearHypothesis(smoke.ols2, c("lcigpric=0", "restaurn=0"), vcov=hc0)
 

# - [ Examine correlations ] -
 corST <- cor(smoke.dat[-c(2,5,9)], use="complete")
 ord <- order(corST[1,])
 xc <- corST[ord, ord]
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
             "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag=T)
 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 4 : Check order condition
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 
 
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

 pchisq(q=LM, df=2-1, lower.tail=T)
 # - degrees of freedom = #instruments
 #                        MINUS #endogenous variables

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IV-2SLS Estimation Recipe
# ~~ STEP 7 : Investigate the suspected endogeneity of regressors
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# - [ Hausman Test ] -
 dwh.test <- function(model.ols, model.iv){
     cf_diff <- coef(model.iv) - coef(model.ols)
     vc_diff <- vcovHC(model.iv, "HC0") - vcovHC(model.ols, "HC0")
     x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
     pchisq(q = x2_diff, df = dim(vc_diff)[1], lower.tail = F)
 }
 dwh.test(inc.ols, inc.iv)

## ===============================================