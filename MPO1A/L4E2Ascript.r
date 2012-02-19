#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 4 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

# - [ Header ] - 
 rm(list=ls()) ; par(mfrow=c(1,1))

 # - [ Useful Packages ] - 
 #install.packages("RCurl")
 library(AER) ; library(plm)
 
 # - [ Custom Functions ] -
 source("http://www.thiloklein.de/R/myfunctions.R")
 source_https <- function(url, ...) {
    require(RCurl)
    sapply(c(url, ...), function(u) {
        eval(parse(text = getURL(u,followlocation=TRUE,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))),envir=.GlobalEnv)
    })
 }
 source_https("https://raw.github.com/mfrmn/qrm/master/mfrmnFns.r")

 
## PART 1 : Examining Panel Data
## =================================================
 
# - [ Loading and Storing Panel Data ] -
 wage.dat <- read.csv("http://thiloklein.de/R/Lent/wagepan.csv",header=T,sep=",")
 str(wage.dat)
 wage.pdat <- pdata.frame(wage.dat, c("nr","year"))
 # - replace nr by individual index variable
 # - replace year by time index variable
 head(wage.pdat)
 
# - [ Calculate summary statistics for variables of interest ] -
 wage.dat$nr <- as.factor(wage.dat$nr)
 wage.dat$year <- as.factor(wage.dat$year)
 wage.sub <- subset(wage.dat, select=c("lwage","educ","black","hisp","exper","married","union"))
 for(i in 1:7){
     m <- mean(wage.sub[,i])
     s.o <- sd(wage.sub[,i])    								                        # overall
     s.b <- sd(by(wage.sub[,i], wage.dat$nr, mean))						                # between
     s.w <- sd( unlist( by( wage.sub[,i], wage.dat$nr, function(x) x - mean(x) ) ) )    # within
     cat("------------------------------------------ \n")
     cat("Variable:", names(wage.sub)[i], "\n")
     print(round( data.frame(Mean=m, SE.overall=s.o, SE.between=s.b, SE.within=s.w), 3))
     cat("\n")
 }
 # SE.overall = pooled standard error
 # SE.between = standard error between individuals
 # SE.within = standard error within individuals
 # Check understanding : Why are some SE.within = 0 ?
 
 
## PART 2 : Pooled OLS
## =================================================
 
# - [ Fitting a pooled OLS model ] -
 wage.pool <- plm(lwage~year+educ+black+hisp+exper+married+union, data=wage.pdat,
                  model="pooling", effect="individual")
 coeftest(wage.pool, vcov=pvcovHC(wage.pool, method="arellano"))
 # - returns correct hetero / series correlation corrected standard errors
 #   for panel data
 
# - [ Compare to normal procedure ] -
 wage.lm <- lm(lwage~year+educ+black+hisp+exper+married+union, data=wage.pdat)
 coef(wage.lm) ; coef(wage.pool) # coefficients are identical
 shccm(wage.lm) # note that the SEs here are different

# - [ Why do we need serial correlation corrected SEs?  ] -
 # In order to get an indication whether the errors are correlated over time,
 # we will look at the correlations of the residuals over time.
 acf(wage.lm$res)
 # - acf at 0 will always = 1 here
 # => highly correlated over time
 
 
## PART 3 : Within-Group Regression
## =================================================
 
# - [ Fixed-Effects (time-demeaning) ] -
 wage.fe <- plm(lwage~-educ+black+hisp+exper+married+union, data=wage.pdat,
                model="within", effect="individual") # set model="within"
 acf(wage.fe$res)
 coeftest(wage.fe, vcov=pvcovHC(wage.fe, method="arellano"))
 # - use corrected SEs again
 # - notice that variables that do not vary over time are dropped
 
 # - [ Least Squares Dummy Variable ] -
 wage.lsdv <- lm(lwage~-1+educ+black+hisp+exper+married+union+nr, data=wage.pdat)
 acf(wage.lsdv$res)
 coeftest(wage.lsdv)[1:6,]
 # produced identical estimates of married, union
 
 # - [ First differencing ] -
 wage.fd <- plm(lwage~-educ+black+hisp+exper+married+union, data=wage.pdat,
                model="fd", effect="individual") # set model="fd"
 acf(wage.fd$res)
 coeftest(wage.fd)
 # Note: effect of exper is lost due to first differencing
 #       as it increases by 1 each year and so is a constant

 
## PART 4 : Between-Group Regression
## =================================================
 
# - [ Random-Effects ] -
 wage.re <- plm(lwage~educ+black+hisp+exper+married+union, data=wage.pdat,
                model="random", effect="individual") # set model="random"
 acf(wage.re$res)
 coeftest(wage.re, vcov=pvcovHC(wage.re, method="white2"))
 # - use corrected SEs again
 
 
## PART 5 : Model Diagnostics
## =================================================
 
 coef(wage.fe) ; coef(wage.re)[5:7]
 # notice that married and union are quite different!
 
# - [ Hausman Test ] -
 phtest(wage.fe, wage.re)
 # we reject the null and conclude that RE is likely biased and
 # so should favour FE
 
# SUPPOSE WE HAD FOUND THAT RE WAS APPROPRIATE. WE WOULD THEN
# PERFORM THE FOLLOWING TEST:
 
# - [ Breusch-Pagan Lagrange Multiplier for RE ] -
 # null is no panel effect (i.e. OLS better).
 plmtest(wage.pool, type="bp")
 # we would reject the null and conclude that RE is appropriate
 # however, since rejected null in Hausman test we conclude we should use FE

# - [ Suggested further reading ] -
 # http://dss.princeton.edu/training/Panel101R.pdf
 
## ===============================================