#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 1 - EXERCISE 2 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")

# - [ Useful Packages ] - 
 #install.packages("TSA")
 #install.packages("dynlm")
 #install.packages("rugarch")
 library(TSA) ; library(dynlm) ; #library(rugarch)
 
 
## PART 1 : ADF Test for Stationarity
## =================================================
 
 # For stochastic non-stationary processes (e.g. the airmiles data set)
 # we can try to perform stationary transformations. We can then test
 # for stationarity using the Augmented Dickey-Fuller test.
 
 data(airmiles)
 air.ts <- ts(as.numeric(airmiles), start=c(1996,1), frequency=12)
 lair.ts <- log(air.ts)
 
 ## --- A procedure to test for unit roots ---
 # Walter Enders (2004) Applied Econometric time series, page 213 ff
 # Step 1: Start with trend and drift model (least restrictive)
 # Step 2: If null in step 1 is NOT rejected, check
    # were too many deterministic regressors included in step 1?
    # we should test for significance of the trend term by joint hypothesis
 # Step 3: If null in step 2 is not rejected, estimate model without trend
 # Step 4: If null in step 3 is NOT rejected, check
    # were too many deterministic regressors included in step 3?
    # we should test for joint significance of constant and regressor by joint hypothesis
 # Step 5: If null in step 4 is not rejected, estimate a model without drift and trend
 
# - [ Step 0 : Approximate appropriate number of lags ] -
 ar(lair.ts, method="mle")
 # -> should always choose more than the cycle length (e.g. 12 months here)
 
# - [ Step 1 : Run ADF Test allowing for Intercept and Trend ] -
 adf.test.1(lair.ts, int=T, trend=T, k=14)
 # - if stationary, STOP
 # - else, continue to Step 2
 
# - [ Step 2 : Test whether can drop the trend term ] -
 lmA <- adf.test.2(lair.ts, int=T, trend=T, k=14)
 linearHypothesis.adf(lmA, c("xD.lag(x, -1)", "xtime(x)"))
 # - if p-value large, can drop trend term in Step 3
 # - else, STOP, there is a unit root and the process is NOT stationary

# - [ Step 3 : Run ADF Test dropping Trend term ] -
 adf.test.1(lair.ts, int=T, trend=F, k=14)
 # - if stationary, STOP
 # - else, continue to Step 4
 
# - [ Step 4 : Test whether can drop the intercept term ] -
 lmA <- adf.test.2(lair.ts, int=T, trend=F, k=14)
 linearHypothesis.adf(lmA, c("(Intercept)", "xlag(x, -1)"))
 # - if p-value large, can drop intercept term in Step 5
 # - else, STOP, there is a unit root and the process is NOT stationary
 
# - [ Step 5 : Run ADF Test dropping Trend and Intercept terms ] -
 adf.test.1(lair.ts, int=F, trend=F, k=14)
 # - if stationary, STOP
 # - else, conclude NOT stationary
 
# - [ Repeat above procedure after taking 1st difference of TS ] -
 lair.ts.d1 <- diff(lair.ts)
 plot(stl(lair.ts.d1, "periodic"))
 adf.test.1(lair.ts.d1, int=T, trend=T, k=14)
 lmA <- adf.test.2(lair.ts.d1, int=T, trend=T, k=14)
 linearHypothesis.adf(lmA, c("xD.lag(x, -1)", "xtime(x)"))
 adf.test.1(lair.ts.d1, int=T, trend=F, k=14)
 lmA <- adf.test.2(lair.ts.d1, int=T, trend=F, k=14)
 linearHypothesis.adf(lmA, c("(Intercept)", "xlag(x, -1)"))
 adf.test.1(lair.ts.d1, int=F, trend=F, k=14)

# - [ Should test for remaining autocorrelation in the residuals ] -
 par(mfrow=c(2,1)); acf(lmA$resid); pacf(lmA$resid)
 ljung.box.test.1(lmA$resid, seq(1,30,1))
 # -> if suspicion of autocorrelation then should increase order of lags in ADF test
 
# - [ Could also try seasonal differencing (i.e. differencing at 12 month lag) ] -
 lair.ts.d1.d12 <- diff(lair.ts.d1,lag=12)
 plot(stl(lair.ts.d1.d12, "periodic"))
 adf.test.1(lair.ts.d1.d12, int=T, trend=T, k=14)
 
 
## PART 2 : The Distributed Lag Model
## =================================================

 par(mfrow=c(1,1))
 
 # Can be used when estimating a stationary time series as a function of lagged values
 # of one or more other stationary time series.
 
 bonds.dat <- read.csv("http://www.frmn.me/teaching/mpo1a/labs/L1E2A.csv")
 str(bonds.dat)
 # payems : Total Nonfarm Payrolls: All Employees
 # baa : Moody's Seasoned Baa Corporate Bond Yield
 # gs10 : 10-Year Treasury Constant Maturity Rate
 # cpiaucsl : Consumer Price Index for All Urban Consumers: All Items - SA
 # tb3ms : 3-Month Treasury Bill: Secondary Market Rate
 # unrate : Civilian Unemployment Rate
 
 # We want to to predict employment growth using the difference between
 # baa and gs10 (yield on corporate bonds - yield on 10yr maturity treasury bonds)
 
# - [ Generating the time series of interest ] -
 baa_r10 <- ts(bonds.dat$baa - bonds.dat$gs10, start=c(1953,5), frequency=12); baa_r10
 # -> observe this ends in an NA, we will reduce the length of our time series by one period to deal with this
 lun.d1 <- ts(diff(log(bonds.dat$unrate)), start=c(1953,6), frequency=12)
 # -> notice this starts one month later since we have taken first difference
 plot(lun.d1)
 # -> things are messy before 1965, lets drop that data
 yld.ts <- ts.union(lun.d1, baa_r10)
 yld65.ts <- window(yld.ts, start=c(1965,1), end=c(2011,11))
 colnames(yld65.ts) <- c("lun","br10")
 plot(yld65.ts)
 
# - [ ADF Test for Stationarity ] -
 adf.test.1(yld65.ts[,1], int=T, trend=T, k=14)
 adf.test.1(yld65.ts[,2], int=T, trend=T, k=14)
 # -> Both series are stationary
 
# - [ Fit a basic linear model ] -
 yld.lm <- lm(lun~br10, yld65.ts)
 shaccm(yld.lm)
 plot(yld65.ts[,1], type="l") ; lines(ts(fitted(yld.lm), start=c(1965,1), frequency=12), col="red")
 acf(rstudent(yld.lm))
 # -> notice the autocorrelation in the residuals
 #    previous months errors are correlated with this months error
 # => might like to try a model that includes the historic spread of corporate/T-bonds
 
# - [ Fit a distributed lag model ] -
 yld.dlm <- dynlm(lun~L(br10,1:12), yld65.ts) # includes 1-12 months of lags, lets exclude current month so we can forecast
 shaccm(yld.dlm)
 # lags 1, 9, 12 appear to have some effect
 # the coefficient is known as the multiplier at lag j
 
# - [ Plot the dynamic multipliers and 95% confidence intervals ] -
 plot(1:12, yld.dlm$coef[2:13], type="l", col="blue", ylim=c(-0.04,0.04), xlab="Lag", ylab="Dynamic Multiplier")
 abline(h=0)
 
# - [ Fitting standard (i.e. not hetero and autocorr corrected) 95% confidence intervals ] -
 conf <- summary(yld.dlm)$coef[2:13,2]*qnorm(p=0.975)
 lines(1:12, yld.dlm$coef[2:13]+conf, col="green")
 lines(1:12, yld.dlm$coef[2:13]-conf, col="green")
 
# - [ Fitting hetero and autocorr corrected 95% confidence intervals ] -
 conf2 <- sqrt(diag(vcovHAC(yld.dlm)))*qnorm(p=0.975)
 lines(1:12, yld.dlm$coef[2:13]+conf2[2:13], col="red")
 lines(1:12, yld.dlm$coef[2:13]-conf2[2:13], col="red")
 legend("topright",legend=c("Dynamic multiplier","95% confidence band","95% confidence band with HAC errors"),fill=c("blue","green","red"),bty="n")
 
# - [ Reducing the distributed lag model ] -
 yld.dlm2 <- dynlm(lun~L(br10,c(1,3,12)), yld65.ts) # includes 2 and 12 month lags
 shaccm(yld.dlm2)
 # -> where do I get the 3 month lag from? guess work..
 plot(yld65.ts[,1], type="l") ; lines(ts(fitted(yld.dlm2), start=c(1965,1), frequency=12), col="red")
 acf(rstudent(yld.dlm2))
 
# - [ Joint F-test of Dropped Lags ] -
 anova(yld.dlm2, yld.dlm)
 # => Do not reject null, reduced model provides better fit.
 
 
## PART 3 : The Autoregressive Distributed Lag Model
## =================================================

 # Can be used when estimating a stationary time series as a function of lagged values
 # of one or more other stationary time series AND itself.
 
# - [ Fit an ADLM ] -
 yld.dlm3 <- dynlm(lun~L(lun,1:4)+L(br10,1:4), yld65.ts)
 shaccm(yld.dlm3)
 plot(yld65.ts[,1], type="l") ; lines(ts(fitted(yld.dlm3), start=c(1965,1), frequency=12), col="red")
 acf(rstudent(yld.dlm3))
 
# - [ Reducing the ADLM ] -
 yld.dlm4 <- dynlm(lun~L(lun,2:4)+L(br10,c(1,4)), yld65.ts)
 shaccm(yld.dlm4)
 
# - [ Joint F-test of Dropped Lags ] -
 anova(yld.dlm4, yld.dlm3)
 # => Do not reject null, reduced model provides better fit.

 
## PART 4 : Model Selection and Forecasting
## =================================================
 
# - [ Adjusted R^2 ] -
 summary(yld.dlm)$adj.r
 summary(yld.dlm2)$adj.r
 summary(yld.dlm3)$adj.r
 summary(yld.dlm4)$adj.r
 # -> higher is better!
 
# - [ Bayesian Information Criteria ] -
 BIC(yld.dlm)
 BIC(yld.dlm2)
 BIC(yld.dlm3)
 BIC(yld.dlm4)
 # -> lower is better!
 
# - [ 1 step-ahead forecast using DLM ] -
 # Lets forecast employment growth for December 2011
 actual <- yld.ts[704,1] # actual value of employment growth
 forecast.2 <- sum(c(1,yld65.ts[c(563,561,552),2])*coefficients(yld.dlm2))
 c(actual, forecast.2)
 # -> multiply actual values for Nov2011, Sep2011, Dec2010 by coefficients of DLM2
 
# - [ 1 step-ahead forecast using ADLM ] -
 forecast.4 <- sum(c(1, yld65.ts[c(562,561,560),1], yld65.ts[c(563,560),2])*coefficients(yld.dlm4))
 c(actual, forecast.4)

# - [ Difference between actual value and forecasts] -
 abs(actual-c(forecast.2,forecast.4))
 # -> lower is better!
 

## EXTRA : The Distributed Lag Model (OJ Example)
## =================================================
 
 # This is the frozen orange juice example again.
 # I have annotated it but we will not cover it in the lab session.
 
 oj.dat <- read.csv("http://www.thiloklein.de/R/Lent/oj.csv",header=T,sep=",")
 oj.ts <- ts.union(as.ts(oj.dat$frz[-1]), as.ts(diff(log(oj.dat$ojfro),lag=1)))
 colnames(oj.ts) <- c("frz","cojfro")
 plot.ts(oj.ts)
 plot(stl(ts(oj.ts[-1,2],start=c(1),frequency=12),"periodic"))
 
# - [ ADF Test for Stationarity ] -
 adf.test.1(oj.ts[,1], int=T, trend=T, k=14)
 adf.test.1(oj.ts[,2], int=T, trend=T, k=14)
 # -> Both series are stationary
 
# - [ Fit a basic linear model ] -
 oj.lm <- lm(cojfro~frz, oj.ts)
 shaccm(oj.lm)
 plot(oj.ts[,2], type="l") ; lines(ts(fitted(oj.lm)), col="red")
 acf(rstudent(oj.lm))
 # -> notice the autocorrelation in the residuals
 #    last months error is correlated with this months, as is last years
 # => might like to try a model that includes the #frozen days last month
 #    and last year
 
# - [ Fit a distributed lag model ] -
 oj.dlm <- dynlm(cojfro~L(frz,0:18), oj.ts) # includes 18 months of lags
 shaccm(oj.dlm)
 # lags 0, 1, 12, 13, 14 appear to have some effect
 # the coefficient is known as the multiplier at lag j
 
# - [ Plot the multipliers and 95% confidence intervals ] -
 plot(0:18, oj.dlm$coef[2:20], type="l", col="blue", ylim=c(-0.003,0.006), xlab="Lag", ylab="Dynamic Multiplier")
 abline(h=0)
 
# - [ Fitting standard (i.e. not hetero and autocorr corrected) 95% confidence intervals ] -
 conf <- summary(oj.dlm)$coef[2:20,2]*qnorm(p=0.975)
 lines(0:18, oj.dlm$coef[2:20]+conf, col="green")
 lines(0:18, oj.dlm$coef[2:20]-conf, col="green")
 
# - [ Fitting hetero and autocorr corrected 95% confidence intervals ] -
 conf2 <- sqrt(diag(vcovHAC(oj.dlm)))*qnorm(p=0.975)
 lines(0:18, oj.dlm$coef[2:20]+conf2[2:20], col="red")
 lines(0:18, oj.dlm$coef[2:20]-conf2[2:20], col="red")
 legend("topright",legend=c("Dynamic multiplier","95% confidence band","95% confidence band with HAC errors"),fill=c("blue","green","red"),bty="n")
 
# - [ Reducing the distributed lag model ] -
 oj.dlm2 <- dynlm(cojfro~L(frz,c(0,1,12,13)), oj.ts)
 shaccm(oj.dlm2)
 plot(oj.ts[,2], type="l") ; lines(ts(fitted(oj.dlm2)), col="red")
 acf(rstudent(oj.dlm2))
 
 # - [ Joint F-test of Dropped Lags ] -
 f.test.lm <- function(mod.R, mod.F) {
     # mod.R is reduced model ; mod.F is full model
     # Null : Reduced model does provides a significantly better fit than Full model
     RSS.R <- sum(mod.R$res^2) ; RSS.F <- sum(mod.F$res^2)
     df.R <- mod.R$df ; df.F <- mod.F$df
     F <- ((RSS.R-RSS.F)/(df.R-df.F)) / (RSS.F/df.F)
     p.val <- 1 - pf(F, df.R-df.F, df.F)
     list(F=F,p.value=p.val)
 }
 f.test.lm(oj.dlm2, oj.dlm)
 # => Do not reject null, reduced model provides better fit.

# - [ Calculating the impact multiplier ] -
 # This reflects the immediate change in Y
 coefficients(oj.dlm)[2]
 coefficients(oj.dlm2)[2]
 # => 1 additional freezing day increasing OJ prives by about 0.5% immediately
 
# - [ Calculating the long-run multiplier ] -
 sum(coefficients(oj.dlm)[2:length(coefficients(oj.dlm))])
 sum(coefficients(oj.dlm2)[2:length(coefficients(oj.dlm2))])
 # => 1 additional freezing day increases OJ prices by approx. 0.4% in the long run 
 
# - [ Although no reason to suspect seasonality, we could also add month factors ] -
 n <- length(oj.ts[,1])
 month <- rep(1:12,ceiling(n/12))[1:n]
 oj.ts2 <- ts.union(oj.ts, as.ts(month))
 colnames(oj.ts2) <- c("frz","cojfro","month")
 oj.dlm.3 <- dynlm(cojfro~-1+L(frz,c(0,1,12,13))+as.factor(month), oj.ts2)
 shaccm(oj.dlm.3)
 acf(rstudent(oj.dlm.3), na.action=na.pass, lag.max=30)
 
## ======================== 