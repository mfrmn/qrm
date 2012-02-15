#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 2 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")

# - [ Useful Packages ] - 
 #install.packages("TSA")
 #install.packages("forecast")
 #install.packages("dynlm")
 #install.packages("FinTS")
 library(TSA) ; library(dynlm) ; library(forecast) ; library(FinTS)

## PRELUDE : Box-Jenkins Procedure
## =================================================

# [1.] Is the time series stationary?
#       i) plot the data
#       ii) compute ACF and PACF
#       iii) perform ADF tests
#
# [2.] Is the series AR, MA, ARIMA, SARIMA.
#       i) plot ACF and PACF
#
# [3.] Estimate the model, defining p and q (guided by PACF and ACF)
#
# [4.] Verify that the ACF and PACF of residuals do not show
#      significant spikes after estimation
#
# [5.] If there are two or more competing models, use AIC or SIBC to decide
#      which is better. The lower AIC and/or SBIC the better the model
#
# [6.] In the final specification all parameters must be significant,
#      and residuals wel behaved:
#       i) No autocorrelation?
#       ii) No heteroskedasticity?
#       iii) Normally distributed?


## PART 1 : Fitting an ARIMA Model
## =================================================

# - [ Loading and Storing Time Series Data ] -
 quart.dat <- read.csv("http://thiloklein.de/R/Lent/quarterly.csv", header=T, sep=",")
 ppi.ts <- ts(quart.dat$PPI, start=c(1960,1), freq=4)
 lppi.ts <- log(ppi.ts)
 lppi.d1 <- diff(lppi.ts)
 plot(ts.union(ppi.ts,lppi.d1))
 plot(stl(lppi.d1, "periodic"))

# - [ Examine the ACF and PACF plots ] -
 par(mfrow=c(2,1))
 acf(lppi.d1) ; pacf(lppi.d1)
 # -> both decay quickly which suggests stationary series

# - [ Formal Test for Stationarity ] -
 # Approximate suitable number of lags
 ar(lppi.d1, method="mle")$order
 adf.test.1(lppi.d1, int=T, trend=T, k=8) # border-line : lets try dropping trend
 lmA <- adf.test.2(lppi.d1, int=T, trend=T, k=8)
 linearHypothesis.adf(lmA, c("xD.lag(x, -1)", "xtime(x)"))
 adf.test.1(lppi.d1, int=T, trend=F, k=8)
 
# - [ Should test for remaining autocorrelation in the residuals ] -
 acf(lmA$resid); pacf(lmA$resid)
 ljung.box.test.1(lmA$resid, seq(1,15,1))
 # -> if suspicion of autocorrelation then should increase order of lags in ADF test

# - [ Use ACF/PACF to determine whether AR/MA and appropraite order ] - 
# An AR process will have 
#    i)  geometrically decaying ACF
#    ii) # spikes of PACF = AR order
# An MAC process will have 
#    i)  # spikes of ACF = MA order
#    ii) geometrically decaying PACF
 acf(lppi.d1) ; pacf(lppi.d1)
 # -> ACF could be decaying... spike at 1 in PACF so perhaps AR(1)
 # -> ACF could have 4 spikes... spike at 1 in PACF so perhaps ARMA(1,4)
 # -> ..or some combination/alternatives : this is NOT an exact science
 
# - [ Fit ARIMA models to reduced time series ] -
 lppi.red <- window(lppi.ts, end=c(2001,1))
 arima10 <- arima(lppi.red, order=c(1,1,0)) ; coeftest(arima10)
 arima11 <- arima(lppi.red, order=c(1,1,1)) ; coeftest(arima11)
 arima22 <- arima(lppi.red, order=c(2,1,2)) ; coeftest(arima22)
 arima14 <- arima(lppi.red, order=c(1,1,4)) ; coeftest(arima14)
 arima44 <- arima(lppi.red, order=c(4,1,4)) ; coeftest(arima44)
 arima1.14 <- arima(lppi.red, order=c(1,1,4), fixed=c(NA, NA, 0, 0, NA)) ; coeftest(arima1.14)


## PART 2 : Fitting an SARIMA Model
## ================================================= 

 # The fact that we are getting significance at the 4th MA term in the above models
 # and the data is quarterly suggests an alternative approach might be to consider
 # a seasonal ARIMA model
 
 # Notice that in the plot of the acf for the first differenced time series there is
 # a small increase at lag 4.
 par(mfrow=c(2,1))
 acf(lppi.d1) ; pacf(lppi.d1)
 
 # Since the 1st differenced time series is stationary, we may not want to try seasonal
 # differencing as this would reduce the number of observations by the season length.
 # If, however, we think it might produce a better model we would proceed as follows:
 lppi.d1.d4 <- diff(lppi.d1,lag=4)
 par(mfrow=c(1,1)); plot(lppi.d1.d4)
 adf.test.1(lppi.d1.d4, int=T, trend=T, k=10)
 lmA <- adf.test.2(lppi.d1.d4, int=T, trend=T, k=10)
 par(mfrow=c(2,1)); acf(lmA$resid); pacf(lmA$resid)
 ljung.box.test.1(lmA$resid, seq(1,20,1))
 # -> this series is stationary
 
# - [ Fit SARIMA models to reduced time series ] -
 sarima1 <- arima(lppi.red, order=c(1,1,0), seasonal=list(order=c(0,0,1), period=4)) ; coeftest(sarima1)
 sarima2 <- arima(lppi.red, order=c(1,1,1), seasonal=list(order=c(0,0,1), period=4)) ; coeftest(sarima2)
 sarima3 <- arima(lppi.red, order=c(1,1,1), seasonal=list(order=c(0,0,2), period=4)) ; coeftest(sarima3)
 sarima4 <- arima(lppi.red, order=c(2,1,2), seasonal=list(order=c(0,0,2), period=4)) ; coeftest(sarima4)
 sarima5 <- arima(lppi.red, order=c(1,1,1), seasonal=list(order=c(1,0,2), period=4)) ; coeftest(sarima5)
 sarima6 <- arima(lppi.red, order=c(0,1,4), seasonal=list(order=c(0,0,2), period=4)) ; coeftest(sarima6)
 
 
## PART 3 : ARIMA model selection
## =================================================
 
# - [ Stat generating model selection function ] -
 arima.select <- function(model) {
     output <- rep(NA, 5)
     names(output) <- c("AIC","SSR","Q(5)","Q(10)","Q(15)")
     output[1] <- model$aic
     output[2] <- sum(model$res^2, na.rm=T)
     tmp <- model$res ; nNA <- is.na(tmp)==F
     output[3:5] <- ljung.box.test.1(tmp[nNA],seq(5,15,5))[,1]
     output
 }
 
# - [ Compare to auto.arima ] -
 auto.arima(lppi.red, d=1, D=0, max.P=0, max.Q=0, allowdrift=FALSE) # without seasonal element
 auto.arima(lppi.red, d=1, D=0, allowdrift=FALSE) # allowing seasonal element
 # -> be warned that auto.arima does not let you to fix AR or MA terms like arima
 
# - [ Calculate AIC value for an example model ] -
 arima10$aic
 
# - [ Calculate sum square of residuals for an example model ] -
 sum(arima10$res^2, na.rm=T)

# - [ Calculate the ljung-box test statistic at lags 5, 10, 15 for an example model ] -
 tmp <- arima10$res
 nNA <- is.na(tmp)==F
 ljung.box.test.1(tmp[nNA],seq(5,15,5))[,1]
 
# - [ Now use function above to automate ] -
 select.mat <- matrix(NA, nrow=12, ncol=5)
 colnames(select.mat) <- c("AIC","SSR","Q(5)","Q(10)","Q(15)")
 
 select.mat[1,] <- arima.select(arima10)
 select.mat[2,] <- arima.select(arima11)
 select.mat[3,] <- arima.select(arima22)
 select.mat[4,] <- arima.select(arima14)
 select.mat[5,] <- arima.select(arima44)
 select.mat[6,] <- arima.select(arima1.14)
 select.mat[7,] <- arima.select(sarima1)
 select.mat[8,] <- arima.select(sarima2)
 select.mat[9,] <- arima.select(sarima3)
 select.mat[10,] <- arima.select(sarima4)
 select.mat[11,] <- arima.select(sarima5)
 select.mat[12,] <- arima.select(sarima6)
 select.mat

 # Low values of AIC, SSR, and Q(5), Q(10), Q(15) are better
 # Notice model 4.4 is overfitted so will of course have lower SSR and ljung.box
 # Models 1.4 and 4.1/4 perform best out of the remaining arima models
 # The best seasonal model, sarima2, is very similar to the best non-seasonal

 
## PART 4 : ARIMA model diagnostics
## =================================================

# - [ Test coefficients for significance ] - 
 coeftest(arima1.14)
 
# - [ Test residuals for independence/autocorrelation ] -
 main <- arima1.14
 par(mfrow=c(1,2)) ; acf(main$resid) ; pacf(main$resid)
 nNA <- is.na(main$res) == F
 ljung.box.test.1(tmp[nNA],seq(1,15,1))[,1]
 
# - [ Test residuals for normality ] -
 par(mfrow=c(1,1))
 hist(rstandard(main), breaks=10, probability=T)
 grid.x <- seq(min(rstandard(main)), max(rstandard(main)), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(rstandard(main),na.rm=T))
 lines(grid.x,grid.y)
 jarque.bera.test(main$res[nNA])
 
# - [ Test residuals for heteroskedasticity ] -
 plot(main$res) # look for signs of change in variance
 par(mfrow=c(2,1)); acf(main$res^2); pacf(main$res^2) # should be approx. 0
 ArchTest(c(main$res), lags=5)
 
# - [ Good to run ARCH test for different values of lags ] -
 arch.check <- function(model,maxlag=40) {
     chklags <- c(1:10,seq(15,min(maxlag, length(model$residuals)), by=5))
     output <- matrix(NA, nrow=length(chklags), ncol=3)
     colnames(output) <- c("lags","statistic","p-value")
     for (i in 1:length(chklags)) {
         archout <- ArchTest(c(model$res), lags=chklags[i])
         output[i,] <- c(chklags[i],archout$statistic,archout$p.value)
     }
     output
 }
 arch.check(arima1.14)
 

## PART 5 : Forecasting with ARIMA models
## =================================================
 
# - [ Generate forecasts for the observations we dropped ] -
 sarima3.pred <- predict(sarima2, n.ahead=4) ; sarima3.pred
 arima1.14.pred <- predict(arima1.14, n.ahead=4) ; arima1.14.pred

# - [ Convert back to the original scale ] -
 predS3.vec <- exp(sarima3.pred$pred+0.5*sarima3.pred$se^2)
 pred1.14.vec <- exp(arima1.14.pred$pred+0.5*arima1.14.pred$se^2)
 # -> Be very careful, the method described here works only when
 #    having taken logs. Notice the extra 0.5*sarima3.pred$se^2.

# - [ Calculate 95% bounds ] -
 predS3.l.vec <- exp(sarima3.pred$pred-qnorm(0.95)*sarima3.pred$se)
 predS3.u.vec <- exp(sarima3.pred$pred+qnorm(0.95)*sarima3.pred$se)
 
 pred1.14.l.vec <- exp(arima1.14.pred$pred-qnorm(0.95)*arima1.14.pred$se)
 pred1.14.u.vec <- exp(arima1.14.pred$pred+qnorm(0.95)*arima1.14.pred$se)

# - [ Examine the forecasts ] -
 cbind(predS3.l.vec,predS3.vec, predS3.u.vec)
 cbind(pred1.14.l.vec,pred1.14.vec, pred1.14.u.vec)

# - [ Compare forecasts to actual ] -
 accuracy(predS3.vec,window(ppi.ts,start=c(2001,2)))
 accuracy(pred1.14.vec,window(ppi.ts,start=c(2001,2)))
 
 # - [ Why is our forecast so far out? ] -
 plot(window(ppi.ts,start=c(1999,2)), lwd=2)
 lines(window(ppi.ts,start=c(2001,2)), col="red", lwd=2)
 # -> notice the change in direction of the time series!
 
 
## ===============================================