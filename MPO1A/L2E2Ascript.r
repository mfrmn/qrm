#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 2 - EXERCISE 2 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")

# - [ Useful Packages ] - 
 #install.packages("TSA")
 #install.packages("dynlm")
 #install.packages("rugarch")
 #install.packages("forecast")
 #install.packages("FinTS")
 library(TSA) ; library(dynlm) ; library(rugarch) ; library(forecast) ; library(FinTS)
 
 
## PART 1 : Fitting an ARCH(q) model
## =================================================

# - [ Loading and Storing Time Series Data ] -
 quart.dat <- read.csv("http://thiloklein.de/R/Lent/quarterly.csv", header=T, sep=",")
 ppi.ts <- ts(quart.dat$PPI, start=c(1960,1), freq=4)
 lppi.ts <- log(ppi.ts)
 lppi.d1 <- diff(lppi.ts)
 lppi.d1 <- as.data.frame(lppi.d1) #ugarchfit seems to currenly have trouble with .ts objects so convert

# - [ Recall our best ARIMA model ] -
 arima1.14 <- arima(lppi.ts, order=c(1,1,4), fixed=c(NA, NA, 0, 0, NA)) ; coeftest(arima1.14)
 ArchTest(c(arima1.14$res), lags=5) # suffers from conditional heteroskedasticity
 par(mfrow=c(2,1)); acf(arima1.14$res^2); pacf(arima1.14$res^2)
 # -> spikes in ACF/PACF at 4 suggests we should try an ARCH 4 model

# - [ Try fitting an ARCH(q) model ] -
 spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(4,0)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T),
                    fixed.pars=list(ma2=0,ma3=0))
 arch4 <- ugarchfit(data=lppi.d1, spec=spec)
 arch4
 # mean is E[Y_t] (if greater than 0 => there is a mean intercept)
 # omega is the variance intercept
 plot(arch4, which="all")

# - [ ARCH Diagnostics ] -

 # i)   Check Q-stats : should be no remaining correlation
 # ii)  Check ARCH LM : should be no evidence of remaining ARCH effects
 # iii) If satisfy (i) and (ii), can we reduce the value of q?
 # iv)  If do not satisfy (i) and (ii) could try increasing q OR try GARCH

 nNA <- which(is.na(arch4@fit$resid)==F)
 arch4.sresid <- arch4@fit$residuals / arch4@fit$sigma
 ljung.box.test.1(arch4.sresid[nNA], seq(0,50,5))
 # -> q-stats seem OK
 # -> ARCH-LM in model output shows there exists remaining ARCH effects
 
## PART 2 : Fitting a GARCH(q,1) model
## =================================================
 
# - [ Try fitting a GARCH(1,1) model ] 
 spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T),
                    fixed.pars=list(ma2=0,ma3=0))
 garch1 <- ugarchfit(data=lppi.d1, spec=spec)
 garch1
 plot(garch1, which="all")

# - [ GARCH Diagnostics ] -

 # i)   Check Q-stats : should be no remaining correlation
 # ii)  Check ARCH LM : should be no evidence of remaining ARCH effects
 # iii) If do not satisfy (i) and (ii) could try increasing q (i.e. garchOrder=c(q,1))
 
 nNA <- which(is.na(garch1@fit$resid)==F)
 garch1.sresid <- garch1@fit$residuals / garch1@fit$sigma
 ljung.box.test.1(garch1.sresid[nNA], seq(0,50,5))
 # -> q-stats seem OK
 # -> ARCH-LM in model output shows there aren't any remaining ARCH effects
 
 # - [ Test residuals for normality ] -
 par(mfrow=c(1,1))
 hist(garch1.sresid, breaks=10, probability=T)
 grid.x <- seq(min(garch1.sresid), max(garch1.sresid), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(garch1.sresid,na.rm=T))
 lines(grid.x,grid.y)
 jarque.bera.test(garch1.sresid[nNA])
 # -> evidence of deviation from normality
 
## PART 3 : Other Models
## =================================================
 
# - [ Drop the mean in mean.model ] -
 # if the mean is insignificant, may as well drop it since model power increases
 spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=F),
                    fixed.pars=list(ma2=0,ma3=0))
 # -> not appropriate here since mean (mu) term is statistically significant
 # NOTE: you could do this in any of the models that follow too
 
# - [ IGARCH ] -
 # use if sum of ARCH and GARCH parameters close to unity may like to try this as is more parsimonious
 spec <- ugarchspec(variance.model = list(model="iGARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T),
                    fixed.pars=list(ma2=0,ma3=0))
 tail(garch1@fit$coef,2) ; sum(tail(garch1@fit$coef,2))
 # -> not appropriate here
 
# - [ TARCH ] -
 # if evidence of sign bias then may wish to try this model, as indicated leverage effects
 spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="TGARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T),
                    fixed.pars=list(ma2=0,ma3=0))
 # -> not appropriate here
 
# - [ EGARCH ] -
 # removes constraints on sign of gamma in TARCH
 spec <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T),
                    fixed.pars=list(ma2=0,ma3=0))
 # -> not appropriate here
 
# - [ GARCH-M and EGARCH-M ] -
 # if you suspect the existence of in-mean effects
 spec <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T, archm=T),
                    fixed.pars=list(ma2=0,ma3=0))
 # -> simply add archm=T to original garch/egarch formula
 
## PART 4 : Forecasting
## =================================================
 
# - [ Return to the GARCH model] -
 spec <- ugarchspec(variance.model = list(model="fGARCH", submodel="GARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(1,4), include.mean=T),
                    fixed.pars=list(ma2=0,ma3=0))
 fit <- ugarchfit(data=lppi.d1, spec=spec, out.sample=20)
 # -> out.sample means to produce the forecast leaving out the last x (here 20) data points
 # -> this will produce a pseudo-out-of-sample forecast
 
# - [ Produce a forecast ] -
 forc <- ugarchforecast(fit, n.ahead=20)
 forc.roll <- ugarchforecast(fit, n.ahead=20, n.roll=20)
 # -> rolling forecast creates a new forecast as data becomes available
 forc
 forc.roll@forecast$forecasts
 
# - [ Plot the forecast ] -
 plot(forc,which=1) # non-rolling forecast of series
 plot(forc.roll,which=2) # rolling forecast of series
 plot(forc,which=3) # forecast of sigma

# - [ Explore the accuracy of the forecast ] -
 fpm(forc) # accuracy of non-rolling forecast
 which.min(fpm(forc.roll)[1,])
 which.min(fpm(forc.roll)[2,])
 # -> forecast will 1-roll has lowest MSE and MAE
 
# - [ Can also forecast using the full data set ] -
 fit <- ugarchfit(data=lppi.d1, spec=spec)
 forc <- ugarchforecast(fit, n.ahead=20)
 par(mfrow=c(2,1));plot(forc,which=1);plot(forc,which=3)
 
 
## ===============================================