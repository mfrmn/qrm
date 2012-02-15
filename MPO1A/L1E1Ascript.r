#############################################################################
## ~~~~~~~~~~~~~~~ MPO1A - LAB SESSION 1 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#############################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")

# - [ Useful Packages ] - 
 #install.packages("TSA")
 #install.packages("dynlm")
 #install.packages("rugarch")
 library(TSA) ; library(dynlm) ; #library(rugarch)


## PART 1 : Plots, Lags, Differences & Growth Rates
## =================================================

# - [ Loading and Storing Time Series Data ] - 
 data(larain) ; data(tempdub)
 ?larain
 ?tempdub
 la.ts <- ts(larain, start=c(1878), frequency=1) ; la.ts	        # yearly data
 dub.ts <-  ts(tempdub, start=c(1964,1), frequency=12) ; dub.ts     # monthly data

 # Dealing with different ts periods:
 # start=c(year), frequency=1 : yearly
 # start=c(year,qrt), frequency=4 : quarterly
 # start=c(year,month), frequency=12 : monthly
 # start=c(year,week), frequency=52 : weekly
 # start=c(week,day), frequency=7 : daily

# - [ Plotting the Time Series ] - 
 plot(la.ts, xlab="Year", ylab="LA Rain (Inches)", type="o")
 plot(stl(dub.ts, "periodic"))  # decomposes ts into seasonal and trend components by loess

# - [ Shifting the Time Series (i.e. lags) ] - 
 la.l1 <- zlag(la.ts,1) ; la.l1	# 1st lag [i.e. z(t) <- y(t-1)]
 la.l2 <- zlag(la.ts,2) ; la.l2	# 2nd lag [i.e. z(t) <- y(t-2)]

 # for nth lag use: zlag(data.ts,n)

# - [ Taking Differences of the Time Series ] - 
 # 1st difference: lag 1 [i.e. y(t)-y(t-1)]
 la.d1 <- c(NA,diff(la.ts,lag=1)) ; la.d1
 
 # 1st difference: lag 2 [i.e. y(t)-y(t-2)]
 la.d1.l2 <- c(rep(NA,2),diff(la.ts,lag=2)) ; la.d1.l2

 # 2nd difference: lag 1 [i.e. y(t)-2*y(t-1)+y(t-2)]
 la.d2 <- c(rep(NA,2),diff(la.ts,diff=2)) ; la.d2		
 # -> equivalently: c(NA,diff(la.ts.d1,lag=1))

 # 2nd difference: lag 2 [i.e. y(t)-2*y(t-2)+y(t-4)]
 la.d2.l2 <- c(rep(NA,4),diff(la.ts,lag=2,diff=2)) ; la.d2.l2
 # -> equivalently: c(rep(NA,2),diff(la.ts.d1.l2,lag=2,diff=1))

# - [ Calculating Approximate Growth Rates ] - 
 c(NA,diff(log(la.ts),lag=1))

# - [ Calculating Actual Growth Rates ] - 
 c(NA,diff(la.ts,lag=1))/la.ts

 
## PART 2 : Basic Trends Models using OLS
## =================================================
 
 # Used for estimating the parameters of common, non-constant mean trend models
 
 data(airmiles) ; ?airmiles
 air.ts <- ts(as.numeric(airmiles), start=c(1996,1), frequency=12)
 plot(air.ts, type="l")
 points(y=air.ts,x=time(air.ts),pch=as.vector(season(air.ts)))
 plot(stl(air.ts, "periodic"))
 
 lair.ts <- log(air.ts)
 
# - [ Linear Time Trend ] -
 lair.lm <- lm(lair.ts~time(lair.ts))
 #shccm(lair.lm) # you will only need to do this to load libraries if shaccm doesn't work
 shaccm(lair.lm)
 # Note: We must use heteroskedasticity AND autocorrelation consistent standard errors
 plot(lair.ts, type="l") ; abline(lair.lm)
 
# - [ Seasonal Means Model ] -
 month <- season(lair.ts)
 lair.lm2 <- lm(lair.ts~-1+month)
 shaccm(lair.lm2)
 plot(lair.ts, type="l") ; lines(ts(fitted(lair.lm2),start=c(1996,1), frequency=12), col="red")
 
# - [ Linear Trend + Seasonal Component Model ] -
 lair.lm3 <- lm(lair.ts~-1+time(lair.ts)+month)
 shaccm(lair.lm3)
 plot(lair.ts, type="l") ; lines(fitted(lair.lm3),x=as.vector(time(lair.ts)), col="red")
 
 
## PART 3 : Residual Analysis
## =================================================
 
# Note: If trend model reasonably correct, then residuals should look like white noise.
#       If residuals are like white noise, then should behave like independent normal r.v.
#          with 0 mean.
 
# - [ Plotting the residuals against time ] -
 par(mfrow=c(1,1))
 plot(rstudent(lair.lm3),type="l",x=as.vector(time(lair.ts)))
 points(y=rstudent(lair.lm3),x=as.vector(time(lair.ts)),pch=as.vector(season(lair.ts)))
 # there is a clear pattern in the residuals relating to different months of the year
 
# - [ Plotting studentised residuals against fitted values ] -
 par(mfrow=c(1,1))
 plot(fitted(lair.lm3), rstudent(lair.lm3),type="n")
 points(x=as.vector(fitted(lair.lm3)), y=rstudent(lair.lm3), pch=as.vector(season(lair.ts)))
 # notice e.g. no A's in left half, no S's in right half
 
# - [ Checking for normality of residuals ] -
 par(mfrow=c(2,1))
 hist(rstandard(lair.lm3), prob=TRUE, 12) ; lines(density(rstandard(lair.lm3)))
 qqPlot(lair.lm3,simulate=T)
 # -> peak to right of 0, long tail to left

# - [ Runs Test ] -
 # Examines the residuals in sequence to look for patterns that would give evidence
 # against independence.
 runs(rstudent(lair.lm3))
 # -> p-value of 8.54e-13 = reject independence of the stochastic component
 
# - [ ACF Plot ] -
 par(mfrow=c(1,1))
 acf(rstudent(lair.lm3))
 # -> expect no autocorrelation between residuals => residuals autocorrelated
 
 # Comment : Putting all of this together it is clear to see that the residuals
 #           do not behave like a white noise process.
 #           As should be clear, OLS is not a good model for non-stationary time-series
 

# Aside : Compare to what would be expected if white noise
 lla.ts <- log(la.ts)
 lla.lm <- lm(lla.ts~time(lla.ts))
 shaccm(lla.lm)
 plot(lla.ts, type="l") ; abline(lla.lm)
 par(mfrow=c(1,1))
 plot(rstudent(lla.lm),type="o",x=as.vector(time(lla.ts)))
 par(mfrow=c(1,1))
 plot(fitted(lla.lm), rstudent(lla.lm))
 par(mfrow=c(2,1))
 hist(rstandard(lla.lm), prob=TRUE, 12) ; lines(density(rstandard(lla.lm)))
 qqPlot(lla.lm,simulate=T)
 runs(rstudent(lla.lm))
 par(mfrow=c(1,1))
 acf(rstudent(lla.lm))
 # => Could be estimated by OLS
 
## ===============================================