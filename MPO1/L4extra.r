################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 4 - EXTRA ~~~~~~~~~~~~~~~~~~~~ ##
################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 source("http://www.thiloklein.de/R/myfunctions.R")
 
 #install.packages("AER"); help("StockWatson2007", package = "AER")
 library(AER)
 data("Guns")
 guns.dat <- Guns
 str(guns.dat)

 
## PART 1 : Fitting and Checking the Full Model
## =================================================
 
 # -[ Look at available variables ]-
 help("Guns")
 
 # -[ Fit full model with total crime as dependent variable ]-
 guns.dat$crime <- guns.dat$violent+guns.dat$murder+guns.dat$robbery 
 guns.lm <- lm(crime~law+prisoners+density+income+population+afam+cauc+male+state+year, guns.dat) ; shccm(guns.lm)

 # -[ Residual plot ]-
 plot(fitted.values(guns.lm), rstandard(guns.lm))
 lines(lowess(fitted.values(guns.lm), rstandard(guns.lm)), col="blue", lwd=2)
 abline(0,0, col="red", lwd=2, lty=2)
 # -> clear heteroskedasticity, non-linearity
 
 # -[ Histogram of residuals ]-
 par(mfrow=c(1,2))
 grid.x <- seq(min(rstandard(guns.lm)), max(rstandard(guns.lm)), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(rstandard(guns.lm)))*nrow(guns.dat)
 hist(rstandard(guns.lm)) ; lines(grid.x, grid.y, col="red")
 qqPlot(guns.lm, simulate=T)
 # -> shape good but long tails
 
 # -[ To plot correlations remove factors with more than 2 levels and any variables
 #    we are not interested in, and re-code binary variables ]-
 library(ellipse)
 guns.dat2 <- guns.dat[,-c(1,2,3,4,12)]
 guns.dat2$law <- ifelse(guns.dat2$law == "yes", 1, 0)
 corST <- cor(guns.dat2, use="complete")
 ord <- order(corST[1,])
 xc <- corST[ord, ord]
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 par(mfrow=c(1,1))
 plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag=T)
 vif(guns.lm)
 # -> notice that including state and year makes interpretation hard
 
 # -[ Fit new model, dropping state and year to better interpret VIFs ]-
 guns.lm2 <- lm(crime~law+prisoners+density+income+population+afam+cauc+male,guns.dat) ; shccm(guns.lm2)
 vif(guns.lm2)
 # -> Observe agam and cauc give us almost the same info and have huge VIFs, we could perhaps drop one
 
 # -[ Fit new model, dropping cauc ]-
 guns.lm3 <- lm(crime~law+prisoners+density+income+population+afam+male,guns.dat) ; shccm(guns.lm3)
 vif(guns.lm3)
 # -> Much better
 
 # -[ Fit new model, adding state and year back in ]-
 guns.lm4 <- lm(crime~law+prisoners+density+income+population+afam+male+state+year,guns.dat) ; shccm(guns.lm4)

 
## PART 2 : Transforming the Variables
## =================================================
 
 # -[ Look for potential transformations of dependent variable ]-
 boxcox(guns.lm4, plotit=T)
 # -> suggests taking log of dependent
 guns.lm5 <- lm(I(log(crime))~law+prisoners+density+income+population+afam+male+state+year,guns.dat); shccm(guns.lm5)

 # -[ Look for potential transformations of independent variables ]-
 crPlots(guns.lm5)
 # -> big spread for prisoners, density, population, afam suggests taking logs
 # -> curve in income suggests should try squaring
 
 # -[ Fit above model ]-
 guns.lm6 <- lm(I(log(crime))~law+I(log(prisoners))+I(log(density))+income+I(income^2)+I(log(population))+I(log(afam))+male+state+year,guns.dat); summary(guns.lm6)
 # -> Note shccm has issue solving this so we have to revert to summary
 crPlots(guns.lm6)
 # -> observe that the trend in year looks like a quadratic fn, maybe we could replace categorical year with quadratic
 
 # -[ New model incorprating time as a quadratic ]-
 guns.dat$time <- as.numeric(guns.dat$year)-1
 guns.lm7 <- lm(I(log(crime))~law+I(log(prisoners))+I(log(density))+income+I(income^2)+I(log(population))+I(log(afam))+male+state+time+I(time^2)+I(time^3)+I(time^4),guns.dat); summary(guns.lm7)
 crPlots(guns.lm7)
 # -> crPlots look good now
 
 # -[ Test to see whether can drop certain variables from model ]-
 linearHypothesis(guns.lm7, c("I(log(density))","I(log(population))"))
 # -> correlation between density and crime much higher than population and crime, try dropping population
 
 # -[ New model dropping population ]-
 guns.lm8 <- lm(I(log(crime))~law+I(log(prisoners))+I(log(density))+income+I(income^2)+I(log(afam))+male+state+time+I(time^2)+I(time^3)+I(time^4),guns.dat); summary(guns.lm8)
 # -> this looks good, you might also like to consider recombining state variables by area (e.g. midwest, south, east coast, etc.)
 
 # -[ State creates lots of factors, lets try replacing states by regions ]-
 region <- list()
 region[[1]] <- c(7,20,22,30,40,46)
 region[[2]] <- c(31,33,39)
 region[[3]] <- c(14,15,23,36,50)
 region[[4]] <- c(16,17,24,26,28,35,42)
 region[[5]] <- c(8,9,10,11,21,34,41,47,49)
 region[[6]] <- c(1,18,25,43)
 region[[7]] <- c(4,19,37,44)
 region[[8]] <- c(3,6,13,27,29,32,45,51)
 region[[9]] <- c(2,5,12,38,48)
 guns.dat$region <- rep(NA,nrow(guns.dat))
 for (i in 1:nrow(guns.dat)) {
     index <- which(levels(guns.dat$state) == guns.dat$state[i])
     for (j in 1:9) {
         if (length(which(region[[j]]==index))>0) guns.dat$region[i] <- j
     }
 }
 guns.dat$region
 guns.dat$region <- as.factor(guns.dat$region)
 guns.lm82 <- lm(I(log(crime))~law+I(log(prisoners))+I(log(density))+income+I(income^2)+I(log(afam))+male+region+time+I(time^2)+I(time^3)+I(time^4),guns.dat); summary(guns.lm82)
 # -> notice that the quality of your model takes quite a big hit, I prefer guns.lm8

## PART 3 : Check New Model Fits OLS Assumptions
## =================================================
 
 # -[ Re-examine residual plot ]-
 par(mfrow=c(1,1))
 plot(fitted.values(guns.lm8), rstandard(guns.lm8))
 lines(lowess(fitted.values(guns.lm8), rstandard(guns.lm8)), col="blue", lwd=2)
 abline(0,0, col="red", lwd=2, lty=2)
 # -> much better
 
 # -[ Re-examine histogram of residuals ]-
 par(mfrow=c(1,2))
 grid.x <- seq(min(rstandard(guns.lm8)), max(rstandard(guns.lm8)), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(rstandard(guns.lm8)))*nrow(guns.dat)
 hist(rstandard(guns.lm8),breaks=8) ; lines(grid.x, grid.y, col="red")
 qqPlot(guns.lm8, simulate=T)
 # -> no longer as long tails, seems to fit normal assumption well
 
 # -[ RESET test for model misspecification ]-
 reset(guns.lm) ; reset(guns.lm8)
 # -> RESET statistic much lower for lm8, but still some evidence of misspecification
 
 # -[ Try adding addition ^2 terms terms ]-
 guns.lm9 <- lm(I(log(crime))~law+I(log(prisoners))+I(log(density))+income+I(income^2)+I(log(afam))+I(log(afam)^2)+male+state+time+I(time^2)+I(time^3)+I(time^4),guns.dat); summary(guns.lm9)
 reset(guns.lm9)
 # -> better, notice that we missed log(afam)^2 using the crPlots, so RESET helps identify these problems
 
 # -[ Test for heteroskedasticity ]-
 library(lmtest)
 bptest(guns.lm) ; bptest(guns.lm9)
 # -> still evidence of heteroskedasticity, should preferably use shccm (but unfortunately can't here)
 
 # -[ Jarque-Bera test for normality ]-
 library(tseries)
 jarque.bera.test(guns.lm$res)
 jarque.bera.test(guns.lm9$res)
 # -> JB statistic much lower for lm9, but still some evidence of non-normality (this is almost always the case)
 
 # -[ Test for outliers ]-
 library(car)
 outlierTest(guns.lm)
 outlierTest(guns.lm9)
 # -> only 1 suggested outlier in lm9, good
 
 # -[ Test for leverage ]-
 K <- length(coef(guns.lm)) ; N <- nrow(guns.lm$model)
 avg.hat <- K/N         # Average hat value
 unname(which(hatvalues(guns.lm)>2*avg.hat))
 # -> in original model some of outliers and points with leverage are the same => risk of influence
 
 K <- length(coef(guns.lm9)) ; N <- nrow(guns.lm9$model)
 avg.hat <- K/N         # Average hat value
 unname(which(hatvalues(guns.lm9)>2*avg.hat))
 # -> in new model outlier and points with leverage are not the same => not much risk of influence
 
 # -[ Test for influence ]-
 unname(which(cooks.distance(guns.lm) > 4/sqrt(guns.lm$df.residual)))
 unname(which(cooks.distance(guns.lm9) > 4/sqrt(guns.lm9$df.residual)))
 
 par(mfrow=c(1,2))
 plot(guns.lm, which=4) ; abline(h=4/sqrt(guns.lm$df.residual), lty=2, col="red")
 plot(guns.lm9, which=4) ; abline(h=4/sqrt(guns.lm9$df.residual), lty=2, col="red")
 # -> ORIGINAL : some high cook's D's relative to avg
 # -> NEW : no single observation with much higher cook's D
 
 par(mfrow=c(1,2))
 plot(guns.lm, which=5)
 influencePlot(guns.lm, id.method="noteworthy", labels=row.names(guns.dat))
 # -> ORIGINAL : possible issues
 
 par(mfrow=c(1,2))
 plot(guns.lm9, which=5)
 influencePlot(guns.lm9, id.method="noteworthy", labels=row.names(guns.dat))
 # -> NEW : fine
 
 # All in all, our new model seems pretty good, and much improved on the original.