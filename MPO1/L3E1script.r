#####################################################################
## ~~~~~~~~~~~~~~~ LAB SESSION 3 - EXERCISE 1 ~~~~~~~~~~~~~~~~~~~~ ##
#####################################################################

 rm(list=ls()) ; par(mfrow=c(1,1))
 
 house.dat <- read.csv("http://thiloklein.de/R/housing", header=T, sep=",")
 attach(house.dat)
 house.lm <- lm(housing~total, data=house.dat)

 student.dat <- read.csv("http://thiloklein.de/R/eaef", header=T, sep=",")
 attach(student.dat)
 student.lm <- lm(earnings~age+schooling+height+weight+siblings, student.dat)


## PART 1 : MAKING USE OF RESIDUAL PLOTS
## =====================================

 par(mfrow=c(1,1))

# -[ Notes on using residual plots ] -
 # fail linearity of variables if plot has a curved shape
 # *** NOTE: linearity of variables is NOT the same as linearity of parameters
 # satisfy normality if residuals concentrated around zero line with those above and below evenly distributed
 # satisfy homoskedasticity if residuals distance to zero line similar along x-axis
 
# -[ Producing a residual plot with smoothed and 0 zero lines ]-
 plot(fitted.values(house.lm), rstandard(house.lm))
 lines(lowess(fitted.values(house.lm), rstandard(house.lm)), col="blue", lwd=2)
 abline(0,0, col="red", lwd=2, lty=2)
 # satisfies linearly of variables (approximately straight line)
 # fails normality (many more observations below 0 line, more dispersion above line)
 # fails homoskedasticity (spread of residuals increasing as we move right in the plot)

 plot(fitted.values(student.lm), rstandard(student.lm))
 lines(lowess(fitted.values(student.lm), rstandard(student.lm)), col="blue", lwd=2)
 abline(0,0, col="red", lwd=2, lty=2)
 # some evidence to suggest may not satisfy linearity of variables (lowess line curved)
 # reason to question normality (observations above 0 line much more dispersed)
 # fails homoskedasticity (clear wedge shape in the plot)

## PART 2 : CHECKING THE NORMALITY OF RESIDUALS (INFORMALLY)
## =========================================================

# -[ Prepares expected shape of histogram ]-
 grid.x <- seq(min(rstandard(house.lm)), max(rstandard(house.lm)), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(rstandard(house.lm)))*nrow(house.dat)
 
# -[ Plot histogram and QQplot to check for normality  ]-
 par(mfrow=c(1,2))
 hist(rstandard(house.lm)) ; lines(grid.x, grid.y, col="red")
 qqPlot(house.lm, simulate=T)
 # narrow with fat fails => evidence suggests doesn't satisfies normality assumption
 # aside: this phenomenon is also very commonly seen with financial data
 
 par(mfrow=c(1,2))
 grid.x <- seq(min(rstandard(student.lm)), max(rstandard(student.lm)), length.out=1000)
 grid.y <- dnorm(grid.x, sd=sd(rstandard(student.lm)))*nrow(student.dat)
 hist(rstandard(student.lm)) ; lines(grid.x, grid.y, col="red")
 qqPlot(student.lm, simulate=T)
 # longer tail to right than expected (see outliers), however, in general ok

## PART 3 : DIAGNOSING MULTICOLLINEARITY
## =====================================
 
# - [ Calculating the inter-variable correlation ] -
 round(cor(student.dat, use="complete"),4)
 # => observe the reasonably high correlation between height and weight

# - [ Plotting the inter-variable correlations ] -
 # install.packages("ellipse")
 library(ellipse)
 corST <- cor(student.dat[,-1], use="complete")
 ord <- order(corST[1,])
 xc <- corST[ord, ord]
 colors <- c("#A50F15","#DE2D26","#FB6A4A","#FCAE91","#FEE5D9","white",
            "#EFF3FF","#BDD7E7","#6BAED6","#3182BD","#08519C")   
 plotcorr(xc, col=colors[5*xc + 6], type = "lower", diag=T)
 # => this is a quick and effective way of visualising the correlations

# - [ Calculating the VIFs ] -
 library(car)
 vif(student.lm)
 # => all VIFs small (<2), and so no major worry about multicollinearity


## ============================================
 detach(house.dat) ; detach(student.dat)