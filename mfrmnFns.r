auto.it <- function(model,alter,lower,upper,marg=F,type=c("logit","probit")) {
    mm <- model.matrix(model) ; n <- ncol(mm) ; k <- nrow(mm)
    name <- colnames(mm) ; loc <- which(name==alter)
    mm.l <- mm; mm.l[,loc] <- rep(lower,k)
    mm.u <- mm; mm.u[,loc] <- rep(upper,k)
    mm.m <- mm; mm.m[,loc] <- rep(marg,k)
    chg <- rep(NA,n)
    for (i in 1:n) {
        chg[i] <- sum((mm[,i] == 1) + (mm[,i] == 0)) == k
    }
    chg <- which(chg==T) ; 
    if (length(which(chg == loc)) > 0) chg <- chg[-which(chg == loc)]
    chg <- chg[-1] ; nchg <- length(chg)
    if (nchg > 0) {
        output <- matrix(NA, nrow=2^nchg, ncol=nchg+3)
        colnames(output) <- c(names(coef(model)[chg]),"P(lower)","P(upper)","marginal")
        for (i in 1:nchg) {
            output[,i] <- rep(c(rep(0,(2^nchg)/(2^i)),rep(1,(2^nchg/(2^i)))),2^(i-1))
        }
        F.log <- function(x){ # logistic function
            1/(1 + exp(-x))
        }
        for (i in 1:nrow(output)) {
            mm.l[,chg] <- t(matrix(rep(output[i,1:nchg],k),nrow=nchg))
            mm.u[,chg] <- t(matrix(rep(output[i,1:nchg],k),nrow=nchg))
            mm.m[,chg] <- t(matrix(rep(output[i,1:nchg],k),nrow=nchg))
            zval.l <- mm.l%*%coef(model)
            zval.u <-  mm.u%*%coef(model)
            zval.m <-  mm.m%*%coef(model)
            if (type=="logit") {
                output[i, nchg+1] <- F.log(mean(zval.l))
                output[i, nchg+2] <- F.log(mean(zval.u))
                if (marg != F) output[i, nchg+3] <- coef(model)[loc]*dlogis(mean(zval.m))
                else output[i, nchg+3] <- output[i, nchg+2]-output[i, nchg+1]
            }
            if (type=="probit") {
                output[i, nchg+1] <- pnorm(mean(zval.l))
                output[i, nchg+2] <- pnorm(mean(zval.u))
                if (marg != F) output[i, nchg+3] <- coef(model)[loc]*dnorm(mean(zval.m))
                else output[i, nchg+3] <- output[i, nchg+2]-output[i, nchg+1]
            }
        }
        output
    }
    else {
        output <- rep(NA,3)
        zval.l <- mm.l%*%coef(model)
        zval.u <-  mm.u%*%coef(model)
        zval.m <-  mm.m%*%coef(model)
        if (type=="logit") {
            output[1] <- F.log(mean(zval.l))
            output[2] <- F.log(mean(zval.u))
            if (marg != F) output[3] <- coef(model)[loc]*dlogis(mean(zval.m))
            else output[3] <- output[2]-output[1]
        }
        if (type=="probit") {
            output[1] <- pnorm(mean(zval.l))
            output[2] <- pnorm(mean(zval.u))
            if (marg != F) output[3] <- coef(model)[loc]*dnorm(mean(zval.m))
            else output[3] <- output[2]-output[1]
        }
        output
    }
}