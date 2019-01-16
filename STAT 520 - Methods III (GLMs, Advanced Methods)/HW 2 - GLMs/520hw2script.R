setwd("~/Documents/Microsoft Word/Grad School/STAT 520/HW 2")
#################################
#read in dataset
clouds <- read.csv(file="clouds.csv", header=T)
head(clouds)
regmodel <- cbind(c(clouds[,1],clouds[,2]), factor(c(rep(1, times = 26),rep(0,times=26))))
#separate variables
uns <- clouds[,1]; seded <- clouds[,2]

#a log-likelihood function for the full model
loglikgams.edf <- function(ys,zs, pars){
  thetay1 <- pars[1];phiy1 <- pars[2]; thetay2 <- pars[3]; phiy2 <- pars[4]
  llik <- sum(phiy1*(thetay1*ys-(-log(-thetay1)))-phiy1*log(phiy1)+(phiy1-1)*log(ys) - log(gamma(phiy1))) + 
    sum(phiy2*(thetay2*zs-(-log(-thetay2)))-phiy2*log(phiy2)+(phiy2-1)*log(zs) - log(gamma(phiy2)))
  return(-1*llik)
}

#a log likelihood function for the reduced model
loglikgams.edf.share <- function(ys,zs, pars){
  thetay1 <- pars[1];phiy1 <- pars[2]; thetay2 <- pars[3];
  llik <- sum(phiy1*(thetay1*ys-(-log(-thetay1)))-phiy1*log(phiy1)+(phiy1-1)*log(ys) - log(gamma(phiy1))) + 
    sum(phiy1*(thetay2*zs-(-log(-thetay2)))-phiy1*log(phiy1)+(phiy1-1)*log(zs) - log(gamma(phiy1)))
  return(-1*llik)
}

#some starting values for our model
edf.ps <- c(-1/mean(uns),var(uns)/(mean(uns)^2),-1/mean(seded),var(seded)/(mean(seded)^2))
#get estimates for the full model
estsfull<- optim(f=loglikgams.edf, par = edf.ps, ys = uns, zs=seded, hessian = T)
estsfull$par
#get estimates for the reduced model
estsred <- optim(f=loglikgams.edf.share, par = c(edf.ps[1],edf.ps[4],edf.ps[3]), ys = uns, zs=seded, hessian = T)
estsred$par

#likelihood ratio test for reduced vs. full
#need to take negative of log-likelihood since we minimized above
lrs <- 2*(estsred$value-estsfull$value)
lrs
1-pchisq(lrs,df=1)
#null hypothesis is that the reduced model is adequate. Fail to reject H0 at 5% or 10% significance level

#constructing estimates and a confidence interval with the reduced model for the ratio of the means
#get means and Fisher information matrix
means <- c(estsred$par[1],estsred$par[3])
infmat <- solve(estsred$hessian)

#Use the Delta method, here the function is (-1/thety2)/(-1/thety1) = thety1/thety2
Dmat <- c(1/means[2],0, -means[1]/(means[2]^2))
#point estimate
rat <- means[1]/means[2]
rat
#standard error
Dmat <- c(1/means[2],0, -means[1]/(means[2]^2))
vrat <- t(Dmat) %*% infmat %*% Dmat
sqrt(vrat)

#95% Confidence interval
c(rat-qnorm(.975)*sqrt(vrat), rat+qnorm(.975)*sqrt(vrat))

#compare the point and interval estimates from above with our actual sample mean ratio
mean(seded)/mean(uns)




##############################
###############################
################################
#################################
thetas <- function(ys, params){
  alph <- params[1]
  bet <- params[2]
  cnt <- 0
  n = length(ys)
  ai1 <- NULL
  ai2 <- NULL
  repeat{
    cnt <- cnt+1
    ai1[cnt] <- (alph-1)*log(ys[cnt])
    ai2[cnt] <- (bet)*ys[cnt]
    if(cnt==n){break}
  }
  return(cbind(ai1,ai2))
}
#########################################
loglikgams <- function(ys,zs, pars){
  alphy <- pars[1];bety <- pars[2]; alphz <- pars[3]; betz <- pars[4]
  canony <- thetas(ys,c(pars[1], pars[2]))
  ya1 <- canony[,1]; ya2 <- canony[,2]
  canonz <- thetas(zs,c(pars[3], pars[4]))
  za1 <- canonz[,1]; za2 <- canonz[,2]
  llik <- sum(ya1-ya2-log(gamma(alphy)+alphy*log(bety))) + sum(za1-za2-log(gamma(alphz))+alphz*log(betz))
  return(-1*llik)
}

loglikgams <- function(ys,zs, pars){
  alphy <- pars[1];bety <- pars[2]; alphz <- pars[3]; betz <- pars[4]
  ny <- length(ys); nz <- length(zs)
  llik <- sum((alphy-1)*log(ys)-bety*ys)-ny*log(gamma(alphy)+ny*alphy*log(bety)) + 
    sum((alphz-1)*log(zs)-betz*zs)-nz*log(gamma(alphz)+nz*alphz*log(betz))
  return(-1*llik)
}
###########################



loglikgams(uns,seded, pars = c(10,10,10,10))
loglikgams.edf(uns, seded, pars = c(-1/mean(uns),var(uns)/(mean(uns)^2),-1/mean(seded),var(seded)/(mean(seded)^2)))
thetas(seded, params = c(10,10))

edf.ps <- c(-1/mean(uns),var(uns)/(mean(uns)^2),-1/mean(seded),var(seded)/(mean(seded)^2))
optim(f=loglikgams.edf, par = edf.ps, ys = uns, zs=seded, hessian = T)
mean(uns)
mean(seded)
