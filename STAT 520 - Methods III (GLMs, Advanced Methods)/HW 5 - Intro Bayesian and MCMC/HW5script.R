#gamma prior for lambda
alpha0 <- 5
beta0 <- 0.5
#data model parameters
lambdad <- 9
nd <- 20

mcsize=2000
mcnt <-0
mcestg <- c()
mcesti<- c()
CIcovg <- rep(0,times=mcsize)
CIcovi <- rep(0,times=mcsize)

#Monte Carlo Simulation of CIs
repeat{
  mcnt <- mcnt+1
  
  #draw lambda from gamma prior
  lam0 <- rgamma(1, shape=alpha0, rate=beta0)

  #generate from data model
  poisi <- rpois(n=nd, lambda=lambdad)
  
  #generate estimates of lambda
  mcestg[mcnt] <- (alpha0+sum(poisi))/(nd+beta0)
  mcesti[mcnt] <- (sum(poisi)+1)/nd
  
  #CI coverage estimates
  CIlg <- qgamma(.05, shape=alpha0+sum(poisi),rate=nd+beta0)
  CIug <- qgamma(.95, shape=alpha0+sum(poisi),rate=nd+beta0)
  if(lambdad<=CIug & lambdad >= CIlg){CIcovg[mcnt]=1}
  CIli <- qgamma(.05, shape=1+sum(poisi),rate=nd)
  CIui <- qgamma(.95, shape=1+sum(poisi),rate=nd)
  if(lambdad<=CIui & lambdad >= CIli){CIcovi[mcnt]=1}
  
  if(mcnt==mcsize){break}
}
########################
#####CI coverages#######
mean(CIcovg)
mean(CIcovi)

#####################
######## MC CIs ###########
#gamma prior
#MC variance and standard error
mcvarg <- (mcsize*(mcsize-1))^(-1)*sum((mcestg-mean(mcestg))^2)
mcseg <- sqrt(mcvarg)
#lower and upper bounds for the MC conf. interval
mcgCIl <- mean(mcestg) - qnorm(.975)*mcseg
mcgCIu <- mean(mcestg) + qnorm(.975)*mcseg
#center of the interval
mean(mcestg)
mcgCIl; mcgCIu
#CI width
widthg <- mcgCIu-mcgCIl; widthg

#uniform prior
#MC variance and standard error
mcvari <- (mcsize*(mcsize-1))^(-1)*sum((mcesti-mean(mcesti))^2)
mcsei <- sqrt(mcvari)
#lower and upper bounds for the MC conf. interval
mciCIl <- mean(mcesti) - qnorm(.975)*mcsei
mciCIu <- mean(mcesti) + qnorm(.975)*mcsei
#center of the interval
mean(mcesti)
mciCIl; mciCIu
#CI width
widthi <- mciCIu-mciCIl; widthi
###################################################
###################################################
###################################################
library(R2jags)
library(coda)

#Bayesian Data Analysis
clouds <- read.csv("CloudData.csv")

JMfit <- jags( data = list(y = clouds$SeededRain, x=clouds$UnseededRain, n = length(clouds$SeededRain)), inits = NULL, model.file = "hw5wb.R", n.chains = 3, n.iter = 110000, 
               n.burnin = 50000, parameters.to.save  = c("mu",   "muc", "alpha"), n.thin = 10)
print(JMfit)
traceplot(JMfit)
JMmcmc <- as.mcmc(JMfit)
gelman.plot(JMmcmc)
autocorr.plot(JMmcmc)
densplot(JMmcmc)
HPDinterval(JMmcmc)

JMfit2 <- jags( data = list(y = clouds$SeededRain, x=clouds$UnseededRain, n = length(clouds$SeededRain)), inits = NULL, model.file = "hw5wb2.R", n.chains = 3, n.iter = 110000, 
               n.burnin = 50000, parameters.to.save  = c("mu",   "muc", "alpha"), n.thin = 10)
print(JMfit2)
traceplot(JMfit2)
JMmcmc2 <- as.mcmc(JMfit2)
gelman.plot(JMmcmc2)
autocorr.plot(JMmcmc2)
densplot(JMmcmc2)
hpd <- HPDinterval(JMmcmc2)















