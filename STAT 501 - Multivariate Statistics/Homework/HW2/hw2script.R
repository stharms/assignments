#bootstrap functions
library(MASS)
library(tidyverse)
library(kableExtra)
#generate covariates
covariategen <- function(n,p){
  mvnSig <- matrix(0,ncol=p,nrow=p)
  i=0
  repeat{
    i=i+1
    j=0
    repeat{
      j=j+1
      mvnSig[i,j] <- 0.75^(abs(i-j))
      if(j==p){break}
    }
    if(i==p){break}
  }
    covmat <- mvrnorm(n=n, mu=rep(0,times=p), Sigma = mvnSig)
    Betas <- rep(c(1,0), each=p/2)
    return(list(xis = covmat, B=Betas))
}

errorgen <- function(n, dist, df=0){
  eis <- rnorm(n=n, mean=0, sd=1)
  if(dist=="t"){
    eis = rt(n=n, df=df, ncp=0)
    eis = eis/sqrt(df/(df-2))
  }
  if(dist=="exponential"){
    eis = rexp(n=n, rate=1)
    eis = eis-1
  }
  return(eis)
}

bootstrapsim <- function(n,p, dist, dof,reps){
        estimates <- matrix(nrow=reps,ncol=p)
        cicovs <- matrix(nrow=reps,ncol=p)
  i=0
  repeat{
    i=i+1
  #generate data
  covs <- covariategen(n,p)
  xis <- covs$xis
  Betas <- covs$B
  errors <- errorgen(n, dist=dist, df=dof)
  yis <- xis%*%Betas + errors
  #estimate
  lm.temp <- lm(formula=yis~xis)
  estimates[i,] <- coef(lm.temp)[-1]
  cis <- confint(lm.temp, level=0.95)
  cicovs[i,] <- c(cis[-1,1] <= Betas & Betas <= cis[-1,2])
  if(i==reps){break}
  }
  return(list(ests=estimates, CIcovs = cicovs))
}


#PUT ALL OBS IN A LIST
listify<- function(nlist,distlist,dflist,reps,p){
  estarray <- array(dim=c(length(nlist), length(distlist), reps, p))
  ciarray <- estarray
  nc=0
  repeat{
    nc=nc+1
      dc = 0
      repeat{
        dc=dc+1
        n=nlist[nc];  distf = distlist[dc]; ddf=dflist[dc]
        sims <- bootstrapsim(n=n, p=p, dist=distf, dof=ddf, reps = reps)
        estarray[nc,dc,,] <- sims$ests
        ciarray[nc,dc,,] <- sims$CIcovs
        if(dc==length(distlist)){break}
    }
    if(nc==length(nlist)){break}
  }
  return(list(CIs = ciarray, ests=estarray))
}

nlist <- c(40, 80)
plist <- c(10, 30)
errordistlist <- c("normal", "t","t", "exponential")
dflist <- c(0,3,10,1)
CIcovlist <- list()

smallp <- listify(nlist=nlist,distlist=errordistlist,dflist=dflist,reps=1000,p=10)
largep <- listify(nlist=nlist,distlist=errordistlist,dflist=dflist,reps=1000,p=30)
colnames(smallp$CIs) <- colnames(smallp$ests) <- colnames(largep$ests) <-colnames(largep$CIs) <- paste(errordistlist, c("", "3","10",""))
rownames(smallp$CIs) <- rownames(smallp$ests) <- rownames(largep$ests) <-rownames(largep$CIs) <- c("n=40", "n=80")
#splitp10 <- smallp$CIs[,,,1:5]; splitpnull <- smallp$CIs[,,,6:10]
#estp10 <- smallp$ests[,,,1:5]; estpnull <- smallp$ests[,,,6:10]
#splitp30 <- largep$CIs[,,,1:15]; splitp30null <- largep$CIs[,,,16:30]
#estp30 <- largep$ests[,,,1:15]; estp30null <- largep$ests[,,,16:30]

#examine effects of sample size and dimension size
p9 <- smallp$CIs %>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p10 <- smallp$ests %>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p1 <- smallp$CIs[,,,1:5]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p2 <-smallp$ests[,,,1:5]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p3 <- smallp$CIs[,,,6:10]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p4 <- smallp$ests[,,,6:10]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p11 <- largep$CIs %>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p12 <- largep$ests %>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p5<- largep$CIs[,,,1:15]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p6<-largep$ests[,,,1:15]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p7<- largep$CIs[,,,16:30]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t
p8<-largep$ests[,,,16:30]%>% apply(c(1,2,4),mean) %>% apply(c(1,2), mean) %>% t

p30cicovs <- apply(smallp$ests,c(1,2,4), mean)
b1cicovsp <- apply(splitp30, c(1,2,4), mean)
b1cicovsp;
apply(b1cicovsp,c(1,2),mean)
b0cicovsp <- apply(splitp30null, c(1,2,4), mean)

results <- cbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12)[,c(seq(1,23, by=2),seq(2,24,by=2))]
resultsp10 <-cbind(p2,p1,p4,p3,p10,p9)[,c(seq(1,11,by=2),seq(2,12,by=2))]
resultsp30 <-cbind(p6,p5,p8,p7,p12,p11)[,c(seq(1,11,by=2),seq(2,12,by=2))]
colnames(resultsp10) <- colnames(resultsp30) <- paste(rep(c("Est", "CIcovg"),times=6), colnames(resultsp10))
round(resultsp10,4);
round(resultsp30,4) %>% kable

resultsp10 %>% round(4) %>% kable %>%  kable_styling(c("striped", "bordered")) %>%
  add_header_above(c("Error dist.", expression(beta*"=1") = 2, expression(beta*"=0") = 2, expression(beta) = 2, expression(beta*"=1") = 2, expression(beta*"=0") = 2, expression(beta) = 2)) %>%
  add_header_above(c("", "n=40"=6, "n=80"=6)) %>% add_header_above(c("","p=10"=12))



covs <- covariategen(n=300,p=8)
errors <- errorgen(n=300, dist="t",df=3);hist(errors)
xi <- as.matrix(covs$xis)
yi <- xi%*%covs$B + errors
testlm <- lm(yi~xi)
coef(testlm)
ci <- confint(testlm, level=0.95)
plot(yi~xi[,1])





