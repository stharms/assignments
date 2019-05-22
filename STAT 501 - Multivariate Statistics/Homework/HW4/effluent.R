#  Code for computing one sample Hotelling
#  T-squared test for a paired comparison study

#              effluent.R  
#  
#  Data are read into a data frame from the file
#
#              effluent.dat
 
edat<-read.table(file = "effluent.dat", header=F, col.names=c("sample", "bod1", "ss1", "bod2", "ss2")) 
   
  edat

#  Compute sample mean vector and
#  sample covariance matrix 

  xbar<-apply(edat[ ,2:5], 2, mean)
  xbar
  xvar<-var(edat[ ,2:5])
  xvar

#  Apply the contrasts

   Cstar <-matrix( c(1, 0, -1, 0, 0, 1, 0, -1), 2, 4, byrow=T)
   Cstar
   Cxbar <- Cstar %*% xbar
   Cxvar <- Cstar %*% xvar %*%t(Cstar)

# Compute Hotelling statistic

   p <- nrow(Cxvar)
   n <- nrow(edat)
   nullmean <- rep(0, p)
   d <- Cxbar-nullmean
   t2<-n*t(d)%*%solve(Cxvar)%*%d;
   t2mod<- (n-p)*t2/(p*(n-1))
   pval <- 1- pf(t2mod,p,n-p)
   
   cat("Hotelling T-squared statistic", fill=T)
   t2

   cat("p-value", fill=T)
   pval

# alternative way done with simple use of Hotelling's T^2-test in the ICSNP
# package

library(ICSNP)
HotellingsT2( as.matrix(edat[, 2:5]) %*% t(Cstar), mu = c(0,0))

