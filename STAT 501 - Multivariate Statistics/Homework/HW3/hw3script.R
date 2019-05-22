conditionalnorm <- function(n, xbar, sigmasq, method="cholesky"){
  #generate mean vector
  muv <- rep(xbar, times = n-1)
  #generate covariance matrix
  sigmav <- sigmasq*diag(n-1) - sigmasq/n*matrix(1,nrow=n-1,ncol=n-1)
  #get cholesky decomposition of the covariance matrix
  C<- chol(sigmav)
  #use cholesky to get samples from MVN conditional distribution
  #we could also just use mvrnorm() which uses eigendecomposition
  rs <- rnorm(n-1)
  newrs <- muv + t(C)%*%rs
  if(method=="eigen"){
  newrs <- mvrnorm(1,mu=muv,Sigma=sigmav)
  }
  newrs <- c(newrs, n*xbar-sum(newrs))
  return(newrs)
}
test <- conditionalnorm(n=10,xbar=10,mu=11,sigmasq=3)
test
test2 <- conditionalnorm(n=10,xbar=10,mu=11,sigmasq=3, method="eigen")
test2
var(test); var(test2)
mean(test); mean(test2)
i=0
variances = matrix(ncol=2,nrow=1000)
repeat{
  i=i+1
  test <- conditionalnorm(n=10,xbar=10,mu=11,sigmasq=3)
  test2 <- conditionalnorm(n=10,xbar=10,mu=11,sigmasq=3, method="eigen")
  variances[i,]= c(var(test),var(test2))
  if(i==1000){break}
}
apply(variances,2,mean)

recovermat <- function(B,sigmasq,method="cholesky"){
  #generate mvn for each element in B
  b <- c(B)
  alist <- alply(b, .margins=1,.fun=conditionalnorm, n=4, sigmasq=sigmasq, method=method) %>% 
    llply(.fun=matrix, nrow=2,ncol=2)
  #reassmble
  outmat <- rbind(cbind(alist[[1]],alist[[3]]),cbind(alist[[2]],alist[[4]]))
  return(outmat)
}


aa <- matrix(c(3,8,35,19),nrow=2,ncol=2)
aa
recovermat(aa,sigmasq=2,method="cholesky")


#############################################
# Normal Q-Q plot
library(ggplot2)
X = read.table("board.stiffness.dat", head = FALSE)

QQplot.normal = function(x){
  # x is an observed vector of a variable
  x.sort = sort(x)
  n = length(x)
  p = ( c(1 : n) - 0.5 ) / n
  q = qnorm(p)
  res = data.frame(cbind(x.sort, q))
  names(res) = c("sample.quantile", "normal.quantile")
  return(res)
}
par(mfrow=c(2,2))
qqnorm(X[,2]) + qqline(X[,2])
qqnorm(X[,3]) + qqline(X[,3])
qqnorm(X[,4]) + qqline(X[,4])
qqnorm(X[,5]) + qqline(X[,5])
QQs <- apply(X[,-1],2,FUN=QQplot.normal) %>% as.data.frame()
QQs <- data.frame(sample.quantile = gather(QQs[,c(1,3,5,7)])[,2], normal.quantile=gather(QQs[,c(2,4,6,8)])[,2],
                  Sample = as.factor(rep(1:4,each=30)))
ggplot(data = QQs) + geom_point(aes(x=normal.quantile, y=sample.quantile)) + facet_wrap(.~Sample)

chisqplot <- function(x){
  # x is an observed vector of a variable
  #get sample statistics
  xbar <- apply(x,2,mean); s <- cov(x)
  #get squared distances from x
  dis <- mahalanobis(x, center=xbar, cov=s)
  #sort
  di.sort = sort(dis)
  #get chi-squared quantiles
  n = nrow(x)
  p = ( c(1 : n) - 0.5 ) / n
  q = qchisq(p,df=ncol(x))
  #combine and output
  res = data.frame(cbind(di.sort, q))
  names(res) = c("sample.quantile", "chisq.quantile")
  return(res)
}

chisqs <- chisqplot(X[,-1])
ggplot(data=chisqs) + geom_point(aes(x=chisq.quantile,y=sample.quantile))
####################################################
testnormality <- function(X, numproj = 100000)
{
  p = ncol(X)
  n = nrow(X)
  x <- matrix(rnorm(numproj * p), nrow = p)
  y <- matrix(sqrt(apply(x^2, 2, sum)), nrow = p, ncol = numproj, by = T)
  
  z <- x / y
  
  tempdat <- as.matrix(X) %*% z  # this gives rise to a p x numproj matrix
  # called tempdat here
  
  # perform Shapiro-Wilks' test and calculate individual p-values on each of
  # the numproj observation sets.
  
  pvals <- as.numeric(matrix(unlist(apply(tempdat, 2, shapiro.test)),
                             ncol=4, by = T)[,2])
  return(min(pvals))
}

system.time(testnormality(X[,-1], numproj=10000)) * 1000

boots <- c()
i=0
bootsize = 1000
repeat{
  i=i+1
  boots[i] <- testnormality(X[,-1], numproj=10000)
  if(i==bootsize){break}
}

length(unique(boots))
boots %>% quantile(probs=.05)
as.numeric(matrix(unlist(apply(X[,-1], 2, shapiro.test)),ncol=4, by = T)[,2])


###############################
library(energy)
mvnorm.etest(X[,-1], R=10000)
