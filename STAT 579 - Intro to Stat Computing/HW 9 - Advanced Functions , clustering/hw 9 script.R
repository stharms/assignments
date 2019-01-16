setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 9")
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(reshape2)
###########################################################
#Exercise 1

blockdiag <- function(n,k){
  mat <- k*diag(n)
  for (i in 1:n){
    mat[i,i-1] = 1
    mat[i-1, i] = 1
  }
  return(mat)
}

blockdiag(6,5)

###########################################################
#Exercise 2
tmpFn <- function(xVec) {
  fx <- NULL
  for (i in 1:length(xVec)){
    x <- xVec[i]
    if (x < 0) fx[i] = x^2 + 2*x + 3 else
      if (x < 2) fx[i] = x + 3 else
        fx[i] = x^2 + 4*x - 7
  }
  return(fx)
}

xs <- seq(from = -3, to = 3, by = .01)
plot(x = xs, y = tmpFn(xs), type = 'l')


###########################################################
#Exercise 3
gcd <- function(m,n) {
  remainder <- m%%n
  gcd <- n
  n <- remainder
  while (remainder != 0) {
    remainder <- gcd %% remainder
    gcd <- n
    n <- remainder
  }
  return(gcd)
}

gcd(78564,35148)
gcd(35,28)

###########################################################
#Exercise 4

order.matrix <- function(x){
  outmat <- which(x == max(x), arr.ind = T)
  sx <- sort(x, decreasing = T)
  for (i in 2:length(x)){
    outmat <- rbind(outmat,which(x == sx[i], arr.ind = T))
  }
  return(outmat)
}

testchisq <- matrix(rchisq(12, df = 1), nrow = 4, ncol = 3)
testchisq
order.matrix(testchisq)

###########################################################
#Exercise 5
polaroid2 <- function(x){
  #first, find R
  R <- sqrt(t(x)%*%x)
  #Then find theta 1 to get started
  theta <- c(rep(0, times = length(x)-1))
  #each theta is the inv cosine of R times product of the sines of all of the previous thetas
  prod <- 1
  for (i in 1:length(theta)){
    theta[i] <- acos(x[i]/(R*prod))
    prod <- prod * sin(theta[i])
  }
  #our last element is just R times previous sines with no cosine
  theta<- c(theta, R * prod)
  #attach R to the front and output
  outvect <- c(R, theta)
  return(outvect)
}

##a
polaroid <- function(x){
#first get the R
  R <- sqrt(t(x)%*%x)
#then theta 1
  theta <- c(acos(x[1]/R))
  p <- length(x)
#a loop to calculate all of the thetas
  for (i in 2:(p-2)){
    divis <- sqrt(t(x[i:p])%*%x[i:p])
    theta[i] <- acos(x[i]/divis)
  }
#the final theta depends on whether xn is >= or < 0
  if(x[p] >= 0) thn1 <- acos(x[p-1]/(sqrt(x[p]^2+x[p-1]^2))) else
    thn1 <- 2*pi - acos(x[p-1]/(sqrt(x[p]^2+x[p-1]^2)))
  theta[p-1] <- thn1
#return the thetas and R together
  outvect <- c(R, theta)
  return(outvect)
}

testp <- c(1,sqrt(6)/2, sqrt(6)/2)
polaroid(testp)

#b
normalize <- function(x){
  SoS <- sqrt(sum(x^2))
  normed <- x / SoS
  return(normed)
}
testn <- c(1,2,3,4,5)
t(normalize(testn))%*%normalize(testn)

#c
#generate the matrix of psuedo random numbers
pseud <- matrix(rnorm(5000), ncol = 5, nrow = 1000)
#apply normalize to the rows
z <- t(apply(pseud, MARGIN = 1, FUN = normalize))
#some checks
head(pseud)
head(z)
#use apply with the K-S test across columns of z
#In all 5 cases, we reject the null hypothesis
apply(z, MARGIN = 2, FUN = ks.test, "punif", min=-1, max = 1)

#d
#apply polaroid across rows of z
y <- t(apply(pseud, MARGIN = 1, FUN = polaroid))
head(y)
#K-S test for R^2, which is the first element of each row squared
ks.test(y[,1]^2, "pchisq", df = 5)
#K-S test for the thetas
ks.test(y[,2], "punif", min = 0, max = pi)
ks.test(y[,3], "punif", min = 0, max = pi)
ks.test(y[,4], "punif", min = 0, max = pi)
ks.test(y[,5], "punif", min = 0, max = 2*pi)
#A plot for each theta, columns 2-5
y <- as.data.frame(y)
names(y) <- c("R", "Theta2", "Theta3", "Theta4", "Theta1")
melty <- melt(y)
g <- ggplot(data = melty, aes(x = value))
h <- g + geom_histogram(color = "blue") + facet_wrap(~variable, nrow = 2) + theme_light()
h


###########################################################
#Exercise 6
##a
loglike <- function(x,theta){
  n <- length(x)
  lth <- n*log(2*pi)
  i = 1
  while (i <= n) {
    lth <- lth + log(1 - cos(x[i] - theta))
    i <- i + 1
  }
  return(lth)
}

xi <- c(3.91, 4.85, 2.28, 4.06, 3.70, 4.04, 5.46, 3.53, 2.28,
        1.96, 2.53, 3.88, 2.22, 3.47, 4.82, 2.46, 2.99, 2.54, .52, 2.5)
theta1 <- seq(from = -1*pi, to = pi, by = .001)

plot(x = theta1, y = loglike(xi, theta1), type = 'l')

##b
optimize(f = loglike, interval = c(-pi, pi), x = xi, maximum = TRUE)

##c
newton <- function(fun, derf, x0, eps){
  iter <- 0
  repeat {
    iter <- iter + 1
    x1 <- x0 - fun(x0) / derf(x0)
    if (abs(x0 - x1) < eps || abs(fun(x1)) < 1e-10)
    break
    x0 <- x1
    cat("****** Iter. No: ", iter, "Current Iterate = ", x1, fill=T)
  }
return(x1)
}

dll <- function(x, theta){
  n <- length(x)
  d.l <- 0
  i <- 1
  while(i<=n){
    d.l <- d.l-sin(x[i] - theta)/ (1-cos(x[i] - theta))
    i<- i + 1
  }
  return(d.l)
} 

d2ll <- function(x, theta){
  n <- length(x)
  d2l <- 0
  i <- 1
  while(i<=n){
    d2l <- d2l- 1/(1-cos(x[i] - theta))
    i<- i + 1
  }
  return(d2l)
}

dllth <- function(theta){
  dll(xi, theta)
}
d2llth <- function(theta){
  d2ll(xi, theta)
}

newton(dllth, d2llth, x0 = 0, eps = .00001)


##d
newton(dllth, d2llth, x0 = -2, eps = .00001)
newton(dllth, d2llth, x0 = -2.7, eps = .00001)

###########################################################
#Exercise 7
##a
#Generate the men and women
men = rnorm(n= 100, 125, 25)
fm = rnorm(n= 100, 125, 15)
#combine them into a data frame
firstgen <- data.frame(men,fm)
#add columns for average height and generation to make it easier later
firstgen$ht <- apply(firstgen, MARGIN = 1, FUN = mean)
firstgen$gen <- rep(1, times = length(firstgen$men))
head(firstgen)

##b
#a function to generate the next generation
nextgen <- function(m,fem, gen){
  men <- c(sample(m, size = length(m), replace = FALSE))
  fems<- fem
  #make pairs
  pair <- cbind(men, fems)
  #next generation is the average of the pairs
  height <- apply(pair, MARGIN = 1, FUN = mean)
  #throw them into a data frame and output with generation #
  nextgen <- data.frame(cbind(height, height, height))
  names(nextgen) <- c("men", "fm", "ht")
  nextgen$gen <- rep(gen, times = length(m))
  return(nextgen)
}

##c
#generate 9 generations starting with our first
gen2 <- nextgen(firstgen$men, firstgen$fm, 2)
gen3 <- nextgen(gen2$men, gen2$fm, 3)
gen4 <- nextgen(gen3$men, gen3$fm, 4)
gen5 <- nextgen(gen4$men, gen4$fm, 5)
gen6 <- nextgen(gen5$men, gen5$fm, 6)
gen7 <- nextgen(gen6$men, gen6$fm, 7)
gen8 <- nextgen(gen7$men, gen7$fm, 8)
gen9 <- nextgen(gen8$men, gen8$fm, 9)

#put all of the generations into a data frame to make it easy to plot
#the histograms show average height clearly revert closer to the mean with each generation
allgens <- rbind.data.frame(firstgen, gen2, gen3, gen4, gen5, gen6, gen7, gen8, gen9)
g <- ggplot(data = allgens, aes(x=ht))
h <- g + geom_histogram() + facet_wrap(~factor(gen), nrow = 3)
h

###########################################
#Exercise 8
##a
#a function to read in the cluster data files
readclust <- function(x, sort.obs = FALSE){
  #read in the file
  readin <- readLines(x)
  #find the total # of clusters
  nclust <- as.numeric(substring(readin[1], first = regexpr("\\d", readin[1])))
  #find the location of the cluster headings in the file
  clust.titles <- which(regexpr("size", readin)>0)
  clustnum <- c(0)
  clustsize <- c(0)
  obs <- c(rep(NA, times = 2))
  grp <- c(rep(NA, times = 2))
  
  #a loop to find all of our clusters and their sizes in the file
  for (i in 1:(length(clust.titles))){
    tind <- clust.titles[i]
    clustnum[i] <- as.numeric(substring(readin[tind], first = 9, last = regexpr(";", readin[tind])-2))
    clustsize[i] <- as.numeric(substring(readin[tind], first = regexpr("size=", readin[tind])+5))
  }
  
  #a loop to assign a cluster number to each observation, and convert the observation to integer format
  for (i in 1:(length(clustnum))){
    clind <- clust.titles[i] + 1
    for (j in clind:(clind + clustsize[i])){
      grp[j-2*i-1] <- clustnum[i]
      obs[j-2*i-1] <- as.integer(readin[j])
    }
  }
  clusters <- data.frame(observation = obs, grp)
  #filter out NAs
  #also sort the observations
  cluster <- clusters %>%filter(observation >= 0) %>% filter(grp >= 0)
  if (sort.obs == T) cluster <- arrange(cluster, observation)
  return(cluster)
}


#function calls for each of the files
iris1 <- readclust("Iris1.out")
head(iris1)
iris2 <- readclust("Iris2.out")
head(iris2)
#can also sort them
iris1.sort <- readclust("Iris1.out", sort.obs = T)
head(iris1.sort)
iris2.sort <- readclust("Iris2.out", sort.obs = T)
head(iris2.sort)

#a quick check of our data
head(cbind(iris1.sort, iris2.sort$grp))
tail(cbind(iris1.sort, iris2.sort$grp))

#cross tabulation
#matches what's in the file
table("iris1" = iris1.sort$grp, "iris2" = iris2.sort$grp)













