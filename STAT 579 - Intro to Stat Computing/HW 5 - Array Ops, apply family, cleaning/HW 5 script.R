rm(list = ls())
setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 5")

##############Exercise 1#####################
#############################################
####a########
genes <- data.frame(read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/diurnaldata.csv", sep = ",", header = TRUE))
dim(genes)
names(genes) <- c("Probe", "0h", "1h", "2h", "4h", "8h", "12h", "13h","14h", "16h", "20h", "24h",
                  "0h.", "1h.", "2h.", "4h.", "8h.", "12h.", "13h.", "14h.", "16h.", "20h.", "24h.")

###########
#b#
######
#Create an array
trial1 <- as.matrix(cbind(genes[,2:12]), nrow = 22810, ncol = 11, byrow = T)
trial2 <- as.matrix(cbind(genes[,13:23]),nrow = 22810, ncol = 11, byrow = T)
genearray <- array( data = c(trial1, trial2), dim = c(22810, 11, 2))

#means for each probe across the 2 measurements
means <- as.matrix(x = apply(genearray, MARGIN = c(1,2), FUN = mean), nrow = 22810)
dim(means)

####
#c
##i
means1 <- apply(means, MARGIN = 1, FUN = mean)
totals <- data.frame(genes$Probe, as.numeric(means1)); names(totals) <- c("gene", "average")
head(totals)

##ii
#replicated matrix of means of means
repped <- matrix(data = means1, nrow = 22810, ncol = 11)
#remove mean effect from averages calculated in part (b)
meaneffect <- means - repped

##iii
#standard deviation of each row
stdevs <- apply(X = means, MARGIN = 1, FUN = sd)
#scaled measurements are the standardized genes
ssss <- matrix(data = stdevs, nrow = 22810, ncol = 11)
scaled <- meaneffect/ ssss

###
#d
#read in data
measures <- read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/micromeans.dat", sep = "", header = F)
#standardize as above
meansm <- apply(measures, MARGIN = 1, FUN = mean)
#replicated matrix of means of means
reppedm <- matrix(data = meansm, nrow = 20, ncol = 11)
#remove mean effect from averages calculated in part (b)
meaneffectm <- measures - reppedm
#standard deviation of each row
stdevsm <- apply(X = measures, MARGIN = 1, FUN = sd)
#scaled measurements are the standardized genes
scaledm <- as.matrix(meaneffectm/ matrix(data = stdevsm, nrow = 20, ncol = 11), nrow = 20, ncol = 11)


###
#e
#set up arrays of replicated data sets, check dimensions
arrayg <- array(data = scaled, dim = c(22810, 11,20))
arraym <- aperm(array(data = c(scaledm), dim = c(20,11,22810)), perm = c(3,2,1))
dim(arraym); dim(arrayg)
#combine them into a 4-D array
arrayc <- array(data = c(arrayg, arraym), dim = c(22810,11,20,2))
#calculate euclidean distance for each point
#first, a function since dist() is not good
euclid <- function(x){
  eu <- (x[,1]-x[,2])^2
  sum <- sum(eu)
  return(sqrt(sum))
}
#apply the function to each row/3rd dimension to get the appropriate result
edist <- apply(arrayc, MARGIN = c(1,3), FUN = euclid)

#find the index of the minimum distance in each row
min.idx <- apply(edist, MARGIN = 1, FUN=which.min)
#frequency table
table(min.idx)
#pie chart of the frequencies
pie(table(min.idx), labels = c(as.character(1:20)))

#######################################################################################
#####################
###Exercise 2#########
##a#
ziptrain1 <- as.matrix(read.table(file = "ziptrain.dat", header = F, sep = ""))
ziptrain2 <- array(data = t(ziptrain1), dim = c(16,16,2000))
ziptrain <- aperm(array(data = ziptrain2, dim = c(16,16,2000)), perm = c(2,1,3))

##b#
###i
par(mar = rep(0.05, 4))
image(z=ziptrain2[,16:1,2],col = rev(gray(0:31/31)),axes = F )

##ii
dev.new(width = 6.5, height = 5.2)
par(mfrow = c(40,50), pin=c(6.5,5.2))
pics <- function(q){
  par(mar = rep(0.05, 4))
  image(z = q[1:16,16:1],col = rev(gray(0:31/31)),axes = F )
}
apply(ziptrain2, MARGIN = 3, FUN = pics)

##c#
digits <- read.table(file = "zipdigit.dat", header = F, sep = "")
#create index matrix of the 10 digits
dig <- 0:9
indexing <- function(v){
  indexmat <- matrix(ncol = 10, nrow= max(table(v)))
  for (y in 0:9){
    indexmat[,y+1] <- c(which(v == y), rep(NA, times = max(table(v) - length(which(v==y)))))
  }
  return(indexmat)
}
indexes <- indexing(digits)

#Create looping functions for means and standard deviations
means <- function(input, index){
  meanarray <- array(dim = c(16,16,10))
  for (j in 0:9){
    meanarray[,,j+1] <- apply(input[,,c(index[,j+1])], MARGIN = c(1,2), FUN = mean, na.rm = TRUE)
  }
  return(meanarray)
}

stds <- function(input, index){
  stdarray <- array(dim = c(16,16,10))
  for (j in 0:9){
    stdarray[,,j+1] <- apply(input[,,c(index[,j+1])], MARGIN = c(1,2), FUN = sd, na.rm = TRUE)
  }
  return(stdarray)
}

#apply the function to the digits
digmeans <- means(ziptrain2, indexes)
stdarray <- stds(ziptrain2, indexes)

#plot the stds images (should be 1 for each digit)
par(mfrow = c(3,4))
apply(stdarray, MARGIN = 3, FUN = pics)


##d
#First, remove mean effect from each digit
rm.means <- function(input, index, means){
  newarray <- array(dim = c(16,16,2000))
  for (j in 1:2000){
    digindex <- index[j,1]
    newarray[,,j] <- input[,,j] - means[,,digindex+1]
  }
  return(newarray)
}

mean.removed <- rm.means(ziptrain2, digits, digmeans)
records <- matrix(mean.removed, nrow = 2000, ncol = 256, byrow = T)

decompose <- function(input, k, index, means){
  decomp <- svd(input)
  Dk <- diag(c(c(decomp$d[1:k]), rep(0, times = 256-k)))
  Y <- decomp$u%*%Dk%*%t(decomp$v)
  return(Y)
}

addback <- function(original, index, means){
  newarray1 <- array(dim = c(16,16,2000))
  z<- array(data = t(original), dim = c(16,16,2000))
  for (j in 1:2000){
    digindex <- index[j,1]
    newarray1[,,j] <- z[,,j] + means[,,digindex+1]
  }
  return(newarray1)
}
#newimages <- function(input, decomp){
#  addback <- input + decomp
#  newimages <- array(data = t(addback), dim = c(16,16,2000))
#  return(newimages)
#}

#First with k=25 eigenvalues

new25 <- decompose(records, k= 25)
image25 <-addback(new25, digits, digmeans)

par(mfrow = c(40,50))
apply(image25, MARGIN = 3, FUN = pics)

#Then with k=50 eigenvalues
new50 <- decompose(records, k=50)
image50 <- addback(new50,digits, digmeans)

par(mfrow = c(40,50))
apply(image50, MARGIN = 3, FUN = pics)

#And finally with 75 eigenvalues
new75 <- decompose(records, k=75)
image75 <- addback(new75,digits, digmeans)

par(mfrow = c(40,50))
apply(image75, MARGIN = 3, FUN = pics)




#####################################################################################################################
################################
####Exercise 3################
#######################
states <- data.frame(state.x77, state.region)
attach(states)
#a
#using tapply
percapinc.t <- tapply(Income, INDEX = state.region, FUN = mean)
percapinc.t
#using aggregate
percapinc.a <- aggregate(Income, by = list(state.region), FUN = mean)
percapinc.a

#b
#using tapply
maxilliterate.t <- tapply(Illiteracy, INDEX = state.division, FUN=max )
maxilliterate.t
#using aggregate
maxilliterate.a <- aggregate(Illiteracy, by = list(state.division), FUN=max)
maxilliterate.a

#c
#The easiest way would be to just use a frequency table
table(state.region)
#using tapply
regioncount.t <- tapply(state.name, INDEX = state.region, FUN= length )
regioncount.t
#using aggregate
regioncount.a <- aggregate(state.name, by = list(state.region), FUN = length)
regioncount.a

#d
#using tapply
names.t <- tapply(state.name, INDEX = state.division, FUN=list)
names.t
#using aggregate
names.a <- aggregate(state.name, by=list(state.division), FUN = list)
names.a <- aggregate(formula = state.name~state.division, data=states, FUN = list)
names.a

#e
#first create our variable
state.size <- cut(x = state.x77[, "Population"], breaks = c(0, 2000, 10000, Inf), 
                  labels = c("Small", "Medium", "Large"))
#using tapply
pops.t <- tapply(Population, INDEX = list(state.size,state.region), FUN=median)
pops.t
#using aggregate
pops.a <- aggregate(Population, by=list(state.size, state.region), FUN = median)
pops.a

#####################################################################################################################
################################
####Exercise 4################
#######################
cars <- data.frame(mtcars)

##a
#using aggregate
mada <- apply(cars, MARGIN = 2, FUN = mad)
mada

##b
#using apply twice and a sweep function to clear it out
meds <- apply(cars, 2, median)
swept <- sweep(cars, 2, meds)
mad2 <- 1.4826 * apply(abs(swept), 2, median)
mad2



