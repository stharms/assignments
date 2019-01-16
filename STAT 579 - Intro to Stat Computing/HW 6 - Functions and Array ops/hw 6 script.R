setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 6")


#######################################
#Exercise 1######################

##a
#first, write it using a for loop
h.for <- function(x, n){
  h = 0
  for (i in 0:n){
    h = h + x^i
  }
  return(h)
}

##b
#next, try it with a while loop
h.while <- function(x, n){
  h = 0
  i = 0
  while (i < n + 1){
    h = h + x^i
    i = i+1
  }
  return(h)
}

#c
#calculate computing time for each of the combinations of x and n, for each function (8 total)
x.3af <- system.time(h.for(.3, 500))
x.3bf <- system.time(h.for(.3, 5000))
x.101af <- system.time(h.for(1.01, 500))
x.101bf <- system.time(h.for(1.01, 5000))
x.3aw <- system.time(h.while(.3, 500))
x.3bw <- system.time(h.while(.3, 5000))
x.101aw <- system.time(h.while(1.01, 500))
x.101bw <- system.time(h.while(1.01, 5000))

#report the "user" time to compute, which is the first element in the object returned by system.time()
loop.time <- rbind(x.3af, x.3bf, x.101af, x.101bf, x.3aw, x.3bw, x.101aw, x.101bw)[,1]

#d
#now, try it without loops
h.no <- function(x,n){
  ex <- c(0:n)
  mat <- rep(x, times = n + 1)
  sum(mat^ex)
}
x.3afn <- system.time(h.no(.3, 500))
x.3bfn <- system.time(h.no(.3, 5000))
x.101afn <- system.time(h.no(1.01, 500))
x.101bfn <- system.time(h.no(1.01, 5000))

noloop.time <-rbind(x.3afn, x.3bfn, x.101afn, x.101bfn)[,1]
#Compare computing times
loop.time
noloop.time

#######################################
#Exercise 2######################
#first an iterative function
lotka.volt <- function(x, y, bx, by, dx, dy){
  xt = x
  yt = y
  time = 0
  mat = matrix(ncol = 3 , nrow = 1)
  mat[1,]<- c(time, x, y)
  while(xt > 3900){
    time = time + 1
    xt = x + bx*x - dx*x*y
    yt = y + by*dx*x*y - dy*y
    x = xt
    y = yt
    mat <- rbind(mat, c(time, x, y))
  }
  lst <- list(mat[,1], mat[,2], mat[,3])
  names(lst) <- c("time", "prey", "predator")
  return(lst)
}

#run the function using specified variables
lv <- lotka.volt(4000, 100, .04, .1, .0005, .2)
#the prey don't last too long
lv
#plot the data
attach(lv)
plot(x = time, y = predator, type = "l", lwd = 1.5, lty = 1, col = "red", xlab = "Time Period", ylab = "Population count",
     main = "Predator vs. Prey Count",ylim = c(0,max(prey)+ 100))
lines(x = time, y = prey, lwd = 1.5, lty = 1, col = "blue")

#######################################
#Exercise 3######################

craps <- function(){
  roll <- sample(6, size = 2, replace = TRUE)
  sum.r <- sum(roll)
  sum.s <- 0
  outcome <- "Loss"
  nroll <- 1
  if (sum.r == 7 || sum.r == 11) outcome = "Win" else
    while (sum.s != sum.r){
      nroll <- nroll + 1
      roll <- sample(6, size = 2, replace = TRUE)
      sum.s <- sum(roll)
      if (sum.s == 7 || sum.s == 11) break
      if (sum.s == sum.r) outcome = "Win"
    }
  out <- list(sum.r, sum.s, nroll, outcome)
  names(out) <- c("First Roll", "Last Roll", "Nrolls","Outcome")
  return(out)
}

#Returns a list with the first roll, the last roll, and the outcome
craps()

#######################################
#Exercise 4######################
#The function creates a matrix of points, we can make it smoother by taking smaller intervals
#It then plots the line after we've created a matrix of points to plot
polarize <- function(start, end, by = .001){
  i<- start
  s <- matrix(ncol = 2, nrow = 1)
  s[1,] <- c(sqrt(i),2*pi*i)
  while(i <= end){
    s <- rbind(s, c(sqrt(i),2*pi*i))
    i <- i + by
  } 
  plot(x = s[,1], y = s[,2], type="l", xlab = "x", ylab = "y", main = "Plot of Polar Coordinates",
       xlim = c(min(s[,1]), max(s[,1])), ylim = c(min(s[,2]), max(s[,2])))
}

#Let's see the result
polarize(0 , 1, by=.0001)

#######################################
#Exercise 5######################
#start with this matrix of random numbers and another of variances
x <- matrix(rnorm(n = 500), ncol = 5)
varx <- var(x)
#Compute square roots of variances for each element
sds <- sqrt(diag(varx))
#Sweep it out by row
swept1 <- sweep(varx, MARGIN = 1, sds, FUN = "/")
#Then by column to get the correlation matrix
r <- sweep(swept1 , MARGIN = 2, sds, FUN = "/")
r
#We can compare it to R's built in function
#Should have the same result
cor(x)

#######################################
#Exercise 6######################
#Read in the data
meas <- aperm(array(scan(file = "etcpod_05-400_102307_1_trig01.dat"), dim = c(72,72, 150)), perm =c(2,1,3))

#a
#first collect the first 5 images
first5 <- meas[,,1:5]
#then average them
f5.avg <- apply(first5, MARGIN = c(1,2), FUN = mean)
dim(f5.avg)

#b
#We can sweep out each matrix in the array using the averages calculated above
bg.rm <- sweep(meas, MARGIN = c(1,2), f5.avg, FUN = "-")
dim(bg.rm)

#c
#I find the hottest pixel overall, then use the arrayIND function to find where it's located
hottest.index <- arrayInd(which.max(bg.rm), dim(bg.rm))
#The hottest pixel is in frame 93
hottest.index[1,3]

#d
#Create a function that we can apply to each frame
smooth.it <- function(m){
  oldmat <- m
  #Create a new frame by expanding the old one on each edge
  #I am just going to add a box of NAs around the image, then ignore them
  m.1 <- rbind(c(rep(NA, times = ncol(m))), m, c(rep(NA, times = ncol(m))))
  newmat <- cbind(c(rep(NA, times = nrow(m)+2)), m.1, c(rep(NA, times = ncol(m)+2)))
  #Now try to smooth it out
  for(i in 1:ncol(m)){
    for(j in 1:nrow(m)){
      oldmat[i,j] <- mean(c(newmat[i, j+1], newmat[i+1, j], newmat[i+1, j+1],
                          newmat[i+1, j+2], newmat[i+2, j+1]), na.rm = TRUE)
    }
  }
  return(oldmat)
}
test <- smooth.it(bg.rm[,,93])
#apply the function to our array of frames
smoothed.1 <- apply(bg.rm, MARGIN = 3, FUN = smooth.it)
#fit our new frames into an array
smoothed <- array(smoothed.1, dim=c(72,72,150))
dim(smoothed)

#Now we find the frame with the hottest pixel again, just as above
#I find the hottest pixel overall, then use the arrayIND function to find where it's located
hottest.sm <- arrayInd(which.max(smoothed), dim(smoothed))
#The hottest pixel is still in frame 93
hottest.sm[1,3]

#e
#reverse the rows so that image reads in correctly
smoothed.rev<- smoothed[,72:1,]
#a plot of our hottest frame shows where the hot spot is
image(z=smoothed.rev[,,93], col = topo.colors(72), axes = F, asp = 1)

image(z=smoothed[,,93], col = topo.colors(72), axes = F, asp = 1)


#######################
#Part b
#i
require(readxl)
#read in the data set
#I removed the quaternion columns and blank columns before reading in the data
prelim <- as.data.frame(read_excel(path = "PreliminaryData.xlsx"))
head(prelim)

#ii
#check to see if y is sorted in increasing order
is.unsorted(prelim$y)
#Hope to get the same number for each result (496)
#check to see if each x value is sorted within each y
check.sort <- tapply(X = prelim$x, INDEX = as.factor(prelim$y), FUN= is.unsorted)
unique(check.sort)
length(check.sort)
#check to see if each y value has the same number of corresponding x values
check.length <- tapply(X = prelim$x, INDEX = as.factor(prelim$y), FUN= length)
unique(check.length)
#Check to make sure all of the x values are unique within each y value
check.unique <- length(tapply(X = prelim$x, INDEX = as.factor(prelim$y), FUN= unique))
check.unique
#Check mins and maxes to make sure they're all the same
check.min <- tapply(X = prelim$x, INDEX = as.factor(prelim$y), FUN= min)
unique(check.min)
length(check.min)
check.max <- tapply(X = prelim$x, INDEX = as.factor(prelim$y), FUN= max)
unique(check.max)
length(check.max)

#iii
#We already confirmed that they were in order, so we can just fill the matrix by columns
mat.old <- matrix(prelim$`DD (old method)`, ncol = max(prelim$y)/10 + 1, nrow = max(prelim$x)/10 + 1)
mat.new <- matrix(prelim$`DD (L1 method)`, ncol = max(prelim$y)/10 + 1, nrow = max(prelim$x)/10 + 1)

#iv
#plot the images, with row order reversed so that image reads them correctly from top to bottom
image(z=mat.old[498:1,], x = c(0:497), y = c(0:495), col = topo.colors(120), main = "Old Method", xlab = "", ylab = "")
image(z=mat.new[498:1,], x = c(0:497), y = c(0:495), col = topo.colors(120), main = "New Method", xlab = "", ylab = "")

#############################################################
##############################################################
#Check to see if all possible measurements are present
#All increments are in 10s, so we can just run a nested for loop to check
check <- function(z){
  i <- 0; ct <- 1;
  missing.vals <- matrix(0, nrow = 1, ncol = 2)
  available.vals <- matrix(0, nrow = 1, ncol = 2)
  #hold y values constant on the outside loop, test for each x inside the loop
  for(i in seq(0,max(z[,2]), by = 10)){
    j<-0
    for(j in seq(0,max(z[,1]), by = 10)){
      if(!(z[ct,2] == i && z[ct, 1] == j)) missing.vals = rbind(missing.vals, c(j, i)) else available.vals = rbind(available.vals, c(j,i))
      j <- j+ 10
      ct <- ct + 1
    }
    i<- i+10
  }
  list.f <- list(missing.vals, available.vals, ct)
  names(list.f) <- c("missing", "avail", "count")
  return(list.f)
}
#Put all of our x values in to a matrix to test if we have all of them
testmat <- cbind(prelim[,1:2])
#run our function
#Hopefully the result is 
testted <- check(testmat)
dim(testted$avail)
testted$missing
testted$count