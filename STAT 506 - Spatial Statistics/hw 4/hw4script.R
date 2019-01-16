library(maptools)
library(spdep)
library(fields)
library(Matrix)
#######
plot.poly <- function (xx.polylist, aux.var, intervals,
                       legend.x, legend.y, ...)
{
  if (missing(aux.var)) {
    plot(xx.polylist, ...)
    ##forcefill=FALSE, ...)
  }
  else {
    cols <- grey(seq(0.2, 0.8, length=length(intervals)))
    
    the.cols <- cols[findInterval(aux.var, intervals)]
    plot(xx.polylist, col=the.cols, ...)
    
    ys <- sort(seq(legend.y[1], legend.y[2], len=length(intervals)))
    
    image(range(legend.x), ys,
          rbind(intervals, intervals), col=cols, add=T)
    
    text(min(legend.x), ys, intervals, pos=2, cex=0.9)
  }
  
  invisible()
}
###########
sat <- read.table("sat.txt", header=TRUE)
str(sat)
load("US_states.RData")
where.is.state <- pmatch(tolower(US$STATE_NAME), tolower(sat$name))
W <- as.matrix(read.table("sat_proximity.txt", header=F))
par(mfrow=c(1,1), cex=0.75, mar=c(4,4,1,1), mgp=c(2,0.5,0), bty="L")
plot(0, 0, xlim=c(-125, -68), ylim=c(25,50), type="n",
     xlab="", ylab="", main="SAT verbal scores for 1999")
plot.poly(US, sat$vscore[where.is.state], seq(450, 600, 25),
          legend.x=c(-71, -70), legend.y=c(25, 30), add=TRUE)
par(mfrow=c(1,1), cex=0.75, mar=c(4,4,1,1), mgp=c(2,0.5,0), bty="L")
plot(sat$pc, sat$vscore,
     xlab="Percentage taking the exam",
     ylab="Verbal score")
###################
satprox <- as.matrix(read.table("sat_proximity.txt", header=F))
head(sat)
#simple linear regression model
slr <- lm(vscore~pc, data=sat)
#summarize coefficients
summary(slr)$coefficients
#get residuals
residslr <- slr$residuals
#choropeth plot
plot(0, 0, xlim=c(-125, -68), ylim=c(25,50), type="n",
     xlab="", ylab="", main="SAT verbal scores for 1999")
plot.poly(US, residslr[where.is.state], seq(-29, 27, 25),
          legend.x=c(-71, -70), legend.y=c(25, 30), add=TRUE)
#qqplot
qqnorm(residslr)
#####################
neighblist <- mat2listw(satprox)
ptest <- moran.mc(residslr, neighblist, nsim = 1000)
ptest$res
hist(ptest$res)
abline(v = ptest$res[1001], lwd = 3)
###############
#spatial auto model, including intercept, CAR
spfit <- spautolm(formula = vscore ~ pc, listw = neighblist, family = "CAR", data=sat)
summary(spfit)
###############
sarfit <- spautolm(formula = vscore ~ pc, listw = neighblist, family = "SAR", data=sat)
summary(sarfit)

car.resids <- spfit$fit$residuals
sar.resids <- sarfit$fit$residuals
plot(0, 0, xlim=c(-125, -68), ylim=c(25,50), type="n",
     xlab="", ylab="", main="SAT verbal scores for 1999")
plot.poly(US, car.resids[where.is.state], seq(from = -26, to = 20,by = 2),
          legend.x=c(-71, -70), legend.y=c(25, 30), add=TRUE)
plot.poly(US, sar.resids[where.is.state], seq(from = -26, to = 21,by = 2),
          legend.x=c(-71, -70), legend.y=c(25, 30), add=TRUE)
as.numeric(ptest$res[1001])

head(sat)
