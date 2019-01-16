library("spatstat")
library("RColorBrewer")
library(ggplot2)
library(viridis)
##########
#2)
bramble <- scan(file="brambles3.txt", sep = ',')
bramble <- bramble[!is.na(bramble)]
length(bramble) ; tail(bramble)
l <- seq(1:718); eventries <- which(l%%2==0)
x <- 9*bramble[-eventries]
y <- 9*bramble[eventries]
brambles <- as.data.frame(cbind(x,y))
canes <- ppp(x, y, window=owin(c(0,9),c(0,9)))

#a plot of the data
plot(x = x, y = y)

#plot of the density
plot(density(canes, diggle=T), main = "Estimated Intensity")
image(density(canes, diggle=T), main = "Estimated Intensity", col = brewer.pal(n = 9, name = "Oranges"))
#K-function
Kc <- Kest(canes, correction="Ripley")
plot(Kc, main="Estimated K function")
plot(Kc,  iso - theo ~ r, main="Deviation from CSR")
abline(h=0, lty=2, col="gray40")
#L-function
Lc <- Lest(canes)
plot(Lc, main = "Estimated L Function")
plot(Lc,  iso - theo ~ r, main="Deviation from CSR")
abline(h=0, lty=2, col="gray40")
#values are large near 0, indication of some clustering



##############
#3)
redwoods <- scan(file = "redwood.txt")
l.r <- seq(1:length(redwoods)); eventries.r <- which(l.r%%2==0)
x.r <- 23*redwoods[-eventries.r] ; y.r <- 23*redwoods[eventries.r];
redwood <- as.data.frame(cbind(x.r, y.r))
trees <- ppp(x.r, y.r, window=owin(c(0,23),c(0,23)))

#a plot of the data
plot(x = x.r, y = y.r)
#plot of the density
plot(density(trees, diggle=T), main = "Estimated Intensity", col = plasma(10))
#K-function
Ks <- Kest(trees, correction="Ripley")
plot(Ks, main="Estimated K function")
plot(Ks,  iso - theo ~ r, main="Deviation from CSR")
abline(h=0, lty=2, col="gray40")
#L-function
Ls <- Lest(trees)
plot(Ls, main = "Estimated L Function")
plot(Ls,  iso - theo ~ r, main="Deviation from CSR")
abline(h=0, lty=2, col="gray40")

