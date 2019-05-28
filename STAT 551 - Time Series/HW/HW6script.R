library(itsmr)

#Problem 4.7
#estimate the model
sunspots <- itsmr::Sunspots
armaests <- yw(sunspots, p=2)
armaests$phi; armaests$sigma2

#periodogram of fitted model. Maximum is at frequency 52pi/289
plots(armaests) + abline(v=2*pi*26/289)
#corresponding period
period <- 2*pi/(52*pi/289)
period

#problem 4.8
#a
#specify model and plot the periodogram
arspec <- specify(ar=c(0,0,.99), sigma2=1)
plots(arspec)

#b
#the maximum is at approximately 2*pi/3, corresponding to period = 3

#c
simars <- sim(arspec, n= 60)
plotc(simars)

#d

#e
plotc(smooth.ma(simars, q=1))
