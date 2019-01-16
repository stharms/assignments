rm(list=ls())
######################
library(geoR)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(viridis)
library(colorRamps)
library(reshape2)
########################
####
#3)
##a)
moist<- read.table("moisture.txt", header = T)
names(moist) <- c("x", "y", "density", "moisture")
summary(moist$moisture)
plot(moist$moisture)
hist(moist$moisture)
hist(log(moist$moisture))
plot(log(moist$moisture))
plot(moist$density)
hist(moist$density)
plot(y = log(moist$moisture), x = moist$density) + abline(lm(moist$moisture)~moist$density)
moist$logm <- log(moist$moisture)
moist.rm <- moist[-75,]
plot(log(moist.rm$moisture))
#no transformation needed, although there is an outlier
g <- ggplot(data = moist, aes(x=x, y=y))
i <- g + geom_tile(mapping = aes(fill = moisture)) +  scale_fill_gradientn(colours = topo.colors(250))+
  theme_bw() + xlab("x") + ylab("y")+ ggtitle("moisture levels")
i
gg <- ggplot(data = moist.rm, aes(x=x, y=y))
ii <- gg + geom_tile(mapping = aes(fill = moisture)) +  scale_fill_gradientn(colours = inferno(25))+
  theme_bw() + xlab("x") + ylab("y")+ ggtitle("moisture levels")
ii
ii+scale_fill_gradientn(colours = inferno(250))
f+scale_fill_gradientn(colours = viridis(280))
c+scale_fill_gradientn(colours = matlab.like(80))
#create geodata
moistgeo <- as.geodata(moist, coords.col = 1:2, data.col = 5, covar.col = 3)
summary(moistgeo)
moistgeo.rm <- as.geodata(moist.rm, coords.col = 1:2, data.col = 5, covar.col = 3)

var.direct <- variog4(moistgeo, uvec = seq(0, 10, length = 15 ), direction = c(0,pi/4,pi/2,3*pi/4))
plot(var.direct)

#estimate empirical variogram
omndvar <- variog(moistgeo, trend = ~density, option = "bin", uvec = seq(0, 13, length = 75 ))
plot(omndvar, main = "Omnidirectional Variogram up to Lag Distance 25")
omnd.rm <- variog(moistgeo.rm, trend = ~density, option = "bin", uvec = seq(0, 13, length = 75 ))
plot(omnd.rm, main = "Omnidirectional Variogram up to Lag Distance 25")

notrendvar <- variog(moistgeo, option = "bin", uvec = seq(0, 13, length = 75 ))
plot(notrendvar, main = "Omnidirectional Variogram up to Lag Distance 25")
#nugget is .00055, range is 10, partial sill is ~.001

##exponential model
vario.exp <- variofit(omndvar, cov.model = "exp", weights = "cressie", nugget = .0048, fix.nugget=F, ini.cov.pars = c(.001, 10))
vario.exp
fit.exp = vario.exp$nugget + vario.exp$cov.pars[1] * (1 - exp(-omndvar$u/vario.exp$cov.pars[2]))
res.exp = sqrt(omndvar$n) * ((omndvar$v - fit.exp)/fit.exp)
plot(omndvar, main = "Exponential Variogram Model vs. Empirical") + lines(vario.exp)
plot(res.exp, main = "Exponential Model Residuals")

#spherical model
vario.sph <- variofit(omndvar, cov.model = "spherical", weights = "cressie", nugget = .0048, fix.nugget=F,ini.cov.pars = c(.002, 10))
vario.sph
fit.sph = vario.sph$nugget + vario.sph$cov.pars[1] * (1.5*(omndvar$u/vario.sph$cov.pars[2])-.5*(omndvar$u/vario.sph$cov.pars[2])^3)
res.sph = sqrt(omndvar$n) * ((omndvar$v - fit.sph)/fit.sph)
plot(omndvar, main = "Spherical Variogram Model vs. Empirical") + lines(vario.sph)
plot(res.sph, main = "Spherical Model Residuals")

vario.sph.rm <-variofit(omnd.rm, cov.model = "spherical", weights = "cressie", nugget = .0044, fix.nugget=F,ini.cov.pars = c(.001, 7))
vario.sph.rm
plot(omnd.rm, main = "Spherical Variogram Model vs. Empirical") + lines(vario.sph.rm)

#gaussian model
vario.gauss <- variofit(omndvar, cov.model = "gaussian", weights = "cressie", nugget = .005, fix.nugget=F,ini.cov.pars = c(.002, 10))
vario.gauss
fit.gs = vario.gauss$nugget + vario.gauss$cov.pars[1] * (1 - exp(-1*(omndvar$u)^2/((vario.gauss$cov.pars[2])^2)))
res.gs = sqrt(omndvar$n) * ((omndvar$v - fit.gs)/fit.gs)
plot(omndvar, main = "Gaussian Variogram Model vs. Empirical") + lines(vario.gauss)
plot(res.gs, main = "Gaussian Model Residuals")



#matern model
vario.matern <- variofit(omndvar, cov.model = "matern", weights = "cressie",nugget = .0055, fix.nugget=F, kappa = .75, fix.kappa=F)
vario.matern
fit.mtr = vario.matern$nugget + vario.matern$cov.pars[1] * (1 - (1/(2^(vario.matern$kappa-1)*gamma(vario.matern$kappa)))*(abs(omndvar$u)/vario.matern$cov.pars[2])^(vario.matern$kappa)*
                                                              besselK((abs(omndvar$u)/vario.matern$cov.pars[2]), vario.matern$kappa))
res.mtr = sqrt(omndvar$n) * ((omndvar$v - fit.mtr)/fit.mtr)
plot(omndvar, main = "Matern Variogram Model vs. Empirical") + lines(vario.matern, col="blue")
plot(res.mtr, main = "Matern Model Residuals")

notrend.matern <- variofit(notrendvar, cov.model = "matern", weights = "cressie",nugget = .00055, kappa = 3.5, fix.kappa=T)
notrend.matern
notrend.mtr = vario.matern$nugget + vario.matern$cov.pars[1] * (1 - (1/(2^(vario.matern$kappa-1)*gamma(vario.matern$kappa)))*(abs(omndvar$u)/vario.matern$cov.pars[2])^(vario.matern$kappa)*
                                                              besselK((abs(omndvar$u)/vario.matern$cov.pars[2]), vario.matern$kappa))
res.mtr = sqrt(omndvar$n) * ((omndvar$v - fit.mtr)/fit.mtr)
plot(omndvar, main = "Matern Variogram Model vs. Empirical") + lines()
plot(res.mtr, main = "Matern Model Residuals")




######
#b)
#fit model with covariates
trendmat <- trend.spatial(trend = ~moistgeo$coords[,1]+moistgeo$covariate[,1],moistgeo)
trendmat.rm <- trend.spatial(trend = ~moistgeo.rm$coords[,1]+moistgeo.rm$covariate[,1],moistgeo.rm)

sph.fit = likfit(moistgeo, trend= trendmat, fix.nugget = F, nugget =.0048, ini = c(.002,10),
                cov.model = "spherical", lik.method = "REML")
sph.fit.nt = likfit(moistgeo, trend= ~moistgeo$covariate[,1], fix.nugget = F, nugget =.0048, ini = c(.002,10),
                    cov.model = "spherical", lik.method = "REML")
sph.fit.nc = likfit(moistgeo, fix.nugget = F, nugget =.0048, ini = c(.002,10),
                    cov.model = "spherical", lik.method = "REML")
sph.fit.rm = likfit(moistgeo.rm, trend= ~moistgeo.rm$covariate[,1], fix.nugget = F, nugget =.0048, ini = c(.002,10),
                    cov.model = "spherical", lik.method = "REML")

### Test the hypothesis that the coefficient of density is zero: get the t-statistics and the p-value.
trend.coef = sph.fit$beta 
names(trend.coef) = c("Intercept","density")
trend.coef
trend.cov = sph.fit$beta.var
trend.cov
t.density = trend.coef[3]/sqrt(trend.cov[3,3])
cat("t-value =",t.density,"\n")
p.density = 2*(1 - pnorm(abs(t.density)))
cat("p-value =",p.density,"\n")

#Predictions
xrange <- range(moist[,1])
yrange <- range(moist[,2])
grid <- expand.grid(x = seq(xrange[1],xrange[2],l=30),y=seq(yrange[1],yrange[2],l=75))

#with trend and covariate
kc.sph.t <- krige.conv(geodata=moistgeo, locations= grid, krige=krige.control(type.krige = "OK", obj.model = sph.fit))
predictions <- exp(kc.sph.t$predict)
krigimage.t <- data.frame(x=grid$x,y = grid$y, predt=predictions)
a <- ggplot(krigimage.t,mapping = aes(x = x, y = y))
c <- a + geom_tile(mapping = aes(fill = predt)) +  scale_fill_gradientn(colours = topo.colors(250))+
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Predictions with covariate and trend")
c
#with covariate but no trend
kc.sph.nt <- krige.conv(geodata=moistgeo, locations= grid, krige=krige.control(type.krige = "OK", obj.model = sph.fit.nt))
krigimage.nt <- data.frame(x=grid$x,y = grid$y, preds=exp(kc.sph.nt$predict))
aa <- ggplot(data=krigimage.nt,mapping = aes(x = x, y = y))
cc<- aa + geom_tile(mapping = aes(fill = preds)) +  scale_fill_gradientn(colours = topo.colors(250))+
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Predictions with covariate but no trend")
cc
c
#with covariate but no trend, outlier removed
kc.sph.t.rm <- krige.conv(geodata=moistgeo.rm, locations= grid, krige=krige.control(type.krige = "OK", obj.model = sph.fit.rm))
predictions.rm <- exp(kc.sph.t.rm$predict)
krigimage.t.rm <- data.frame(x=grid$x,y = grid$y, predt=predictions.rm)
d <- ggplot(krigimage.t.rm,mapping = aes(x = x, y = y))
f <- d + geom_tile(mapping = aes(fill = predt)) +  scale_fill_gradientn(colours = topo.colors(250))+
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Predictions with covariate, outlier removed")
f
#without trend or covariate
kc.sph <- krige.conv(geodata=moistgeo, locations= grid,krige=krige.control(type.krige="OK",obj.model = sph.fit.nc))
krigimage <- data.frame(x=grid$x,y = grid$y, pred=exp(kc.sph$predict))
q <- ggplot(krigimage,mapping = aes(x = x, y = y))
u <- q + geom_tile(mapping = aes(fill = pred)) +  scale_fill_gradientn(colours = topo.colors(250))+
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Predictions with no trend or covariate")
u

###prediction differences
msd <- mean((exp(kc.sph$predict) - exp(kc.sph.nt$predict))^2)
msd
msd/sd(moist$moisture)
msd/var(moist$moisture)
###########
sph.fit.nt$nospatial$beta.ns
sph.fit.nt$beta
nospat <- lm( formula=moist$logm~moist$density)
summary(nospat)$coefficients
####



 ##################
#4)
##a)
arse <- read.table("arsenic.txt", header = T)
missing = (arse$depth_m == 0) | (arse$year_made == -1)
arsenic = arse[!missing,]
arsenic$logar <- log(arsenic$arsenic)
as_geo  = as.geodata(obj = arsenic, coords.col = 2:1, data.col = 8)
arvar <-variog(as_geo, uvec = seq(0.1,3.5,length.out = 50))
plot(arvar, main = "Omnidirectional Variogram up to Lag Distance 4")
arse.mtr = likfit(as_geo, nugget = 2, ini.cov.pars = c(5,1), kappa = .25,
                 cov.model = "matern", lik.method = "REML")
jittered <- jitterDupCoords(as_geo, max = .0002)

arse.mtr <- variofit(arvar,ini.cov.pars = c(5,1), cov.model = "matern", weights = "cressie",nugget = 2, kappa = .25, fix.kappa = T)
arse.mtr

###########################################################

#xvalid() in geoR doesn't report parameter estimates, so we can just write our own function to do cross-validation
arsesim <- function(rawd,ntimes){
  #set up some matrices to get us 
  options(warn = -1, max.print = 15)
  mtr.mspe = exp.mspe = exp.sill = exp.range = exp.nugget = mtr.sill = mtr.range = mtr.nugget <- matrix()
  modeltype <- c(rep("exp", times = ntimes),rep("matern", times = ntimes))
  number <- c(rep(1:ntimes, times = 2))

  for(i in 1:ntimes){
  set.seed(1234*i^2)
  subsample = sample.int(nrow(rawd), size = length(rawd[,1])/2)
  geot = jitterDupCoords(as.geodata(obj = rawd[-subsample,],coords.col = 2:1 ,data.col = 8),max = .0002)
  geoval = jitterDupCoords(as.geodata(obj = rawd[subsample,],coords.col = 2:1 ,data.col = 8),max = .0002)
  arvar <-variog(geot, uvec = seq(0.1,3.5,length.out = 50))
  pred.locs <- rawd[subsample,2:1]
  ar.exp <- variofit(arvar,ini.cov.pars = c(5,1), cov.model = "exponential", weights = "cressie",
                     nugget = 2)
  ar.mtr <- variofit(arvar,ini.cov.pars = c(5,1), cov.model = "matern", weights = "cressie",
           nugget = 2, kappa = .25, fix.kappa = T)
  pred.exp = krige.conv(geodata=geot, locations= pred.locs,
                              krige=krige.control(type.krige="OK",obj.model = ar.exp))
  pred.mtr = krige.conv(geodata=geot, locations= pred.locs,
                        krige=krige.control(type.krige="OK",obj.model = ar.mtr))
  exp.mspe[i] <- mean((pred.exp$predict-geoval$data)^2)
  mtr.mspe[i] <- mean((pred.mtr$predict-geoval$data)^2)
  exp.sill[i] <- ar.exp$cov.pars[1]
  exp.range[i] <- ar.exp$cov.pars[2]
  mtr.sill[i] <- ar.mtr$cov.pars[1]
  mtr.range[i] <- ar.mtr$cov.pars[2]
  mtr.nugget[i] <- ar.mtr$nugget
  exp.nugget[i] <- ar.exp$nugget
  }
  outlist <- tibble(number,modeltype,c(exp.mspe,mtr.mspe), c(exp.sill,mtr.sill),c(exp.range,mtr.range), c(exp.nugget, mtr.nugget))
  names(outlist) <- c("number","model", "mspe", "sill", "range", "nugget")
  return(outlist)
}

sims <- arsesim(arsenic,20)
sims <- arsesim(arsenic,2)
sims %>% arrange(number) %>% head()
#Some comparisons of the simulations of the 2 models
bb <- ggplot(data = sims, aes(x = factor(model)))
bb + geom_boxplot(aes(y=mspe, fill = factor(model))) + ylab("MSPE") + xlab("model") + ggtitle("MSPE Box Plots")
bb + geom_boxplot(aes(y=sill, fill = factor(model))) + ylab("Sigma^2") + xlab("model") + ggtitle("Partial Sill Box Plots")
bb + geom_boxplot(aes(y=range, fill = factor(model))) + ylab("Phi") + xlab("model") + ggtitle("Range Box Plots")
bb + geom_boxplot(aes(y=nugget, fill = factor(model))) + ylab("Tau") + xlab("model") + ggtitle("Nugget Box Plots")
sims %>% group_by(model) %>% summarize(sill.mean = mean(sill), sill.sd = sd(sill), range.mean = mean(range), range.sd = sd(range), nugget.mean = mean(nugget), nugget.sd = sd(nugget))

