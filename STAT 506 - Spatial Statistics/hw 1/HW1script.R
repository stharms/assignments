install.packages("geoR")

######################
library(geoR)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
soil <- read.table(file = "soil.txt", header = T)
head(soil)


#####
#a)
g <- ggplot(data = soil, aes(x=x, y=y))
h <- g + geom_point(aes(colour = lead)) + scale_color_gradientn(colours =brewer.pal(n = 9, name = "RdYlGn")) +
  theme_bw()
h
h <- g + geom_point(aes(colour = lead)) + scale_color_gradientn(colours =rev(topo.colors(8))) +
  theme_dark()
h
#######
#b)
soilgeo <- as.geodata(soil, coords.col = 1:2, data.col = 3, covar.col = 4)
summary(soil)

omndvar <- variog(soilgeo, option = "bin", uvec = seq(0, 1400, length = 30 ))
plot(omndvar, main = "Omnidirectional Variogram up to Lag Distance 1400")

#nugget is 5000, sill is 17500, range ~ 800, partial sill 17500-5000=12500
#c)
vario.tri <- variofit(omndvar, ini.cov.pars = c(12500/800, 800), cov.model = "linear", weights = "cressie")
vario.tri
fit.tri = vario.tri$nugget + vario.tri$cov.pars[1]* (omndvar$u/vario.tri$cov.pars[2])
res.tri = sqrt(omndvar$n) * ((omndvar$v - fit.tri)/fit.tri)
plot(omndvar, main = "Triangular Variogram Model vs. Empirical") + lines(vario.tri)
plot(y=res.tri, x=omndvar$u, main = "Triangular Model Residuals", ylab = "Residuals", xlab= "Lag Distance")

########
vario.exp <- variofit(omndvar, ini.cov.pars = c(12500, 800), cov.model = "exp", weights = "cressie", nugget = 5000)
vario.exp
fit.exp = vario.exp$nugget + vario.exp$cov.pars[1] * (1 - exp(-omndvar$u/vario.exp$cov.pars[2]))
res.exp = sqrt(omndvar$n) * ((omndvar$v - fit.exp)/fit.exp)

plot(omndvar, main = "Exponential Variogram Model vs. Empirical") + lines(vario.exp)

plot(res.exp, main = "Exponential Model Residuals")
#####################
vario.sph <- variofit(omndvar, ini.cov.pars = c(12500, 800), cov.model = "spherical", weights = "cressie", nugget = 5000)
vario.sph
fit.sph = vario.sph$nugget + vario.sph$cov.pars[1] * (1.5*(omndvar$u/vario.sph$cov.pars[2])-.5*(omndvar$u/vario.sph$cov.pars[2])^3)
res.sph = sqrt(omndvar$n) * ((omndvar$v - fit.sph)/fit.sph)

plot(omndvar, main = "Spherical Variogram Model vs. Empirical") + lines(vario.sph)
plot(res.sph, main = "Spherical Model Residuals")
#####################
vario.matern <- variofit(omndvar, ini.cov.pars = c(12500, 800), cov.model = "matern", weights = "cressie", kappa = 1)
vario.matern
fit.mtr = vario.matern$nugget + vario.matern$cov.pars[1] * (1 - (1/gamma(1))*(abs(omndvar$u)/vario.matern$cov.pars[2])*
                                                              besselK((abs(omndvar$u)/vario.matern$cov.pars[2]), nu = 1))
res.mtr = sqrt(omndvar$n) * ((omndvar$v - fit.mtr)/fit.mtr)

plot(omndvar, main = "Matern Variogram Model vs. Empirical") + lines(vario.matern)

plot(res.mtr, main = "Spherical Model Residuals")

############################################
###########
#d)
var.direct <- variog4(soilgeo, uvec = seq(0, 1400, length = 30), direction = c(0,pi/4,pi/2,3*pi/4))
plot(var.direct)

var.direct

