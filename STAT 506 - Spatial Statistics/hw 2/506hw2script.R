######################
library(geoR)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggthemes)
library(viridis)
soil <- read.table(file = "soil.txt", header = T)
head(soil)


#####
#a)
soil$lead <- log(soil$lead)
plot(soil$lead)
soilgeo <- as.geodata(soil, coords.col = 1:2, data.col = 3, covar.col = 4)
summary(soil)

#####
omndvar <- variog(soilgeo, option = "bin", uvec = seq(0, 1400, length = 30 ))
plot(omndvar, main = "Omnidirectional Variogram up to Lag Distance 1400")
#exponential model
vario.exp <- variofit(omndvar, ini.cov.pars = c(.6, 1100), cov.model = "exp", weights = "cressie", nugget = 0.1)
vario.exp
mle.exp = likfit(soilgeo,ini.cov.pars = vario.exp$cov.pars,nugget = vario.exp$nugget, cov.model = "exp")
mle.exp
#spherical model
vario.sph <- variofit(omndvar, ini.cov.pars = c(.6, 1100), cov.model = "spherical", weights = "cressie", nugget = 0.1)
vario.sph
mle.sph = likfit(soilgeo,ini.cov.pars = vario.sph$cov.pars,nugget = vario.sph$nugget, cov.model = "spherical")
mle.sph
#matern model
vario.mtr <- variofit(omndvar, ini.cov.pars = c(.6, 1100), cov.model = "matern", weights = "cressie", kappa = 1)
vario.mtr
mle.mtr = likfit(soilgeo,ini.cov.pars = vario.mtr$cov.pars,nugget = vario.mtr$nugget, cov.model = "matern", kappa = 1, fix.kappa = T)
mle.mtr
#compare BICs
c(mle.mtr$BIC, mle.exp$BIC, mle.sph$BIC)

###########
#b)
exp.xv <- xvalid(soilgeo, model = mle.exp, locations.xvalid = "all", reestimate = F)
exp.mspe <- (1/length(exp.xv$error))*sum((exp.xv$error)^2)
exp.mspe
sph.xv <- xvalid(soilgeo, model = mle.sph, locations.xvalid = "all", reestimate = F)
sph.mspe <- (1/length(sph.xv$error))*sum((sph.xv$error)^2)
sph.mspe
mtr.xv <- xvalid(soilgeo, model = mle.mtr, locations.xvalid = "all", reestimate = F)
mtr.mspe <- (1/length(mtr.xv$error))*sum((mtr.xv$error)^2)
mtr.mspe
#spherical has the lowest MSPE, just as it had smallest BIC. Conclude spherical is the best model
resids <- data.frame(cbind(exp.xv$error, sph.xv$error, mtr.xv$error))
names(resids) <- c("exponential", "spherical", "matern")
resids1 <- gather(resids)
b <- ggplot(data = resids1, aes(x = key, y = value))
b+ geom_boxplot() + ylab("residual value") + xlab("model") + ggtitle("Residual Box Plots")

mspes <- rbind(c(names(resids)), c(exp.mspe,sph.mspe, mtr.mspe))
mspes

#############
#c)
xrange <- range(soil[,1])
yrange <- range(soil[,2])
grid <- expand.grid(x = seq(xrange[1],xrange[2],l=50),y=seq(yrange[1],yrange[2],l=50))
kc.sph <- krige.conv(geodata=soilgeo, locations= grid, krige=krige.control(type.krige="OK",obj.model = mle.sph))
krigimage <- data.frame(x=grid$x,y = grid$y, pred=kc.sph$predict)
i <- ggplot(krigimage,mapping = aes(x = x, y = y))
h <- i + geom_tile(mapping = aes(fill = pred)) +  scale_fill_gradientn(colours = brewer.pal(n = 9, name = "YlOrBr"))+
    theme_bw() + xlab("x") + ylab("y")+ ggtitle("Kriging Predictions")
h
j <- i + geom_tile(mapping = aes(fill = pred)) +  scale_fill_gradientn(colours = inferno(250))+
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Predictions")
j

variances.sph = data.frame(x=grid$x,y = grid$y,var=kc.sph$krige.var)
v <- ggplot(variances.sph,mapping = aes(x = x, y = y))
v + geom_tile(mapping= aes(fill = var)) + scale_fill_gradientn(colours = brewer.pal(n = 9, name = "Purples")) +
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Variances")
#####################################
gridd <- expand.grid(x = seq(xrange[1],xrange[2],l=200),y=seq(yrange[1],yrange[2],l=200))
kc.sph <- krige.conv(geodata=soilgeo, locations= gridd, krige=krige.control(type.krige="OK",obj.model = mle.sph))
krigimage <- data.frame(x=gridd$x,y = gridd$y, pred=kc.sph$predict)
q <- ggplot(krigimage,mapping = aes(x = x, y = y))
r <- q + geom_tile(mapping = aes(fill = pred)) +  scale_fill_gradientn(colours = (brewer.pal(n = 9, name = "PiYG")))+
  theme_bw() + xlab("x") + ylab("y")+ ggtitle("Kriging Predictions")
r
u <- q + geom_tile(mapping = aes(fill = pred)) +  scale_fill_gradientn(colours = heat.colors(250))+
  theme_bw() + xlab("x") + ylab("y") + ggtitle("Kriging Predictions")
u
