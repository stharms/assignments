setwd("~/Documents/Microsoft Word/Grad School/STAT 520/HW 3")
#read in data and remove 1 na case
gulls <- data.frame(read.csv(file="gullsdata.csv", header=T))
gulls <- gulls[-(which(is.na(gulls$bm))),]
#a histogram (for marginal means)
hist(gulls$bm, breaks=11)

#Plot Response vs. Explanatory, and a log transformation
plot(gulls$bm~(gulls$oxy),xlab="OXY", ylab="Body Mass (g)")
plot(gulls$bm~log(gulls$oxy),xlab="Log(OXY)", ylab="Body Mass (g)")
plot(gulls$bm[gulls$sex==1]~log(gulls$oxy[gulls$sex==1]))
plot(gulls$bm[gulls$sex==2]~log(gulls$oxy[gulls$sex==2]))
plot(gulls$bm[gulls$sex==1]~log(gulls$hb[gulls$sex==1]))
plot(gulls$bm[gulls$sex==2]~log(gulls$hb[gulls$sex==2]))
#residual plot
resids <- lm(data=gulls, formula=bm~log(oxy))$residuals
plot(resids~log(gulls$oxy), ylab="Residuals", xlab="Log(OXY)") + abline(h=0,lty=2)

#Pick the random component
#bin the data
binnedbm <- data.frame(bm=gulls$bm, cats = cut(gulls$bm, breaks=quantile(gulls$bm, prob=seq(0.1,1,by=0.1)), include.lowest=T))
bmSD<- tapply(binnedbm$bm, binnedbm$cats, sd)
bmMean <- tapply(binnedbm$bm, binnedbm$cats, mean)
binnedbmm <- data.frame(bm=gulls$bm[gulls$sex==1], cats = cut(gulls$bm[gulls$sex==1], breaks=quantile(gulls$bm[gulls$sex==1], prob=seq(0.1,1,by=0.1)), include.lowest=T))
bmmSD <-tapply(binnedbmm$bm, binnedbmm$cats,sd)[-5]; bmmSD<-bmmSD[-9]
bmmMean <-tapply(binnedbmm$bm, binnedbmm$cats,mean)[-5]; bmmMean <-bmmMean[-9]
bmmMod <- lm(log(bmmSD)~log(bmmMean))
pts <- c(seq(7.4,7.7, by=0.01))
pdf("MaleRand.pdf")
plot(log(bmmSD)~log(bmmMean), pch = 19, cex = 1.5, xlab = "Log of Group Mean", ylab = "Log of Group SD",main="Male")+abline(a=bmmMod$coef[1],b=bmmMod$coef[2])
binnedbmf <- data.frame(bm=gulls$bm[gulls$sex==2], cats = cut(gulls$bm[gulls$sex==2], breaks=quantile(gulls$bm[gulls$sex==2], prob=seq(0.1,1,by=0.1)), include.lowest=T))
bmfSD <-tapply(binnedbmf$bm, binnedbmf$cats,sd)
bmfMean <-tapply(binnedbmf$bm, binnedbmf$cats,mean)
bmfMod <- lm(log(bmfSD[-2])~log(bmfMean[-2]))
pdf("FemaleRand.pdf")
plot(log(bmfSD)~log(bmfMean), pch = 19, cex = 1.5, xlab = "Log of Group Mean", ylab = "Log of Group SD",main="Female")+abline(a=bmfMod$coef[1],b=bmfMod$coef[2])
#table(binnedbm$cats)
#regress group SDs and group means
regrand <- lm(log(bmSD)~log(bmMean))
summary(regrand)
#Regression fit is poor, but coefficients and plot suggest theta > 3, so inverse Gaussian?
plot(log(bmSD)~log(bmMean), pch = 19, cex = 1.5, xlab = "Log of Group Mean", ylab = "Log of Group SD")
abline(a=regrand$coef[1], b=regrand$coef[2])

#Pick the systematic component. Using log(gulls$oxy) as explanatory
plot(gulls$bm~(gulls$oxy), col=as.factor(gulls$sex), pch=19)
#log transformation for oxy looks better
plot(gulls$bm~log(gulls$oxy), col=as.factor(gulls$sex), pch=19)
#plots using canonical link and also log link
#both plots and residual plots look reasonable, use log link (although canonical link works fine)
pdf("CanonScat.pdf")
plot(1/(gulls$bm)^2~log(gulls$oxy), col=as.factor(gulls$sex), pch=19, main="Canonical Link vs. log(OXY)")
pdf("LogScat.pdf")
plot(log(gulls$bm)~log(gulls$oxy), col=as.factor(gulls$sex), pch=19, main="Log Link vs. log(OXY)")
resids2<- lm(data= gulls, formula=1/bm^2~log(oxy)+as.factor(sex))$residuals
pdf("CanonicalSYs.pdf")
plot(resids2~log(gulls$oxy), col=as.factor(gulls$sex), pch=19, main = "Residuals for Canonical Link vs. log(OXY)") + abline(h=0)
residsl <- lm(data= gulls, formula=log(bm)~log(oxy)+as.factor(sex))$residuals
pdf("LogSYS.pdf")
plot(residsl~log(gulls$oxy), col=as.factor(gulls$sex), pch=19, main = "Residuals for Log Link vs. log(OXY)") + abline(h=0)

lm(data=gulls, formula=bm~oxy+dde+hcb+p99+p118+p138+p153+p170+p180+pc1+cond)
library(MASS)
allfit <- lm(data=gulls, formula=bm~log(oxy)+log(dde)+log(p99)+log(p138)+as.factor(sex))
stepped <- step(allfit)
summary(stepped)
#scatterplot matrix with possible covariates shows high multicollinearity, so we stick with sex, hb, and log(oxy)
pdf("pairsmat.pdf")
pairs(data=gulls, bm~log(oxy)+log(p99)+log(p138)+log(p170)+hb)
boxplot(data=gulls, bm~as.factor(sex))

#here is our final model
final.model <- glm(data=gulls,bm~log(oxy)+as.factor(sex), family="inverse.gaussian"(link="log"))
#summary shows that log(oxy) is not significant at 5% or 10% level
summary(final.model)

#######################
######## Consider three distributions: gamma, normal, inverse gaussian, each with log link

gammafit <- glm(data=gulls,bm~log(oxy)+as.factor(sex), family="Gamma"(link="log"))
normalfit <- glm(data=gulls,bm~log(oxy)+as.factor(sex), family="gaussian"(link="log"))
inversegaussianfit <- glm(data=gulls,bm~log(oxy)+as.factor(sex), family="inverse.gaussian"(link="log"))
igfit<-inversegaussianfit
########## Extract the coefficients for each model and put them in a table:
library("xtable")
estse <- rbind(as.vector(t(summary(gammafit)$coef[,c(1,2,3,4)])),as.vector(t(summary(normalfit)$coef[,c(1,2,3,4)])), as.vector(t(summary(inversegaussianfit)$coef[,c(1,2,3,4)])))
xtable(estse, digits = 3)

#########  Construct 95% Wald intervals for regression parameters, and put them in a table
intig <- cbind( summary(inversegaussianfit)$coef[,c(1)] - 1.96*summary(inversegaussianfit)$coef[,c(2)] ,
                summary(inversegaussianfit)$coef[,c(1)]+ 1.96*summary(inversegaussianfit)$coef[,c(2)])
intgamma <- cbind( summary(gammafit)$coef[,c(1)] - 1.96*summary(gammafit)$coef[,c(2)] ,
                summary(gammafit)$coef[,c(1)]+ 1.96*summary(gammafit)$coef[,c(2)])
intnormal <- cbind( summary(normalfit)$coef[,c(1)] - 1.96*summary(normalfit)$coef[,c(2)] ,
                   summary(normalfit)$coef[,c(1)]+ 1.96*summary(normalfit)$coef[,c(2)])

rbind(as.vector(t(intgamma)), as.vector(t(intnormal)), as.vector(t(intig)))

####################################
#########  Calculate the scaled deviance for each model:
##### Scaled deviance for inverse-Gaussian
igdev <- summary(inversegaussianfit)$deviance/summary(inversegaussianfit)$dispersion
igdev
pchisq(igdev, 107)
#####  Unscaled deviance for inverse-Gaussian
summary(inversegaussianfit)$deviance
###Scaled deviance for Gamma*************
rtestforgamma <- (gulls$bm - gammafit$fitted)/sqrt(gammafit$fitted^2)
2/(sum(rtestforgamma^2)/107)*sum( (gulls$bm - gammafit$fitted)/gammafit$fitted - log(gulls$bm/gammafit$fitted))
###Unscaled deviance for Gamma
summary(gammafit)$deviance

###Scaled deviance for Normal*************
rtestfornormal <- (gulls$bm - normalfit$fitted)
2/(sum(rtestfornormal^2)/107)*sum( (gulls$bm - normalfit$fitted)/normalfit$fitted - log(gulls$bm/normalfit$fitted))
##### Unscaled deviance for normal
summary(normalfit)$deviance


###############  Combine unscaled ans scaled deviances in a table:
col1dev <- c(summary(gammafit)$deviance, summary(gammafit)$deviance/summary(gammafit)$dispersion)
col2dev <- c(summary(normalfit)$deviance, summary(normalfit)$deviance/summary(normalfit)$dispersion)
col3dev <- c(summary(igfit)$deviance, summary(igfit)$deviance/summary(igfit)$dispersion)

cbind(col1dev, col2dev, col3dev)
#########################################################
##########################################
#### H matrix for gamma:
X <- cbind(1, log(gulls$oxy),as.factor(gulls$sex))
Xm <- cbind(1, log(gullsM$oxy))
Xf <- cbind(1, log(gullsF$oxy))
pdf("Mresids.pdf")
Zigm <- diag(sqrt(igfitM$weights))%*%Xm
HiiIGm<-diag(Zigm%*%solve(t(Zigm)%*%Zigm)%*%t(Zigm))
dsdigm<- residuals(igfitM, type = "deviance")/sqrt(summary(igfitM)$dispersion)/sqrt(1 - HiiIGm)
plot(igfitM$fitted, dsdigm, xlab = "Fitted Value (g)", ylab = "Standardized Deviance Residual",
     pch=19, main="Deviance Residuals for Males")+abline(h = 0)
dev.off()
pdf("Fresids.pdf")
Zigf <- diag(sqrt(igfitF$weights))%*%Xf
HiiIGf<-diag(Zigf%*%solve(t(Zigf)%*%Zigf)%*%t(Zigf))
dsdigf<- residuals(igfitF, type = "deviance")/sqrt(summary(igfitF)$dispersion)/sqrt(1 - HiiIGf)
plot(igfitF$fitted, dsdigf, xlab = "Fitted Value (g)", ylab = "Standardized Deviance Residual",
     pch=19, main = "Deviance Residuals for Females")+abline(h = 0)
dev.off()
#####################################################
##### Confidence intervals for the mean
gullsM <- gulls[which(gulls$sex==1),]
gullsF<- gulls[which(gulls$sex==2),]

igfitM <- glm(data=gullsM, formula=bm~log(oxy), family="inverse.gaussian"(link="log"))
estseigM <- predict(igfitM, se.fit = TRUE, type = "response")
lowerMuigM <- estseigM$fit - 1.96*estseigM$se
upperMuigM <- estseigM$fit + 1.96*estseigM$se

igfitF <- glm(data=gullsF, formula=bm~log(oxy), family="inverse.gaussian"(link="log"))
estseigF <- predict(igfitF, se.fit = TRUE, type = "response")
lowerMuigF <- estseigF$fit - 1.96*estseigF$se
upperMuigF <- estseigF$fit + 1.96*estseigF$se
#####  Plot the confidence intervals for the mean
pdf("95Ints.pdf")
plot(gulls$bm~log(gulls$oxy), xlab = "log(OXY) concentration", ylab = "Body Mass (g)", pch = 19, cex = 1.5, col=as.factor(gulls$sex), main = "Plot of Estimated Means and 95% Bands")
lines(log(gullsM$oxy[order(gullsM$oxy)]), estseigM$fit[order(gullsM$oxy)]) 
lines(log(gullsM$oxy[order(gullsM$oxy)]), lowerMuigM[order(gullsM$oxy)], lty = 2) 
lines(log(gullsM$oxy[order(gullsM$oxy)]), upperMuigM[order(gullsM$oxy)], lty = 2)
lines(log(gullsF$oxy[order(gullsF$oxy)]), estseigF$fit[order(gullsF$oxy)]) 
lines(log(gullsF$oxy[order(gullsF$oxy)]), lowerMuigF[order(gullsF$oxy)], lty = 2) 
lines(log(gullsF$oxy[order(gullsF$oxy)]), upperMuigF[order(gullsF$oxy)], lty = 2)
dev.off()

pdf("resids.pdf")
devs <- residuals(igfit, type = "deviance")/sqrt(summary(igfit)$dispersion)
plot(resid(igfit)~log(gulls$oxy), xlab = "log(OXY) concentration", ylab = "Body Mass (g)", pch = 19, cex = 1.5,
     col=as.factor(gulls$sex), main = "Residuals from Inv. Gaussian Model") + abline(h=0, lty=2)
dev.off()
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################
###############################################################

######## Part 2#####################
xmat <- cbind(1, (1:50)/50)
b1 <- xmat[,2]

simbasicglm <- function(b, Xmat, phi, linkInv, random, ns = 1,returnmu=F){
  eta <- Xmat%*%b; #return(eta)
  mu <- linkInv(eta) ; if(returnmu==T){return(mu)}
  random(mu, phi, ns)
}
clogloginv <- function(x){
  1-exp(-1*exp(x))
}
BinomRan <- function( mu, phi, ns){
  rbinom(length(mu), prob= mu, size = ns)
}


simvals<-simbasicglm(b=c(-0.5,2), Xmat=xmat, phi=1, linkInv=clogloginv, random=BinomRan,ns=1)
#pdf("2scatterplot.pdf")
plot(simvals~b1, xlab="X", main="Scatterplot of Simulated Values vs. X (covariate) values", pch=20)
bernmodel<-glm(simvals~xmat[,2], family="binomial"(link="cloglog"))
summary(bernmodel)
deviance(bernmodel)
#########  Construct 95% Wald intervals for regression parameters, and put them in a table
intervalbern <- cbind( summary(bernmodel)$coef[,c(1)] - 1.96*summary(bernmodel)$coef[,c(2)] ,
                summary(bernmodel)$coef[,c(1)]+ 1.96*summary(bernmodel)$coef[,c(2)])
intervalbern

####################################
#########  Calculate the scaled deviance for each model:
##### Scaled deviance for binomial
summary(bernmodel)$deviance/summary(bernmodel)$dispersion
#####  Unscaled deviance for binomial
summary(bernmodel)$deviance
#log-likelihood
logLik(bernmodel)
summary(bernmodel)
#true response curve
pdf("2TrueEst.pdf")
pts <- cbind(1,c(seq(1,50, by=0.1))/50)
trc <- clogloginv(-0.5+pts[,2]*(2))
lines(trc~pts[,2], xlab="x1", ylab="Value", lwd=2)
#estimated response curve
#erc <- simbasicglm(b=coef(bernmodel),Xmat=pts,phi=1,linkInv=clogloginv,random=BinomRan,ns=1,returnmu=T)
erc <- clogloginv(bernmodel$coef[1]+pts[,2]*bernmodel$coef[2])
lines(erc~pts[,2], col="red", lwd=2)
legend(x="right",col=c("black","red"), pch=20,legend=c("True", "Estimated"))

#Expectation function and confidence bands
estseB <- predict(bernmodel, se.fit = TRUE, type = "response")
lowerMuB <- estseB$fit - 1.96*estseB$se
upperMuB <- estseB$fit + 1.96*estseB$se
pdf("2ExpF.pdf")
plot(simvals~b1, xlab= "X", main="Simulated values vs. X, with Estimated Expectation Function", pch=20)
lines(lowerMuB~xmat[,2],lty=2,lwd=2) + lines(upperMuB~xmat[,2],lty=2,lwd=2) + lines(estseB$fit~xmat[,2], lwd=2)
lines(trc~pts[,2], xlab="x1", ylab="Value", lwd=2, col="red")
legend(x="right",col=c("black","red"), pch=20,legend=c("Estimated","True"))
dev.off()
