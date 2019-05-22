library(classdata)
library(tidyverse)
library(car)
library(matrixcalc)
library(ggplot2)

#transform the data to log population
fbi <- fbiwide %>% select(-Rape) %>% 
  mutate_at(vars(5:11), funs(./Population)) %>% mutate_at(5:11, funs(log(.)))
#get covariates
fbi <- fbi %>% mutate(Y.1 = Year-1961, Y.2=(Y.1)^2, Y.3=(Y.1)^3)
#complete cases only (drop NAs)
fbi <- fbi %>% filter(complete.cases(.))

head(fbi)

#######
##a)
#regression model and MANOVA
fit.lm <- lm(cbind(fbi %>% select(5:11) %>% as.matrix()) ~ Y.1 + Y.2 + Y.3 + State , data = fbi)
fit.manova <- Manova(fit.lm)
#this output is long!
fit.lm %>% summary()
#MANOVA output
fit.manova %>% summary()


#######
##b)
#Pillai : tr[(Sig_1 - Sig)Sig^-1) <- This looks wrong? I use a different formula to get correct matching answer
matrix.trace(
  solve((fit.manova$SSP$Y.3+fit.manova$SSPE)) %*%
    (fit.manova$SSP$Y.3)
  )

#######
##c)
#regression curve fitted values are in fit.lm$fitted.values[,2]
ggplot(fbi) + geom_line(aes(x=Year, y=fit.lm$fitted.values[,2], col=as.factor(State))) +
  ggtitle("Burglary Rate (fitted) by Year and State")+ labs(y="Log Burglary Rate / population")

#######
##d)
#prediction vector
#Iowa is the 19th row of coefficients
which(row.names(fit.lm$coefficients)=="StateIowa")
Y.p <- 2019-1961
predv <- c(1,Y.p,Y.p^2,Y.p^3, rep(0,times=14),1,rep(0, times=51-15)) 
#prediction. Population was not included in our regression model?
pred <- predv %*% fit.lm$coefficients
pred
#if we want to get total number of predicted crimes (with population), convert back to regular scale
pred %>% exp()*3160000
#95% prediction intervals
p <- ncol(fit.lm$fitted.values)
r <- fit.lm$rank - 1
n <- nrow(fit.lm$fitted.values)
fscale <- (p*(n-r-1))/(n-r-p)
fvalue <- qf(.05, p, n-r-p, lower.tail=F)

betavcov <- (model.matrix(fit.lm) %>% t()) %*% model.matrix(fit.lm)
prederrmat <- t(predv) %*% solve(betavcov) %*% predv
prederr <- 1+prederrmat
varerr <- (n/(n-r-1))*diag(cov(fit.lm$residuals))
merror <- sqrt(fscale*fvalue)*sqrt(prederr%*%varerr)

lbs <- pred - merror
ubs <- pred + merror
pred.intervals <- rbind(ubs,lbs) %>% t()
pred.intervals
#on same scale
pred.intervals %>% exp()*3160000

#######
##e)
#new model
fit2.lm <- update(fit.lm, .~ . -Y.1 -Y.2 -Y.3)
#test with ANOVA
anova(fit.lm,fit2.lm,test="Wilks")
#large test statistic, small p-value
#Test with linearhypothesis
#all interaction dummy variables are 0
c<- cbind(rep(0, times=3), diag(3), matrix(0, nrow=3,ncol=51))
newfit <- linearHypothesis(model = fit.lm, hypothesis.matrix = c)
newfit
#Wilks Statistic is the same
###########
##f)
#center the response variables
fbi.av <- fbi[,c(1,5:11)] %>% group_by(State) %>% summarise_all(mean) 
head(fbi.av)
fbij <- left_join(fbi, fbi.av, by=c('State'='State'))
fbi.centered <- cbind(fbi[,1:4],fbi[,5:11]-fbij[,15:21],fbi[,12:14]) %>% data.frame()
head(fbi.centered)

#PC analysis
fbi.centered.pc <- prcomp(fbi.centered[,5:11])
fbi.centered.pc
#the first principal component is just a weighted average of the variables (overall crime rate)
#the second is a contrast between centered assault/rape and the rest of the variables 
#(pretty much contrast between theft cases and violent cases, except for murder)
#the third pc is another contrast between violent crimes (assault/murder) and non-violent/theft crimes, except for rape
csums <- cumsum(fbi.centered.pc$sdev^2) / sum(fbi.centered.pc$sdev^2)
csums
plot(y=csums,x=seq(1:7), type='l')
# I will use 3 pcs, although we could justify any between 2 and 4

############
##g)
#looking only at the first two PCs
#Transform data by PCs
fbi.pc1 <- (as.matrix(fbi.centered.pc$x) + as.matrix(fbij[,15:21])) %*%fbi.centered.pc$rotation[,1]
fbi.pc2 <- (as.matrix(fbi.centered.pc$x) + as.matrix(fbij[,15:21])) %*%fbi.centered.pc$rotation[,2]
fbi.tr <- data.frame(Abb=fbi$Abb, pc1=fbi.pc1, pc2=fbi.pc2)
fbi.tr <- fbi.tr %>% filter(Abb%in% c("CA", "IA","IL","D.C.","NY"))
ggplot(fbi.tr) + geom_point(aes(x=pc1,y=pc2, col=Abb))
# Iowa has much lower overall crime rate (pc1), but much higher rate of violent crime vs. nonviolent/theft crimes
# D.C. has much higher overall crime rate, but somewhat lower rate of Assault/rape vs. theft crimes
# The rest of the states are similar, although it is clear that they are different over time


############################
#################################
###################################
##2)
###a)
#pixels
ziptrain <- read_delim("ziptrain.dat", delim=" ", col_names=F) %>% as.data.frame()
#digits
zipdigit <- read_delim('zipdigit.dat', delim=" ", col_names=F) %>% as.matrix
#model
fitpix <- lm(as.matrix(zipdigit)~as.matrix(ziptrain))
anovas <- summary(fitpix)
top100 <- anovas$coefficients[,4] %>% sort() 
top100 <- names(top100[1:100]) %>% str_sub(start=21) %>% as.numeric()
#another way?
i=0
pvals <- matrix(nrow=256,ncol=2)
repeat{
  i=i+1
  pix <- lm(c(ziptrain[,i])~as.factor(zipdigit[,1]))
  pvals[i,] <- c(i,anova(pix)$`Pr(>F)`[1])
  if(i==256){break}
}

top100p <- pvals %>% data.frame %>% arrange(X2)
top100p <- top100p[1:100,1]

###b)
#center the data first
#use only the best 100
ziptrain.t <- ziptrain[,top100p]
ziptrain.m <- ziptrain.t %>% apply(2,mean)
ziptrain.c <- sweep(ziptrain.t, MARGIN=2,ziptrain.m)
#PCs
zip.pc <- prcomp(ziptrain.c)
zip.pc %>% summary()
cumsum(zip.pc$sdev^2) / sum(zip.pc$sdev^2)
#around 16-20

# Test for the PC proportions
#want the largest # of PCs where we fail to reject at 5% significance
#16 fails to reject, but 15 rejects
#We want the first 16 PCs
PCs.proportion.variation.enuff(zip.pc$sdev^2, 15, 0.8, nobs=nrow(ziptrain))
PCs.proportion.variation.enuff(zip.pc$sdev^2, 16, 0.8, nobs=nrow(ziptrain))

###c)
#transform by principal components
ziptransf <- data.frame(zipdig=zipdigit[,1], pc1=as.matrix(ziptrain.t) %*% zip.pc$rotation[,1], pc2=as.matrix(ziptrain.t) %*% zip.pc$rotation[,2])
#plot using ggplot
#with characters, does not look very good
ggplot(ziptransf)+ geom_text(aes(y=pc2,x=pc1, label=as.factor(zipdig)))
#try with colors
ggplot(ziptransf) + geom_point(aes(y=pc2,x=pc1, col=as.factor(zipdig)))
