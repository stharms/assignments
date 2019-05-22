library(tidyverse)
library(reshape2)
library(energy)
library(ellipse)
##2
#read in data
effluent <- read.table(file = "effluent.dat", header=F, col.names=c("sample", "bod1", "ss1", "bod2", "ss2"))

#conduct energy test with 10000 bootstrap replicates
set.seed(112358)
mvnorm.etest(effluent[,-1], R=10000)
#p-value is large, fail to reject H0 of multivariate normality

#Hypothesis test
#try deleting the outlier first
effluent_r <- effluent[-8,]
mvnorm.etest(effluent_r[,-1], R=10000)

xbar<-apply(effluent_r[,-1],2,mean)
xbar
xvar<-var(effluent_r[ ,-1])
xvar

#  Apply the contrasts

Cstar <-matrix( c(1, 0, -1, 0, 0, 1, 0, -1), 2, 4, byrow=T)
Cstar
Cxbar <- Cstar %*% xbar
Cxvar <- Cstar %*% xvar %*%t(Cstar)

# Compute Hotelling statistic

p <- nrow(Cxvar)
n <- nrow(effluent_r )
nullmean <- rep(0, p)
d <- Cxbar-nullmean
t2<-n*t(d)%*%solve(Cxvar)%*%d;
t2mod<- (n-p)*t2/(p*(n-1))
pval <- 1- pf(t2mod,p,n-p)

cat("Hotelling T-squared statistic", fill=T)
t2

cat("p-value", fill=T)
pval

# alternative way done with simple use of Hotelling's T^2-test in the ICSNP
# package

library(ICSNP)
HotellingsT2( as.matrix(effluent_r[, 2:5]) %*% t(Cstar), mu = c(0,0))

######################################
###############################
#read in data
olive <- read.table("olive.dat", header=T)
#focus on region 2
olive <- olive %>% filter(group.id %in% c(5,6))
#####
##a
s.5 <- olive %>% filter(group.id ==5) %>% select(-group.id) %>% var
s.6 <- olive %>% filter(group.id ==6) %>% select(-group.id) %>% var
s.5
s.6
mx <- max(s.5,s.6); mn <- min(s.5,s.6); mid <- median(c(s.5,s.6))
s.5c <- olive %>% filter(group.id ==5) %>% select(-group.id) %>% cor
s.6c <- olive %>% filter(group.id ==6) %>% select(-group.id) %>% cor
round(s.5c,2)
round(s.6c,2)
mxc <- max(s.5c,s.6c); mnc <- min(s.5c,s.6c); midc <- median(c(s.5c,s.6c))

s.5.m <- s.5 %>% melt %>% mutate(id = 5)
s.6.m <- s.6 %>% melt %>% mutate(id = 6)
#levels(s.5.m$Var2)<- levels(s.6.m$Var2)<- rev(levels(s.5.m$Var1))
covdf <- rbind(s.5.m, s.6.m)
s.5.cm <- s.5c %>% melt %>% mutate(id = 5)
s.6.cm <- s.6c %>% melt %>% mutate(id = 6)
#levels(s.5.cm$Var2)<- levels(s.6.cm$Var2)<- rev(levels(s.5.cm$Var1))
covdfc <- rbind(s.5.cm, s.6.cm)
#covariance matrix visualized
ggplot(data = s.5.m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(limits=c(mn, mx), midpoint=mid)
ggplot(data = s.6.m, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(limits=c(mn, mx), midpoint=mid)
ggplot(data=covdf, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + scale_fill_gradient2(limits=c(mn, mx), midpoint=mid) + 
  facet_wrap(~as.factor(id)) + ggtitle("Covariance Matrices by Region") +
  scale_y_discrete(limits = rev(levels(covdf$Var2)))
heatmap(s.5, Colv = NA, Rowv = NA)
heatmap(s.6, Colv = NA, Rowv = NA)
#correlation matrices instead
ggplot(data = s.5.cm %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(limits=c(mnc, mxc))
ggplot(data = s.6.cm %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + scale_fill_gradient2(limits=c(mnc, mxc), midpoint=midc)
ggplot(data=covdfc, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile() + facet_wrap(~as.factor(id)) +
  scale_fill_gradient2(limits=c(mnc, mxc), midpoint=midc) + ggtitle("Correlation Matrices by Region") +
  scale_y_discrete(limits = rev(levels(covdfc$Var2)))

######
##b
#split into subregions
olive.x <- olive %>% filter(group.id ==5) %>% select(-group.id)
olive.y <- olive %>% filter(group.id ==6) %>% select(-group.id)
#test for normality first
set.seed(112358)
mvnorm.etest(olive.x, R=10000)
mvnorm.etest(olive.y, R=10000)
#test for equality of means
HotellingsT2(X=olive.x, Y=olive.y)

######
##c
#variable 5
xbarf<- mean(olive.x[,5])-mean(olive.y[,5])
spoolf <- ((length(olive.x[,5])-1)*var(olive.x[,5]) + (length(olive.y[,5])-1)*var(olive.y[,5]))/
  (length(olive.x[,5])+length(olive.y[,5]) - 2)
sef <- sqrt(spoolf)*sqrt(1/length(olive.x[,5]) + 1/length(olive.y[,5]))
dff <- length(olive.x[,5]) + length(olive.y[,5]) - 2

lbf <- xbarf - qt(1-.05/(2*2), dff) * sef
ubf <- xbarf + qt(1-.05/(2*2), dff) * sef
lbf<- round(lbf,3); ubf <- round(ubf,3)
#CI
paste("(", lbf, " , ", ubf, ")", sep="")
t.test(x=olive.x[,5],y=olive.y[,5], alternative='two.sided', mu=0, conf.level = (1-.05/2), var.equal=T)

#variable 6
xbars<- mean(olive.x[,6])-mean(olive.y[,6])
spools <- ((length(olive.x[,6])-1)*var(olive.x[,6]) + (length(olive.y[,6])-1)*var(olive.y[,6]))/
  (length(olive.x[,6])+length(olive.y[,6]) - 2)
ses <- sqrt(spools)*sqrt(1/length(olive.x[,6]) + 1/length(olive.y[,6]))
dfs <- length(olive.x[,6]) + length(olive.y[,6]) - 2

lbs <- xbars - qt(1-.05/(2*2), dfs) * ses
ubs <- xbars + qt(1-.05/(2*2), dfs) * ses
lbs<- round(lbs,3); ubs <- round(ubs,3)
#CI
paste("(", lbs, " , ", ubs, ")", sep="")
t.test(x=olive.x[,6],y=olive.y[,6], alternative='two.sided', mu=0, conf.level = (1-.05/2), var.equal=T)

#Get estimated covariance matrices
Sx <- var(olive.x[,5:6])
Sy <- var(olive.y[,5:6])
#are the covariance matrices equal?
Sx; Sy
#close enough
#get pooled covariance matrix
lx <- nrow(olive.x); ly<-nrow(olive.y)
Spooled <- (lx-1)/(lx+ly-2)*Sx + (ly-1)/(lx+ly-2)*Sy
#get covariance matrix for differences
Sdiff <- (1/lx + 1/ly)*Spooled
Sdiff

#plot the ellipse and the bonferroni confidence regions
plot(ellipse(Sdiff, centre=c(xbarf,xbars)), type="l", lwd=2, ylim=c(0,8), xlim=c(-250,-190), 
     main="Confidence Regions for Difference in Means for Chemicals 5 and 6") + 
  rect(lbf,lbs,ubf,ubs, lty=3, lwd=2) + points(xbarf,xbars, cex=2, pch=19)
  legend(-250, 7.8, legend=c("Simultaneous", "Individual"), lty=c(1,2), cex=0.8)
