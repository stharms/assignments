library(tidyverse)
library(reshape2)
library(car)
library(classdata)
##2
###a)
olives <- read.delim('olive.dat', sep="")
#melt and summarize the data
summed <- olives %>% mutate(region = as.factor(ifelse(group.id < 5, 1, ifelse(group.id > 6, 3 , 2)))) %>%
  melt(id=c('group.id','region')) %>% group_by(region,group.id,variable) %>% summarise( grmean = mean(value)) %>%
  mutate(variable=as.numeric(str_sub(variable,-1L)))
#plot
#all
summed %>% ggplot(aes(x= as.factor(variable), y=grmean)) +
  geom_line(aes(group=as.factor(group.id), linetype=as.factor(group.id), color=region), size=1.3) +
  labs(x="Chemical", y="Group Mean")
#facet by region
summed %>% ggplot(aes(x= as.factor(variable), y=grmean)) +
  geom_line(aes(group=as.factor(group.id), color=as.factor(group.id)), size=1.3) +
  labs(x="Chemical", y="Group Mean") + facet_wrap('region')
#r1
summed %>% filter(region==1) %>%  ggplot(aes(x= as.factor(variable), y=grmean)) +
  geom_line(aes(group=as.factor(group.id), linetype=as.factor(group.id)), size=1.3) +
  labs(x="Chemical", y="Group Mean")
#r2
summed %>% filter(region==2) %>%  ggplot(aes(x= as.factor(variable), y=grmean)) +
  geom_line(aes(group=as.factor(group.id), linetype=as.factor(group.id)), size=1.3) +
  labs(x="Chemical", y="Group Mean")
#r3
summed %>% filter(region==3) %>%  ggplot(aes(x= as.factor(variable), y=grmean)) +
  geom_line(aes(group=as.factor(group.id), linetype=as.factor(group.id)), size=1.3) +
  labs(x="Chemical", y="Group Mean")
#Clearly chemicals 4 and 5 appear to be vary the most across group/region, agreeing with previous homework result
#It is difficult to see much interaction, although it does appear that the lines cross from chemical 5 to 6 so it is possible

###b)
#need to specify regions
olives <- olives %>% mutate(region = as.factor(ifelse(group.id < 5, 1, ifelse(group.id > 6, 3 , 2))), group.id=as.factor(group.id))
#fit the model
fitlm <- lm(as.matrix(olives[,2:9])~olives$region)
manolive <- Manova(fitlm)
summary(manolive)
#all of the p-values for the overall F-test are ~0, which indicates we reject the null hypothesis of equal group means for all regions

###c)
#specify M matrix
M = rbind(c(1,-1,rep(0,times=6)),
          c(0,1,-1,rep(0,times=5)),
          c(0,0,1,-1, rep(0,times=4)),
          c(rep(0,times=3),1,-1,rep(0,times=3)),
          c(rep(0,times=4),1,-1, rep(0,times=2)),
          c(rep(0,times=5),1,-1,0),
          c(rep(0,times=6),1,-1))
t(M)
fitint <- Manova(lm((as.matrix(olives[,2:9]) %*% t(M)) ~ olives$region))
fitint
summary(fitint)
#fail to reject the null hypothesis of no interaction effects. This agrees with the plots from part (a)
#C matrix
C <- rbind(c(0,1,0),
           c(0,1,-1))
fitint.c <- linearHypothesis(model = fitlm, hypothesis.matrix = C)
fitint.c
fitint.cm <- linearHypothesis(model = fitlm, hypothesis.matrix = C, P=t(M))
fitint.cm
###d)
#get r1 only
olivesr1 <- olives %>% filter(region==1)
#fit new model
fitlmr <- lm(as.matrix(olivesr1[,2:9])~olivesr1$group.id, contrasts = contr.SAS(4))
manova.grp <- Manova(fitlmr)
summary(manova.grp)
C <- cbind(c(0,0,0),diag(3))
C <- rbind(c(0,1,0,0), c(0,1,-1,0), c(0,1,0,-1))
fitint.cg <- linearHypothesis(model = fitlmr, hypothesis.matrix = C)
fitint.cg
#fitr1 <- manova(as.matrix(olivesr1[,2:9]) %*% t(M) ~ olivesr1$group.id)
#fitr1
summary(fitr1,"Wilks")
################################
################################

##3)
###a)
#filter the data
fbif <- fbiwide %>% filter(State%in%c("California","Iowa","Illinois", "District of Columbia", "New York"))
#log and lag
fbif <- fbif %>% mutate_at(vars(4:12), funs(log(.))) %>% data.frame() %>% mutate_at(vars(4:12), funs(.-lag(.)))

###b)
#add decade variable
fbid <- fbif %>% mutate(decade = as.factor(ifelse(between(Year,1961,1980), 1 ,
                                              ifelse(between(Year, 1981,2000), 2,
                                                 3)))) %>% select(-Rape) %>% filter(complete.cases(.))
#fit two-way Manova
fittwo <- lm(as.matrix(fbid[,(5:11)]) ~ fbid$State * fbid$decade, data = fbid)
fit.manova.two <- Manova(fittwo)
summary(fit.manova.two)
#Fail to reject null hypothesis for difference between states (p=.2697)
#Reject null hypothesis for difference between decades (p~0)
#Fail to reject null hypothesis for interaction between states and decades (Wilks: p=.7616)


###c)
#SS matrices. The factors and interactions, then the error SS
SSm <- fit.manova.two$SSP %>% lapply(as.matrix)
SSm
fit.manova.two$SSPE
(fit.manova.two$SSPE + SSm[[1]] + SSm[[2]] + SSm[[3]])/269
t(scale(as.matrix(fbid[,(5:11)]),scale=F)) %*% scale(as.matrix(fbid[,(5:11)]),scale=F)/269

fmt <- manova(as.matrix(fbid[,(5:11)]) ~ fbid$State * fbid$decade, data = fbid)
cov(fitted.values(fmt))+cov(fmt$residuals)
cov(as.matrix(fbid[,(5:11)]))

###d)
M <- rbind(c(1,-1,rep(0,times=5)),
           c(0,1,-1,rep(0,times=4)))
fitcon <- lm(as.matrix(fbid[,(5:11)])%*%t(M) ~ fbid$State * fbid$decade)
t(M)
fit.manova.con <- Manova(fitcon)
summary(fit.manova.con)
