library(MASS)
library(nlme)
#1
proj <- function(x){
  px <- x%*%ginv(t(x)%*%x)%*%t(x)
  return(px)
}
w1 <- c(rep(1,times=20))
pw1 <- proj(w1)
x2 <- c(rep(1,times=10),rep(0,times=10))
w2 <- (diag(20)-pw1)%*%x2
pw2 <- proj(cbind(w1,w2))
x3 <- c(1,1,rep(0,times=8),rep(1, times=6),rep(0,times=4))
w3<- (diag(20)-pw2)%*%x3
pw3 <- proj(cbind(w1,w2,w3))

w <- cbind(w1,w2,w3)
w
round(t(w)%*%w)
wtwin <- solve(round(t(w)%*%w))
wtwin

######
#2
heartrate <- read.table(file="HeartRate.txt",header=T)
View(heartrate)
examscores <- read.table(file="ExamScores.txt",header=T)
heartrate$woman <- factor(heartrate$woman); heartrate$drug <- factor(heartrate$drug);heartrate$time <- factor(heartrate$time)
glmc <- lme(y~drug*time, data=heartrate, random = ~1|woman)
bvec <- glmc$coefficients$fixed
##b
#The final line in the anova table should give us the correct test for interactions
anova(glmc)[4,]
##c
C1 <- c(0,1,rep(0,times=8),1,0)
C2 <- c(0,0,1,rep(0,times=7),0,1)
C<-rbind(C1,C2)
est<-C%*%bvec
est
se <- sqrt(C%*%glmc$varFix%*%t(C))
se
est[1]/se[1,1]
est[2]/se[2,2]
pt(est[1]/se[1,1], df=48, lower.tail=F)
pt(est[2]/se[2,2], df=48, lower.tail=F)
##d
estd <- C1%*%bvec
sed <- sqrt(t(C1)%*%vcov(glmc)%*%C1)
upd <- estd + qt(.975,48)*sed
lowd <- estd - qt(.975,48)*sed
c(lowd, upd)
#############
#3
##a
alm <- gls(y~drug*time, data=heartrate, correlation=corCompSymm(form=~1|woman))
alm
##b
baic <- -2*logLik(alm) + 2*(12+2)
bbic <- -2*logLik(alm) + (12+2)*log(60-12)
##c
clm <- gls(y~drug*time, data=heartrate, correlation=corAR1(form=~1|woman))
clm
##d
daic <- -2*logLik(clm) + 2*(12+2)
dbic <- -2*logLik(clm) + (12+2)*log(60-12)
##e
elm <- gls(y~drug*time, data=heartrate, correlation=corSymm(form=~1|woman))
##f
faic <- -2*logLik(elm) + 2*(12+7)
fbic <- -2*logLik(elm) + (12+7)*log(60-12)
##g
anova(alm,clm,elm)
##h
esth <- C1%*%coef(clm)
seh <- sqrt(t(C1)%*%vcov(clm)%*%C1)
uph <- esth + qt(.975,48)*seh
lowh <- esth - qt(.975,48)*seh
c(lowh, uph)
###########
#4
d <- read.table(file="ExamScores.txt",header=T)
d$exam <- factor(d$exam)
d$student <- factor(d$student)
scoremod <- lme(score ~ 0 + exam, random = ~ 1 | student, weights = varIdent(form = ~ 1 | exam), data = d)
scoremod$varFix  
scoremod
sigs <- 13.43525^2
sig1 <- 7.933829^2 ; sig2 <- .9787570*sig1 ; sig3 <- .5222791*sig1
sigs
sig1
sig2
sig3
eblup <- fixed.effects(scoremod)[3] + random.effects(scoremod)[1,1]
eblup
#######
var(d$score[d$exam==3])-sigs
var(d$score[d$exam==2])-sigs
var(d$score[d$exam==1])-sigs
