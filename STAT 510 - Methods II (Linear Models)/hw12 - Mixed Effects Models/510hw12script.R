library(MASS)
library(nlme)
library(lme4)
#########
#1)
##a) and ##b)
leaf<- read.table(file="LeafArea.txt",header=T)
leaf$ResearchStation <- factor(leaf$ResearchStation)
mod1 <- lmer(LeafArea ~ Dose + (1 + Dose | ResearchStation), data = leaf)
mod1
sige <- sigma(mod1)^2
Sigmab <- matrix(c(3.238626^2, .06*3.238626*.007499, .06*3.238626*.007499, .007499^2), nrow = 2, ncol = 2)
sige
Sigmab
##c)
#get our regression coefficients using the model
b <- fixef(mod1)[1]
a <- fixef(mod1)[2]
c(a,b)
#plot data and the line together
plot(y=leaf$LeafArea[which(leaf$ResearchStation==7)], x = leaf$Dose[which(leaf$ResearchStation==7)]) +
  abline(coef = c(b,a))

##d)
B1b17 <- fixef(mod1)[1] + ranef(mod1)$ResearchStation[7,1]
B2b27 <- fixef(mod1)[2] + ranef(mod1)$ResearchStation[7,2]
c(B1b17,B2b27)

##e)
station7 <- leaf[leaf$ResearchStation==7,]
mod7 <- lm(LeafArea ~ Dose, data=station7)
mod7

##f)
plot(y=leaf$LeafArea[which(leaf$ResearchStation==7)], x = leaf$Dose[which(leaf$ResearchStation==7)], pch = 20,
     ylab = "Leaf Area" , xlab = "Dose") +
  abline(coef = c(b,a), lwd = 2)  +
  abline(reg= mod7, col ="Blue", lwd = 2) +
  abline(coef = c(B1b17,B2b27), col = "Red", lwd = 2)

##g)
#fit a reduced model where B2 = 0
mod0 <- lmer(LeafArea ~ 1 + (1 + Dose | ResearchStation), data = leaf)
#Calculate the Likelihood ratio statistic for reduced vs. full model
lrs <- -2*(logLik(mod0) - logLik(mod1))
lrs

##h)
AIC(mod1)
-2*logLik(mod1) + 2*(6)

##i)
#fit model with only a random effect on the intercept
modi <- lmer(LeafArea ~ Dose + (1 | ResearchStation), data = leaf)
AIC(modi)
-2*logLik(modi) + 2*4

##j)
#without random effects for intercept or slope, it's just a simple linear regression
modj <- lm(LeafArea ~ Dose, data = leaf)
AIC(modj)
-2*logLik(modj) + 2*3

##k)
anova(modi,modj,mod1)
#model from i is preferred with the smallest AIC

##################
#3)
donner <- read.table(file="Donner.txt", header=T)
View(donner)
mod3 <- glm(status~sex + age, family = binomial(link=logit), data= donner)
summary(mod3)
confint(mod3)
mod3a <- glm(status~sex, family = binomial(link=logit), data= donner)
mod3b <- glm(status~age, family = binomial(link=logit), data= donner)
mod3c <- glm(status~age + sex + age*sex, family = binomial(link=logit), data= donner)
anova(mod3, mod3a, test = "Chisq")
anova(mod3, mod3b, test = "Chisq")

exp(coef(mod3))
exp(confint(mod3))

x <- min(donner$age):max(donner$age)
plot(x,1/(1+exp(-(coef(mod3)[1]+coef(mod3)[3]*x))),ylim=c(0,1),
     type="l",col=4,lwd=2,xlab="Age",
     ylab="Estimated Probability of Survival", cex.lab=1.3)
lines(x,1/(1+exp(-(coef(mod3)[1]+coef(mod3)[3]*x + coef(mod3)[2]))),col=2,lwd=2)
legend("topleft", legend=c("Females","Males"),
       col=c(4,2),lwd=2)
