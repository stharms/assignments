library(nlme)
library(tidyverse)
rm(list=ls())
Yield <- read.table(file = "hw10GenotypeYield.txt", header=T)
head(yield)
str(yield)
Yield$genotype <- factor(Yield$genotype)

####
#a)
mod1 <- lm(data = Yield, yield~genotype)
blues <- mod1$coefficients + mod1$coefficients[1]
blues[1] <- blues[1]/2
blues
#b)
mod <- lme(data = Yield, yield~1, random = ~1|genotype)
mod
sigmag <- 2.686537
sigmae <- 9.669021
#c)
blups <- mod$coefficients$fixed +
  mod$coefficients$random$genotype + mod$coefficients$random$genotype[1]
blups[1] <- mod$coefficients$fixed[1] + mod$coefficients$random$genotype[1]
head(blups)
#d)
plot(x=blues, y = blups) + abline(a = 0, b = 1, col = "blue", lwd = 1)
#e)
ests <- data.frame(unique(Yield$genotype),as.numeric(blues), blups)
names(ests) <- c("genotype", "blues", "blups")
ests %>% arrange(desc(blues)) %>% head()
#f)
ests%>% arrange(desc(blups)) %>% head()


#########
#3)
geno <- factor(c(rep(1, times = 7),rep(2,times = 7)))

y <- c(14,9,10,5,18,9,9,17,10,17,18,13,17,16)
testmod <- lm(y~geno)
testmod1 <- glm(y~geno, family = poisson)
logy <- log(y)
sum(logy[1:7])
sum(logy[8:14])
log(74/7)
log(108/7)
aica <- -2*(-7*(74/7) + 74*log(74/7) - sum(log(factorial(y))[1:7]) - 7*(108/7) + 108*log(108/7) - sum(log(factorial(y))[8:14])) + 4
aica
bica <- aica - 4 + 2*log(14)
bica
aicb <- -2*(-14*(13) + 182*log(13) - sum(log(factorial(y)))) + 2
bicb<- aicb - 2 + log(14)
aicb
bicb
testmod2 <- lm(y~1)
AIC(logLik(testmod2))
BIC(logLik(testmod2))

lrnum <- -14*(13) + 182*log(13) - sum(log(factorial(y)))
lrdenom <- -7*(74/7) + 74*log(74/7) - sum(log(factorial(y))[1:7]) - 7*(108/7) + 108*log(108/7) - sum(log(factorial(y))[8:14])
lrs <- -2*(lrnum - lrdenom)
pchisq(lrs, df=1, lower.tail=F)


tht <- c(74/49,108/49)*diag(2)                              
cont <- c(1,-1)
se <- sqrt(t(cont)%*%tht%*%cont)
t <- ((74/7) - (108/7))/se
t

