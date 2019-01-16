rm(list=ls())
library(lme4)
library(lsmeans)
library(tidyverse)
library(phia)
##########################
fabric <- read.table(file="fabric.txt", header = T)
fabric$surface <- as.factor(fabric$surface)
fabric$filler<-factor(fabric$filler)
fabric$p <- factor(fabric$p)
fabric <- fabric %>% arrange(filler)
View(fabric)
lmer(data = fabric, formula = y ~ surface + filler + p)
cm <- lm(data = fabric, formula = y ~ surface + filler + p + surface:filler + surface:p + filler:p + surface:filler:p)
cm$coefficients
c <- c(cm$coefficients)
c[1]+c[3]+c[4]+c[9] 
mse <- anova(cm)$'Mean Sq'[8]
se <- sqrt(mse/2)
lsmeans(cm, ~ surface + filler + p + surface:filler + surface:p + filler:p + surface:filler:p)[7]

fitmod <- fitted(cm)
lsmeans(cm, ~filler)
lsmeans(cm, ~surface+p)[2]

ff <- lsmeans(cm,~filler)
cnt <-contrast(ff, method = "eff", interaction = T)
test(cnt)
ff
anova(cm)
(268.7/24)^.5
214.75-181.0833333
mse <- 268.7
tst <- (214.75-181.083333)/((mse*(1/6))^.5)
tst
fst <- tst^2
pf(fst, 1, 12, lower.tail = F)
fst
pt(tst, df = 6, lower.tail=F)

testInteractions(cm, pairwise  = c("p","filler"))

fft <- lsmeans(cm, ~ surface + filler + p + surface:filler + surface:p + filler:p + surface:filler:p)
eft <- lsmeans(cm, ~filler|p)
int <-contrast(eft, method = "eff", interaction=T)
int
test(int)
fullm <- cm
redm <- lm(data=fabric, formula = y~surface + filler + p)
redmm <- lm(data=fabric, formula = y~surface+filler+p+p:surface+filler:surface)
fullmm <- lm(data = fabric, formula = y~surface + filler + p + surface:filler + surface:p + filler:p)
anova(fullm,redm)
anova(fullm)
anova(redmm, fullm)
anova(redmm,fullmm)
anova(fullmm)
