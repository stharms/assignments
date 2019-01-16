install.packages("Sleuth3")
library(Sleuth3)
library(ggplot2)
library(MASS)
library(tidyverse)
library(lsmeans)
df <- case0501
#######
###a)
g <- ggplot(data = df, aes(x = Diet, y = Lifetime))
g + geom_boxplot()

#####
##b)
mod <- lm(Lifetime~0+Diet, data = df)
anva <- anova(mod)
anva
anva$`Sum Sq`[2]
#alternative using matrices
lenx <- length(df$Diet)
diets <- arrange(df, Diet)
Ident <- diag(lenx)
X <- model.matrix(~0+factor(diets$Diet))
Px <- X%*%ginv(t(X)%*%X)%*%t(X)
y <- diets$Lifetime
sse <- t(y)%*%(Ident-Px)%*%y
sse
######
##c)
#Using ANOVA Table
anva
s2 <- anva$`Mean Sq`[2]

#using matrices
sig2hat <- sse/(lenx - 6)
sig2hat

########
##d)
#using lm
dietsn <- diets
#recode
dietsn$Diet[dietsn$Diet=="N/R50"] <-  "lopro"
#lm
mod.red <- lm(Lifetime~0+Diet, data = dietsn)
anva.red <- anova(mod.red)
anva.red
anva.red$`Sum Sq`[2]
#using matrices
Xo <- matrix(data = c(rep(c(1, rep(0, times = 4)), times = 57), rep(c(0,1,0,0,0), times = 60),
                     rep(c(0,0,0,0,1), times = 71), rep(c(0,0,1,0,0), times = 49),
                     rep(c(0,0,0,1, 0), times = 56), rep(c(0,0,0,0,1), times = 56))
            , nrow = lenx, byrow = T)
Xo <- model.matrix(~0+factor(dietsn$Diet))
Po <- Xo%*%ginv(t(Xo)%*%Xo)%*%t(Xo)
y <- diets$Lifetime
sse.red <- t(y)%*%(Ident-Po)%*%y
sse.red
###########
##e)
#using anova()
anova(mod.red, mod)$F[2]
#using matrices
Fstat <- ((sse.red - sse)/(anva.red$Df[2]-anva$Df[2])) / (sse/anva$Df[2])
Fstat
pf(Fstat, df1 = (anva.red$Df[2]-anva$Df[2]), df2 = (sse - anva$Df[2]))

#######
##g)
C = t(c(0,0,1,0,0,-1))
q = 1
d = 0
y <- diets$Lifetime
X <- model.matrix(~0+factor(diets$Diet))
betahat = ginv(t(X)%*%X)%*%t(X)%*%y
Fc = (t((C%*%betahat))%*%solve(C%*%(ginv(t(X)%*%X))%*%t(C))%*%(C%*%betahat) / 1 ) / (sig2hat)
Fc

rm(list= ls())


A <- matrix(c(-1,1,1,-1), nrow = 2, byrow = T)
A
G <- matrix(c(-1/2,-1,1,-1/2), nrow = 2, byrow = T)
G
A%*%G%*%A
A
G


#############
#4
a <- cbind(c(rep(1, times = 6)), c(0,0,1,1,0,0), c(0,0,0,0,1,1))
a
a%*%solve(t(a)%*%a)%*%t(a)
###############
?pt
pt(.975, df = 10)
t <- qt(.975, df = 39)
sig <- 10*.2252*.2252
marg <- t*.3184
.87 + marg
.87 - marg
x1 <- cbind(c(rep(1, times = 43)))
x2 <- c(rep(0, times =10), rep(1, times = 10), rep(0, times =10),rep(1, times =13))
x3 <- c(rep(0, times =10), rep(0, times = 10), rep(1, times =10),rep(1, times =13))
x4 <- c(rep(0, times =10), rep(0, times = 10), rep(0, times =10),rep(1, times =13))
x <- cbind(x1,x2,x3,x4)
x
solve(t(x)%*%x)
###############################
2*pt(-1.075, df = 5)
 xst <- cbind(c(rep(1, times = 8)), c(1,1,1,1,-1,-1,-1,-1), c(1,1,-1,-1,1,1,-1,-1))
library(MASS)
bst <- solve(t(xst)%*%xst)%*%t(xst)
#####################3
w <- cbind(c(2,-1,0),c(-1,2,0),c(0,0,1))
w
solve(w)
