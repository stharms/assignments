library(itsmr)

mink <- ts(read.table("apph.tsm"))
mink <- log(mink)
ts.plot(mink)
acf(mink, type="covariance", main = "ACVF")
plota(mink, h=20)
plots(mink)
periodogram(mink) + abline(v=2*pi/9)+ abline(v=2*pi/23)

mink1 <- diff(mink,lag=9)
ts.plot(mink1)
mink1t <- Resid(mink, M=c("log","diff",9,"trend",1))
ts.plot(mink1t)
acf(mink1t,type="covariance")
plota(mink1t, h=20)
plots(mink1)
periodogram(mink1t) + abline(v=2*pi/14)

mink14 <- diff(mink1, lag=14)
mink14<- Resid(mink, M=c("log" ,"diff",9,"trend",1, "diff",14))
ts.plot(mink14)
acf(mink14,type="covariance")
plota(mink14, h=20)
plots(mink14)
periodogram(mink14)

minks <- smooth.ma(mink14,6)
acf(minks, type="covariance", main = "ACVF")
plota(minks, h=20)
plotc(minks, mink14)
plotc(minks+mink, mink+mink14)

minkd1 <- diff(mink, lag=1)
periodogram(minkd1)
###########
arma11 <- arma(mink1t, p=1, q=1)
arma10 <- arma(mink1t, p=1, q=0)
arma01<- arma(mink1t, p=0, q=1)
arma00 <- arma(mink1t, p=0, q=0)
basic <- autofit(mink, p=0:5, q=0:10)
mink9mod <- autofit(mink1t, p=0:1, q=0:5)
arma11 <- arma(mink14, p=1, q=1)


ma1model <- autofit(mink14, p=0:5, q=0:10)
smmodel <- autofit(minks, p=0:8, q=0:8)

c(arma01$phi - qnorm(.975)*arma01$se.phi, arma01$phi + qnorm(.975)*arma01$se.phi)
c(arma01$theta - qnorm(.975)*arma01$se.theta, arma01$theta + qnorm(.975)*arma01$se.theta)
c(arma11$phi - qnorm(.975)*arma11$se.phi, arma11$phi + qnorm(.975)*arma11$se.phi)
c(arma11$theta - qnorm(.975)*arma11$se.theta, arma11$theta + qnorm(.975)*arma11$se.theta)


test(Resid(mink,M=c("log" ,"diff",9,"trend",1, "diff",14), a=ma1model))
test(Resid(mink, M=c("log" ,"diff",9,"trend",1), a=mink9mod))

forecast(mink, M=c("log" ,"diff",9,"trend",1, "diff",14), a=ma1model, h=10)
forecast(mink, M=c("log" ,"diff",9,"trend",1), a=mink9mod, h=10)
######################################### 
resids9 <- Resid(mink, M=c("log" ,"diff",9,"trend",1), a=mink9mod)
plota(resids9)
