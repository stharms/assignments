rllik <- function(x){
  (log(.5*(1/(1+x^2)+ x^2/((1+x^2)^2*(1+x^2-(x^2/(1+x^2)))))) + .5*(log(1+x^2+x^4)))
} 

thetas <- seq(0,5, length=1000)
lliks <- rllik(thetas)
plot(lliks~thetas, type="l",  xlab=expression(theta) , ylab="reduced log-likelihood")
png("ma1rll.png")

optim(par=1, fn=rllik, method="Brent", lower=0, upper=10)

sig2 <- function(x){
  .5*(1/(1+x^2)+ x^2/((1+x^2)^2*(1+x^2-(x^2/(1+x^2)))))
}
sig2(1)
