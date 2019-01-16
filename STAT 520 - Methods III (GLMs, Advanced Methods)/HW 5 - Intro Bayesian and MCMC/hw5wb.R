model{
  for(i in 1:n){
    y[i]~dgamma(alpha, beta[i])
    x[i]~dgamma(alpha, betac[i])
    beta[i] = alpha/mu
    betac[i] = alpha/muc
  }
  mu ~ dgamma(.001, .001)
  muc ~ dgamma(.001, .001)
  alpha ~ dt(0, pow(2.5,-2), 1)T(0,)
}
