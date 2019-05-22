# This is a function that provides the p-value of the test for the null
# hypothesis that the first q PC's are adequate in representing a certain
# proportion of the total variation in the data. It follows the theoretical
# developments in the class and can be obtained using the material in
# Anderson (1984)'s book titled "An Introduction to Multivariate Statistical
# Analysis". Note that while this test is not formally presented there, the
# ingredients are all present.  
# 
# Ranjan Maitra (03/30/2012)

PCs.proportion.variation.enuff <- function(lambda, q = 1, propn, nobs)
  {
    den <- sum(lambda) # sum of all the eigenvalues
    num <- sum(lambda[1:q]) # sum of the first q eigenvalues
    if (num/den >= propn) return(1)
    else {
      se <- sqrt(2 * sum(lambda[-(1:q)])^2 * sum(lambda[1:q]^2) +
                 2 * sum(lambda[1:q])^2 * sum(lambda[-(1:q)]^2)) /
                   (sqrt(nobs) * den^2)
                                        #asymptotic sd of the test statistic
      test.stat <- (num/den - propn)/se
      return(pnorm(test.stat))
    }
  }
