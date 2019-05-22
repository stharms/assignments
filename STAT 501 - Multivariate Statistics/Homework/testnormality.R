# Normal Q-Q plot
X = read.table("board.stiffness.dat", head = FALSE)

QQplot.normal = function(x){
  # x is an observed vector of a variable
  x.sort = sort(x)
  n = length(x)
  p = ( c(1 : n) - 0.5 ) / n
  q = qnorm(p)
  res = data.frame(cbind(x.sort, q))
  names(res) = c("sample.quantile", "normal.quantile")
  return(res)
}

# we will now do an example applying Cramer's characterization of the normal
# distribution, but we will do this in some sort of efficient manner so that R
# will be happy.
# Multiple testing summarized by q-value

# we generate N = 100,000 random unit projections in 2-D
#

testnormality <- function(X, numproj = 100000)
  {
    # note that the value returned is the q-value of the test
    p = ncol(X)
    n = nrow(X)
    x <- matrix(rnorm(numproj * p), nrow = p)
                                        # generate 100,000, standard
                                        # p-variate
                                        # normal random variables.


    y <- matrix(sqrt(apply(x^2, 2, sum)), nrow = p, ncol = numproj, by = T)
  
    z <- x / y

    tempdat <- as.matrix(X) %*% z  # this gives rise to a p x numproj matrix
                                   # called tempdat here

    # perform Shapiro-Wilks' test and calculate individual p-values on each of
    # the numproj observation sets.

    pvals <- as.numeric(matrix(unlist(apply(tempdat, 2, shapiro.test)),
                             ncol=4, by = T)[,2])

    return(min(sort(pvals) * numproj / (1:numproj)))
  }
