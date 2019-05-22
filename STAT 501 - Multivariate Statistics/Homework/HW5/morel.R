morel = read.table(file = "morel.dat",  col.names=c("studentgroup", "aptitude", "mathematics", "language", "generalknowledge"))
morel
n = dim(morel)[1]

morel[,1] = as.factor(morel[,1])
fit = manova(as.matrix(morel[,-1]) ~ morel[,1])

# understanding the manova output
fit
ls(fit)
fit$residuals
cov(fit$residuals)

# residual sum of squares
diag(cov(fit$residuals)) * (n - 1)

fit$coefficients

# trt sum of squares
diag(cov(fit$fitted.values)) * (n - 1)

# check the decomposition of sum of squares
cov(morel[,-1])
cov(fit$fitted.values) + cov(fit$residuals)

# test for equality of means of the three groups
summary(fit, test="Wilks")
summary(fit, test="Pillai")

# Wilks likelihood ratio statistic
det(cov(fit$residuals) * (n - 1)) / det(cov(morel[,-1]) * (n - 1))

# Test for interactions
M = matrix(c(1, 0, 0, -1, 1, 0, 0, -1, 1, 0, 0, -1), ncol = 4)

fitp <- manova((as.matrix(morel[, -1]) %*% t(M)) ~ morel[, 1])
summary(fitp, test="Wilks")
