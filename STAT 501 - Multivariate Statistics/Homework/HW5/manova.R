morel <- read.table(file = "morel.dat",  col.names=c("studentgroup", "aptitude", "mathematics", "language", "generalknowledge"))

morel$studentgroup <- as.factor(morel$studentgroup)

library(car)

yfit.lm <- lm(cbind(aptitude, mathematics, language, generalknowledge)~studentgroup , data = morel)
# Note that by default, R puts the first treatment equal to zero
#
# If we want to do the same as SAS, we have to use the contrasts option:
#
#
fit.lm <- lm(cbind(aptitude, mathematics, language, generalknowledge)~studentgroup , data = morel, contrasts = list(studentgroup = contr.SAS))



fit.manova <- Manova(fit.lm)

summary(fit.manova)

Type II MANOVA Tests:

Sum of squares and products for error:
                  aptitude mathematics  language generalknowledge
aptitude         55036.195   8140.0376 5569.9774        5490.1278
mathematics       8140.038  14588.9983 2619.2097        -128.0217
language          5569.977   2619.2097 4759.8655        -102.4044
generalknowledge  5490.128   -128.0217 -102.4044        7040.6248

------------------------------------------

Term: studentgroup

Sum of squares and products for the hypothesis:
                  aptitude mathematics  language generalknowledge
aptitude         24600.207    6832.584 5709.5104       -2040.4327
mathematics       6832.584    2329.599 1570.1805       -1064.7222
language          5709.510    1570.181 1325.6955        -455.5712
generalknowledge -2040.433   -1064.722 -455.5712         743.4849

Multivariate Tests: studentgroup
                 Df test stat  approx F num Df den Df     Pr(>F)
Pillai            2 0.4926982  6.292329      8    154 4.8238e-07 ***
Wilks             2 0.5434483  6.773565      8    152 1.3843e-07 ***
Hotelling-Lawley  2 0.7735884  7.252392      8    150 4.0901e-08 ***
Roy               2 0.6750591 12.994889      4     77 3.9151e-08 ***
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1  1

> names(fit.manova)
[1] "SSP"      "SSPE"     "df"       "error.df" "terms"    "repeated" "type"
[8] "test"


fit.manova$SSPE
                  aptitude mathematics  language generalknowledge
aptitude         55036.195   8140.0376 5569.9774        5490.1278
mathematics       8140.038  14588.9983 2619.2097        -128.0217
language          5569.977   2619.2097 4759.8655        -102.4044
generalknowledge  5490.128   -128.0217 -102.4044        7040.6248


C <- matrix(c(0, 1, 0, 0, 1, -1), ncol = 3,  by = T)
M <- matrix(c(1, 0, 0, -1, 1, 0, 0, -1, 1, 0, 0, -1), nrow = 4, by = T)

newfit <- linearHypothesis(model = fit.lm, hypothesis.matrix = C)

print(newfit)

Sum of squares and products for the hypothesis:
                  aptitude mathematics  language generalknowledge
aptitude         24600.207    6832.584 5709.5104       -2040.4327
mathematics       6832.584    2329.599 1570.1805       -1064.7222
language          5709.510    1570.181 1325.6955        -455.5712
generalknowledge -2040.433   -1064.722 -455.5712         743.4849

Sum of squares and products for error:
                  aptitude mathematics  language generalknowledge
aptitude         55036.195   8140.0376 5569.9774        5490.1278
mathematics       8140.038  14588.9983 2619.2097        -128.0217
language          5569.977   2619.2097 4759.8655        -102.4044
generalknowledge  5490.128   -128.0217 -102.4044        7040.6248

Multivariate Tests:
                 Df test stat  approx F num Df den Df     Pr(>F)
Pillai            2 0.4926982  6.292329      8    154 4.8238e-07 ***
Wilks             2 0.5434483  6.773565      8    152 1.3843e-07 ***
Hotelling-Lawley  2 0.7735884  7.252392      8    150 4.0901e-08 ***
Roy               2 0.6750591 12.994889      4     77 3.9151e-08 ***
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

newfit <- linearHypothesis(model = fit.lm, hypothesis.matrix = C, P = M)

print(newfit)

 Response transformation matrix:
                 [,1] [,2] [,3]
aptitude            1    0    0
mathematics        -1    1    0
language            0   -1    1
generalknowledge    0    0   -1

Sum of squares and products for the hypothesis:
           [,1]     [,2]     [,3]
[1,] 13264.6375 363.6553 5115.040
[2,]   363.6553 514.9337  853.636
[3,]  5115.0403 853.6360 2980.323

Sum of squares and products for error:
          [,1]      [,2]      [,3]
[1,] 53345.119 -9399.728 -2667.382
[2,] -9399.728 14110.444 -2115.038
[3,] -2667.382 -2115.038 12005.299

Multivariate Tests:
                 Df test stat  approx F num Df den Df     Pr(>F)
Pillai            2 0.4539686  7.634505      6    156 3.3709e-07 ***
Wilks             2 0.5689034  8.362413      6    154 7.4605e-08 ***
Hotelling-Lawley  2 0.7175639  9.089142      6    152 1.7098e-08 ***
Roy               2 0.6563063 17.063963      3     78 1.2962e-08 ***
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1


summary(yfit.lm)
Response aptitude :

Call:
lm(formula = aptitude ~ studentgroup, data = morel, contrasts = contr.SAS(4))

Residuals:
   Min     1Q Median     3Q    Max
-42.16 -17.16 -10.58  21.39  58.00

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     39.000      5.504   7.086 5.11e-10 ***
studentgroup2   28.158      6.973   4.038 0.000124 ***
studentgroup3  -11.571      7.966  -1.453 0.150316
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.39 on 79 degrees of freedom
Multiple R-squared: 0.3089,	Adjusted R-squared: 0.2914
F-statistic: 17.66 on 2 and 79 DF,  p-value: 4.589e-07


Response mathematics :

Call:
lm(formula = mathematics ~ studentgroup, data = morel, contrasts = contr.SAS(4))

Residuals:
    Min      1Q  Median      3Q     Max
-32.391  -8.095   0.360   8.816  23.816

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     47.391      2.834  16.725   <2e-16 ***
studentgroup2    3.793      3.590   1.056   0.2940
studentgroup3   -9.296      4.102  -2.266   0.0262 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.59 on 79 degrees of freedom
Multiple R-squared: 0.1377,	Adjusted R-squared: 0.1159
F-statistic: 6.307 on 2 and 79 DF,  p-value: 0.002875


Response language :

Call:
lm(formula = language ~ studentgroup, data = morel, contrasts = contr.SAS(4))

Residuals:
     Min       1Q   Median       3Q      Max
-19.2895  -4.2895   0.5509   5.4972  19.8571

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     70.609      1.619  43.625  < 2e-16 ***
studentgroup2    6.681      2.051   3.258  0.00166 **
studentgroup3   -2.466      2.343  -1.053  0.29577
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.762 on 79 degrees of freedom
Multiple R-squared: 0.2178,	Adjusted R-squared: 0.198
F-statistic:    11 on 2 and 79 DF,  p-value: 6.097e-05


Response generalknowledge :

Call:
lm(formula = generalknowledge ~ studentgroup, data = morel, contrasts = contr.SAS(4))

Residuals:
     Min       1Q   Median       3Q      Max
-23.1905  -5.4424   0.4737   2.7826  26.8095

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     19.217      1.968   9.763 3.18e-15 ***
studentgroup2    2.309      2.494   0.926  0.35738
studentgroup3    7.973      2.849   2.798  0.00645 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9.44 on 79 degrees of freedom
Multiple R-squared: 0.09551,	Adjusted R-squared: 0.07261
F-statistic: 4.171 on 2 and 79 DF,  p-value: 0.01896



summary(fit.lm)

Response aptitude :

Call:
lm(formula = aptitude ~ studentgroup, data = morel, contrasts = list(studentgroup = contr.SAS))

Residuals:
   Min     1Q Median     3Q    Max
-42.16 -17.16 -10.58  21.39  58.00

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     27.429      5.760   4.762 8.54e-06 ***
studentgroup1   11.571      7.966   1.453     0.15
studentgroup2   39.729      7.177   5.536 3.91e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.39 on 79 degrees of freedom
Multiple R-squared: 0.3089,	Adjusted R-squared: 0.2914
F-statistic: 17.66 on 2 and 79 DF,  p-value: 4.589e-07


Response mathematics :

Call:
lm(formula = mathematics ~ studentgroup, data = morel, contrasts = list(studentgroup = contr.SAS))

Residuals:
    Min      1Q  Median      3Q     Max
-32.391  -8.095   0.360   8.816  23.816

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     38.095      2.965  12.846  < 2e-16 ***
studentgroup1    9.296      4.102   2.266 0.026159 *
studentgroup2   13.089      3.695   3.542 0.000669 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.59 on 79 degrees of freedom
Multiple R-squared: 0.1377,	Adjusted R-squared: 0.1159
F-statistic: 6.307 on 2 and 79 DF,  p-value: 0.002875


Response language :

Call:
lm(formula = language ~ studentgroup, data = morel, contrasts = list(studentgroup = contr.SAS))

Residuals:
     Min       1Q   Median       3Q      Max
-19.2895  -4.2895   0.5509   5.4972  19.8571

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     68.143      1.694  40.230  < 2e-16 ***
studentgroup1    2.466      2.343   1.053    0.296
studentgroup2    9.147      2.111   4.334 4.28e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.762 on 79 degrees of freedom
Multiple R-squared: 0.2178,	Adjusted R-squared: 0.198
F-statistic:    11 on 2 and 79 DF,  p-value: 6.097e-05


Response generalknowledge :

Call:
lm(formula = generalknowledge ~ studentgroup, data = morel, contrasts = list(studentgroup = contr.SAS))

Residuals:
     Min       1Q   Median       3Q      Max
-23.1905  -5.4424   0.4737   2.7826  26.8095

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     27.190      2.060  13.199  < 2e-16 ***
studentgroup1   -7.973      2.849  -2.798  0.00645 **
studentgroup2   -5.664      2.567  -2.207  0.03025 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9.44 on 79 degrees of freedom
Multiple R-squared: 0.09551,	Adjusted R-squared: 0.07261
F-statistic: 4.171 on 2 and 79 DF,  p-value: 0.01896


#
# How to get other contrasts? Suppose we wanted \tau_2 = 0.
#
# Use R function contr.treatment
#
#

morel$studentgroup <- C(object = morel$studentgroup, contr = contr.treatment(n = 4,  base = 2))
#
# Note that C() ["C" is in capitals] and is not the combine c() function.
#
# Also, base = 2 specifies that we want the second treatment to be the baseline

fit.lm2 <- lm(cbind(aptitude, mathematics, language, generalknowledge)~studentgroup , data = morel)

summary(fit.lm2)






Response aptitude :

Call:
lm(formula = aptitude ~ studentgroup, data = morel)

Residuals:
   Min     1Q Median     3Q    Max
-42.16 -17.16 -10.58  21.39  58.00

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     67.158      4.282  15.685  < 2e-16 ***
studentgroup1  -28.158      6.973  -4.038 0.000124 ***
studentgroup3  -39.729      7.177  -5.536 3.91e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.39 on 79 degrees of freedom
Multiple R-squared: 0.3089,	Adjusted R-squared: 0.2914
F-statistic: 17.66 on 2 and 79 DF,  p-value: 4.589e-07


Response mathematics :

Call:
lm(formula = mathematics ~ studentgroup, data = morel)

Residuals:
    Min      1Q  Median      3Q     Max
-32.391  -8.095   0.360   8.816  23.816

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     51.184      2.204  23.218  < 2e-16 ***
studentgroup1   -3.793      3.590  -1.056 0.293966
studentgroup3  -13.089      3.695  -3.542 0.000669 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 13.59 on 79 degrees of freedom
Multiple R-squared: 0.1377,	Adjusted R-squared: 0.1159
F-statistic: 6.307 on 2 and 79 DF,  p-value: 0.002875


Response language :

Call:
lm(formula = language ~ studentgroup, data = morel)

Residuals:
     Min       1Q   Median       3Q      Max
-19.2895  -4.2895   0.5509   5.4972  19.8571

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     77.289      1.259  61.380  < 2e-16 ***
studentgroup1   -6.681      2.051  -3.258  0.00166 **
studentgroup3   -9.147      2.111  -4.334 4.28e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7.762 on 79 degrees of freedom
Multiple R-squared: 0.2178,	Adjusted R-squared: 0.198
F-statistic:    11 on 2 and 79 DF,  p-value: 6.097e-05


Response generalknowledge :

Call:
lm(formula = generalknowledge ~ studentgroup, data = morel)

Residuals:
     Min       1Q   Median       3Q      Max
-23.1905  -5.4424   0.4737   2.7826  26.8095

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)     21.526      1.531  14.056   <2e-16 ***
studentgroup1   -2.309      2.494  -0.926   0.3574
studentgroup3    5.664      2.567   2.207   0.0302 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 9.44 on 79 degrees of freedom
Multiple R-squared: 0.09551,	Adjusted R-squared: 0.07261
F-statistic: 4.171 on 2 and 79 DF,  p-value: 0.01896


#
# alternative (preferred way) defining C
#
#

C1 <- matrix(c(1, 0, 1, 0, 0, -1, 0, 1, -1), ncol = 3, by = T)
newfit1 <- linearHypothesis(model = fit.lm, hypothesis.matrix = C1[-1, ])
C1 %*% yfit.lm$coef

     aptitude mathematics  language generalknowledge
[1,] 27.42857   38.095238 68.142857        27.190476
[2,] 11.57143    9.296066  2.465839        -7.973085
[3,] 39.72932   13.088972  9.146617        -5.664160

# compare with fit.lm$coef (exactly the same)



# Two-way MANOVA

peanuts <- read.table(file = "peanuts.dat",  col.names=c("location", "variety", "yield", "weight", "seedsize"))

peanuts$location <- as.factor(peanuts$location)
peanuts$variety <- as.factor(peanuts$variety)


fit.lm <- lm(cbind(yield, weight, seedsize) ~ location * variety, data = peanuts)

fit.manova <- Manova(fit.lm)

summary(fit.manova)

Type II MANOVA Tests:

Sum of squares and products for error:
           yield  weight seedsize
yield    104.205  49.365   76.480
weight    49.365 352.105  121.995
seedsize  76.480 121.995   94.835

------------------------------------------

Term: location

Sum of squares and products for the hypothesis:
               yield    weight    seedsize
yield      0.7008333  -10.6575    7.129167
weight   -10.6575000  162.0675 -108.412500
seedsize   7.1291667 -108.4125   72.520833

Multivariate Tests: location
                 Df test stat approx F num Df den Df   Pr(>F)
Pillai            1  0.893484 11.18432      3      4 0.020502 *
Wilks             1  0.106516 11.18432      3      4 0.020502 *
Hotelling-Lawley  1  8.388243 11.18432      3      4 0.020502 *
Roy               1  8.388243 11.18432      3      4 0.020502 *
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

------------------------------------------

Term: variety

Sum of squares and products for the hypothesis:
            yield    weight seedsize
yield    196.1150  365.1825  42.6275
weight   365.1825 1089.0150 414.6550
seedsize  42.6275  414.6550 284.1017

Multivariate Tests: variety
                 Df test stat  approx F num Df den Df    Pr(>F)
Pillai            2  1.709109  9.792388      6     10 0.0010562 **
Wilks             2  0.012444 10.619086      6      8 0.0019275 **
Hotelling-Lawley  2 21.375675 10.687838      6      6 0.0054869 **
Roy               2 18.187611 30.312685      3      5 0.0012395 **
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1

------------------------------------------

Term: location:variety

Sum of squares and products for the hypothesis:
            yield   weight  seedsize
yield    205.1017 363.6675 107.78583
weight   363.6675 780.6950 254.22000
seedsize 107.7858 254.2200  85.95167

Multivariate Tests: location:variety
                 Df test stat  approx F num Df den Df   Pr(>F)
Pillai            2  1.290861  3.033867      6     10 0.058708 .
Wilks             2  0.074300  3.558197      6      8 0.050794 .
Hotelling-Lawley  2  7.544290  3.772145      6      6 0.065517 .
Roy               2  6.824094 11.373490      3      5 0.011340 *
---
Signif. codes:  0 *** 0.001 ** 0.01 * 0.05 . 0.1   1








