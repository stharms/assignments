setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 3")

install.packages("readxl")
library(dplyr)
##################################
###EXERCISE 1################
#A#
library(readxl)

wind <- data.frame(read_excel("wind.xls"))
wind

#B#
sumstats.function <- function(x){
  c(mean = mean(x), median = median(x), sd = sd(x), quartile = quantile(x), IQR = IQR(x))
}
apply(wind, 2, FUN = sumstats.function)

###############################################################
apply(wind, 2, FUN = summary)
apply(wind, 2, FUN = sd)
apply(wind, 2, FUN = mean)
apply(wind, 2, FUN = median)
apply(wind, 2, FUN = IQR)
apply(wind, 2, FUN = quantile)
apply(wind, 2, FUN = )
###############################################################

#C#
#To me, all of the summary statistics can make sense in this context. The mean and median are measures of average wind direction,
#So if I wanted to know which direction to expect the wind to be blowing in a certain season, I would use those measurements to
#make a guess. The standard deviation and quartiles are measures of how erratic the wind direction would be, so a large s.d. or IQR
#would suggest that the wind changes directions often (or to more extreme directions). So yes, as long as we have a reference point
#(i.e., the center of the circle for which  these angular measurements are taken), they have some use. However, note that there is no
#magnitude measurements so we have no idea how strong the wind is actually blowing in each direction (so it's not as useful as it might
#seem)

#D#
attach(wind)
#Use reshape to melt the data frame into something easier
library(reshape)
windmlt <- melt(wind)
names(windmlt) <- c("Season", "Direction")

#Convert degrees to radians, and then get unit circle coordinates
convert.f <- function(z){
  rad = z*pi/180
  x = cos(rad)
  y = sin(rad)
  return(data.frame(x,y))
}
windplot <- data.frame(windmlt$Season, convert.f(windmlt$Direction))

#Plot the direction coordinates around the circle
par(pty = "s")
plot(x = jitter(windplot$x, factor = 8), y = jitter(windplot$y, factor = 8),
     xlim = c(-1,1), xlab = "<-West  East->",
     ylim = c(-1,1), ylab = "North ->", cex = 1.2,
     main = "Wind Direction Observations by Season", 
     pch=21, bg = c("red", "green", "blue", "black")[(windmlt$Season)], oma = c(5,20,5,5),
     axes = F
     )
abline(h = 0, v = 0, lty = 5)
legend(x = -.4, y= .8, legend = c("Spring", "Summer", "Autumn", "Winter"),
       fill=c("red", "green", "blue", "black"), title = "Season")
#There is not much to notice in the measurements. It is clear that most of the winter observations (in black) were blowing to the
#south (below the x axis), while in summer most were blowing slightly to the north and east.
#Autumn had a few clustered to the South, either straight South or West/Southwest
#The most erratic was Spring without much obvious trend at all, although most are to the East, somewhat similar to the Summer observations
#In total, it is hard to see visually much of a trend across seasons, especially without any magnitude data.

################################################################################
##Exercise 2##
#A#
#Generate 2000 random samples from U(0,1) and fill in a 1000 x 2 matrix
m <- matrix(runif(2000), nrow = 1000, ncol = 2)

#B#
#generating sample mean for each sample
xbar <- .5*(m[,1] + m[,2])

#C#
#A qqplot for our sample means
qqnorm(xbar, main = "Q-Q Plot for 1000 Samples of Size n = 2")
#looks approximately normal

#D#
#n= 10
ten <- matrix(runif(n= 1000*10), nrow = 1000, ncol = 10)
xbar10 <- (1/(ncol(ten)))*(rowSums(ten))
qqnorm(xbar10, main = "Q-Q Plot for 1000 Samples of Size n = 10")

#n = 25
twofive <- matrix(runif(n= 1000*25), nrow = 1000, ncol = 25)
xbar25 <- (1/(ncol(twofive)))*(rowSums(twofive))
qqnorm(xbar25, main = "Q-Q Plot for 1000 Samples of Size n = 25")

#n = 100
hund <- matrix(runif(n= 1000*100), nrow = 1000, ncol = 100)
xbar100 <- (1/(ncol(hund)))*(rowSums(hund))
qqnorm(xbar100, main = "Q-Q Plot for 1000 Samples of Size n = 100")


#################################################
#Exercise 3######################
#############################################
#a
#Read in the data
titanic <- read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/titanic.txt", header = TRUE, sep = ",")

#b
crosstab <- table(titanic$Sex, titanic$PClass, useNA = "ifany")
crosstab
additional <- table(titanic$Sex, titanic$PClass, titanic$Survived, useNA = "ifany")
additional

#C
#First, subset into men & women
men <- subset(titanic, titanic$Sex == "male")
mensurvive <- subset(men, men$Survived == 1 & !is.na(men$Age))
mendead <- subset(men, men$Survived == 0 & !is.na(men$Age))
women <- subset(titanic, titanic$Sex == "female")
womensurvive <- subset(women, women$Survived == 1 & !is.na(women$Age))
womendead <- subset(women, women$Survived == 0 & !is.na(women$Age))

#Then, calculate mean age of those who survived and those who didn't separately, need to exclude NAs here
survmean.men <- mean(mensurvive$Age)
survmean.women <- mean(womensurvive$Age)
deadmean.women <- mean(womendead$Age)
deadmean.men <- mean(mendead$Age)


#Next, calculate standard errors using a function. Normally would need to include na.rm to remove NAs,
#but I'm already removing them so it doesn't matter for this exercise

stde <- function(x){
  sqrt(var(x)/ length(x))
}

#Calculate standard errors for each group
survstderr.women <- stde(womensurvive$Age)
survstderr.men <- stde(mensurvive$Age)
deadstderr.women <- stde(womendead$Age)
deadstderr.men <- stde(mendead$Age)

#View all of the values we calculated above
survmean.men
deadmean.men
survmean.women
deadmean.women

survstderr.men
deadstderr.men
survstderr.women
deadstderr.women


#We can just use the t-test function in R to test difference in mean age
t.test(mensurvive$Age, mendead$Age)
t.test(womensurvive$Age, womendead$Age)

#######################################
##Exercise 4###########################
######################################
#a
#Read in the data, row by row, to a 83 x 108 matrix
anat <- matrix(scan("anat.dat.txt"), nrow = 83, ncol = 108, byrow = TRUE)
activ <- matrix(scan("activ.dat.txt"), nrow = 83, ncol = 108, byrow = TRUE)

#b
#Create a grayscale color map of activation sites
require(grDevices); require(graphics);
filled.contour(activ, col = rev(gray(1:20/20)), axes=FALSE, main = "Brain Activation at Tapping of Finger", 
               key.axes = axis(4, seq(0, max(activ, na.rm = TRUE), by = .05)))
#c
#overlay the previous plot with the contour of the Brain's anatomy
#First need to resize the plot
par(oma=c(0,0,0,6))

#Then add the contour plot, note we need to add = TRUE to overlay
contour(anat, levels = c(100, seq(from = 250, to = 950, by = 100)), axes = FALSE, drawlabels= FALSE, add = TRUE)






filled.contour(activ, col = rev(gray(1:20/20)), axes=FALSE, main = "Brain Activation at Tapping of Finger", 
               )

#Ovelay the plot with the brain's anatomy

contour(anat, levels = c(100, seq(from = 250, to = 950, by = 100)), axes = FALSE, drawlabels= FALSE)
