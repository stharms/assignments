setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 8")
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(reshape2)

###########################################################
#Exercise 1
##a
housing <- txhousing
#I make two different plots, neither one is very useful
#First, a plot with a key for cities by color
a <- ggplot(data = housing, aes(x=date, y = sales, col = city))
a + geom_line()
#Now one with all lines the same color and no key
c<- ggplot(data = housing, aes(x=date, y = sales, z = city))
c + geom_line()
#Some of the larger cities had far more sales than the smaller cities, so the scale of the plot makes it difficult to see
#anything but the large cities. In addition, there appears to be a large seasonal effect of the sales that makes it difficult
#to examine the underlying trend

#b
#now a plot using log transformed sales
b <- ggplot(data = housing, aes(x=date, y = log(sales)))
b + geom_line(aes(z = city))
b + geom_line(aes(col = city))

#c
#Remove the na values from the housing set
housing <- filter(housing,!is.na(sales))
#fit the model using log(sales), with date and factor city as explanatory
model <- lm(log(sales)~factor(month) + city, data = housing)
modell <- by(housing, housing$city, function(x) lm(log(sales)~ month, data = housing))
resids <- residuals(modell)
#Save the residuals
housing$rel_sales <- model$residuals
housing$rel2 <- c(resids)
View(housing)
#Look at the new plot(s)
#Much better
g <- ggplot(data = housing, aes(x=date))
h<-g + geom_line(aes(y = rel_sales, col = city))
h<-g + geom_line(aes(y= rel_sales, z = city))
h

#d
#Get our monthly average residuals
dateavg <- housing %>% group_by(date) %>% summarize(mean(rel_sales))
names(dateavg) <- c("dates", "averagesales")
#Create columns of months and dates for easier plotting later
dateavg$year <- floor(dateavg$dates)
dateavg$month <- round(12*(dateavg$dates-dateavg$year), 0) + 1
#Add it to our previous plot with a different color so we know which one it is
i <- h + geom_line(data = dateavg, aes(x = dates, y = averagesales),col = "black", size = 2)
i

#e
#Get our monthly average
monthlyavg <- housing %>% group_by(month) %>% summarize(mean(rel_sales))
names(monthlyavg) <- c("months", "avgsales")
#Create a set of plots, one for each year
j <- ggplot(data = housing, aes(x = month, y = rel_sales))

#Make sure to add a black line with the mean values
k <- j + geom_line(aes(col = city)) + geom_line(data = dateavg, aes(x = month, y = averagesales), col = "black", size = 2)

#legend isn't really useful and just takes up space with so many different cities on the same plote
m <- k + theme(legend.position = "none")
#Facet by year
l <- m +facet_wrap(~year, nrow = 4, ncol = 4)
l
###########################################################
#Exercise 2
##a
breakdown <- read.table(file = "http://maitra.public.iastate.edu/stat501/datasets/breakdown.dat", header = F)
names(breakdown) <- c("Severity", "Problem", "Engineer", "Assessment", "Implementation","Resolution")

##b
#melt the variables into one column for easy faceting
breakmelt <- melt(breakdown, id.vars = c("Severity", "Engineer", "Problem"))
#plot the 3 variables side-by-side for each engineer type
bb <- ggplot(data = breakmelt, aes(x = Engineer, y = value))
bbb <- bb + geom_boxplot() + facet_grid(.~variable) + ylab("Time")
bbb
bb + geom_boxplot() + facet_grid(variable ~Problem + Severity) + ylab("Time")
##c
cc <- bbb + facet_grid(Severity ~ variable)
cc

##d
dd <- bb + geom_boxplot() + facet_grid(Problem ~ variable) + ylab("Time")
dd

dddd <- ggplot(data = breakmelt, aes(x = variable, y = value)) + geom_boxplot() + facet_grid(.~Problem) + ylab("Time")
dddd
