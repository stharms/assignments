setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 1")

##################################
###EXERCISE 1################
#A#
#Read the data from the webpage. No headers, so I name the variables with col.names
scores <- read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/student-apt.dat", header = FALSE, col.names = c("group", "aptitude","mathematics","language", "generalknowledge"))
#Then I attach the table so I can reference the variables directly
attach(scores)

#B#
#Create a scatterplot matrix to compare scores across all variables. I color them by group, and expand the margin area so I can include a legend.
pairs(~aptitude + mathematics + language + generalknowledge, main = "Student Scores by Area - 3 disciplines", pch=21, bg = c("red", "green", "blue")[(scores$group)], oma = c(5,20,5,5))
#Now add a legend for the colors of each group.
legend(x = .05, y= .903, legend = c("Technical", "Architecture", "Medical Tech"),  fill=c("red", "green", "blue"), title = "Group")

#C#
#In general, it appears that the architecture students scored consistently higher than other groups in every area except for general knowledge, in which medical technology students scored higher.
#The students in technical disiplines generally scored between the other 2 groups, and were very scattered in every area.
#Across all 3 groups there is a positive correlation between aptitude, mathematics, and language scores, but no visible correlation between general knowledge and any of the other scores areas.

detach(scores)
rm(list= ls()) #remove all variables since we're done with them

##################################
###EXERCISE 2#########################
#A#
#Read the Pi Digits file from the web page. I skip the first 60 irrelevant rows, and name the variable.
PiDigits <- read.table(file = "http://www.itl.nist.gov/div898/strd/univ/data/PiDigits.dat", header= FALSE, skip = 60, col.names = "Digit")
#attach the table so I can reference the variable directly
attach(PiDigits)

#B#
#Use the table() funciton to create a frequency table, then view the table in the console
freqtable <- table(PiDigits)
freqtable

#C#
#Create a bar plot with using the frequency table. Add titles, expand the y-axis and change the color to make the chart more appealing
barplot(freqtable, main = "Frequency of Each Digit in first 5000 Digits of Pi", xlab = "Digit", ylab = "Count", ylim= c(0, 600), col = "purple")

#D#
#Use the chisq.test() function to conduct a chi-square test for the count of the digits
chisq.test(freqtable)
#The p-value for the Chi-square test is 0.3224, which means that we cannot reject the null hypothesis, which is that all digits appear at an equal rate. The bar plot illustrates this well.

################################
###EXCERCISE 3##################
xvals <- seq(from=0, to=3, by = .01) #generate a sequence of x values
y1 <- xvals #generate function y = x
y2 <- xvals^2 #generate function y = x^2
y3 <- sqrt(xvals) #generate function y = sqrt(x)
df <- data.frame(xvals,y1,y2,y3) #combine them all into a data frame

#Plot all 3 lines in the same frame. I expand the y-axis to include the maximum values, and also color the lines.
plot(x = xvals, y = y1, type = "l", lwd = 3, xlim = c(0 , max(xvals)), xlab = "X", ylim = c(0, max(y1, y2, y3) + .3), ylab = "Y", main = "Plot of 3 Functions", oma = c(5,20,5,5))
lines(x = xvals, y = y2, col = "blue", lwd = 3) #a line for the y = x^2 function
lines(x = xvals, y = y3, col = "red", lwd = 3) #another line for the y = sqrt(x) function
abline(v= c(1,2,3), lty= 5) #add some vertical lines for x
ccc
###############################
###EXERCISE 4##################
#A and B#
rm(list=ls())

#I did both a) and b) together here
#I put the pressure data into a data frame first, and then added my fahrenheit vector directly into the frame
frame <- data.frame(pressure) #create data frame
names(frame)= c("Celsius", "Pressure") #add my names for the variables into the frame
frame$Fahrenheit = 9*frame$Celsius/5 + 32 #Create a new variable that converts Celsius to Fahrenheit
attach(frame)

#C#
#Plot Fahrenheit vs. Pressure, and store it as a variable for easy reference later
plot(y = Fahrenheit, x = Pressure, type = "p", lwd = 2, col = "red", main = "Temperature vs. Pressure", ylab = "Temp in degrees F", xlab="Pressure in mm", ylim = c(0, 1000))
plotC <- recordPlot() #save the plot for later


#D#
#Create a simple linear regression model for Temperature vs. Pressure
noint <- lm(formula = Fahrenheit ~ Pressure -1, data=frame) #The -1 will remove the intercept term from the model
fitted <- fitted(noint) #Create fitted values from the model
summary(noint) #View the summary of the model results

plot.new() #Clear the old plot
plotC #recall the plot
lines(x = Pressure, y = fitted, lwd = 3, col = "blue") #Plot the fitted line onto the plot from (c)
plotD <- recordPlot() #Save the plot for later

#E#
resids <- resid(noint) #Create a vector of residuals from the no intercept simple linear regression
#Plot the Residuals vs. the Fitted values
plot.new()
residualplotE <- plot(x=fitted, y = resids, xlab = "Fitted Values", ylab= "Residuals", main = "Residuals vs. Fitted")


#F#

newframe <- data.frame(Fahrenheit, Pressure, Pressure^2, Pressure^3) #Create a new data frame, directly applying the functions to pressure data
names(newframe)<- c("Fahrenheit", "Pressure", "SquaredPressure", "CubedPressure") #Add names for our new vectors in our new data frame

detach(frame) #detach the old data frame
attach(newframe)  #attach the new one

#G
#Create a multiple regression model using our new function
newmodel <- lm(formula = Fahrenheit ~ Pressure + SquaredPressure + CubedPressure, data=newframe)
#view a summary for the model output
summary(newmodel)
#Note that all of the coefficients are significant at 5% significant level

#H#
#First we create fitted values for the new multiple regression model
newfit <- fitted(newmodel)
#Then add it into our plot with previous regression line and actual data
plot.new()
plotD #Recall the plot from earlier
lines(x = Pressure, y = newfit, lwd = 3, col = "green") #add the new fitted line to the old plot

#I#
#Create Residuals from the new multiple regression model
newresids <- resid(newmodel)
#Create a new residual vs. fitted value plot
plot.new()
residualplotI <- plot(x = newfit, y = newresids, xlab = "Fitted Values", ylab= "Residuals", main = "Residuals vs. Fitted")

