setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 2")

elections2 <- read.table(file = "assignment.pdf", skip = 3, skipNul = TRUE, header = TRUE, nrows = 17, sep = " ")
##################################
###EXERCISE 1################
#A#

year <- seq(from = 2008, to = 1948, by = -4)
winner <- rev(c(175, 179, 179, 183, 193, 182, 182, 177, 185, 185, 188, 189, 189, 182, 182, 185))
opponent <- rev(c(173, 178, 178, 182, 180, 180, 185, 183, 177, 180, 173, 188, 187, 185, 193, 175))
elections <- data.frame(year, winner, opponent)
elections

electionsdiff <- data.frame(year, winner, opponent, difference = winner - opponent)
electionsdiff
elections$taller.won <- (winner - opponent > 0)
elections

percentages <- prop.table(table(elections$taller.won))
percentages

plot.new()

barplot(rev(electionsdiff$difference), names.arg= rev(elections$year), axes = TRUE,
        xlab = "Year", ylab = "Difference(cm)", main = "Difference in Height of Presidential Election Winners, 1948-2008",
        col = "BLUE", cex.names = .98, las=2)
axis(1, at = electionsdiff$year, labels = TRUE, tick = TRUE)

write.table(elections, file = "heights.csv", sep = ",", row.names = FALSE, col.names = c("Year", "Winner Height (cm)", "Loser Height (cm)", "Taller Won?"))
#write.csv(elections, file = "heights.csv", row.names = FALSE, col.names = c("Year", "Winner Height (cm)", "Loser Height (cm)", "Taller Won?"))

##################################
###EXERCISE 2################
#I downloaded the file into the working directory, reading from canvas does not work well
students <- read.table(file = "students.txt", header = TRUE,
                       sep = "", col.names = c("height", "shoe size", "gender", "population"))
#Calculating some summary statistics for our data set
mean(students$height)
mean(students$shoe.size)
sd(students$height)
sd(students$shoe.size)

#a summary of the gender column will give us counts by gender
summary(students$gender)

attach(students)
population

#Recode to colors
population <- as.character(population)
population[population == "kuopio"] <- "blue"
population[population == "tampere"] <- "red"


students$population
population2 <- ifelse(students$population == "kuopio", "blue", "red")


#Combine data into a new data frame
students_new <- data.frame(height, shoe.size, gender, population)
students_new

#Subset the data
male <- subset(students, gender == "male")
female <- subset(students, gender == "female")

#Write the subsets into text files, removing the quotes
write.table(male, file = "male.txt", row.names = FALSE, col.names = TRUE, quote = FALSE)
write.table(female, file = "female.txt", row.names = FALSE, col.names = TRUE, quote = FALSE)

#Subset the data based on the median. Problem didn't specify what to do with median so I just leave it out of the subsets
med <- median(height)
below <- subset(students, height < med)
abovem <- subset(students, height > med)

#Write the subsets into 2 new files
write.table(abovem, file = "abovem.csv", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ",")
write.table(below, file = "below.csv", row.names = FALSE, col.names = TRUE, quote = FALSE, sep = ",")


##################################
###EXERCISE 3################
#Read in the data. I keep column names given in the file for now
cars <- read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/cars.dat", header = TRUE)

#attach the data frame
attach(cars)

#convert to feet per second
fps <- speed * 5280 / (60*60)

#Plot speed vs. distance
plot(y = dist, x = fps, type = "p", ylab = "Distance (ft)", xlab = "Speed (ft/s)", main = "Speed vs. Stopping Distance",
     pch = 16, col = "blue")
plot1 <- recordPlot()

#Convert speeds and distance into metric
#Distance in meters
distance.metric <- dist*(1.6903*1000)/5280

#Speed in km/hr
speed.metric <- speed*1.6903

#Speed in meters/second
speed.ms <- fps*1.6903*1000/5280

#Detach data frame
detach(cars)

#Plot Speed against distance again, now in metric
plot(y = distance.metric, x = speed.ms, type = "p", ylab = "Distance (m)", xlab = "Speed (m/s)", main = "Speed vs. Stopping Distance",
     pch = 16, col = "black")
plot2 <- recordPlot()

#Print to pdf
dev.copy2pdf(file = "Speed vs Distance metric.pdf", device = plot2)

#Recall the old plot before saving
plot.new()
plot1

#Save to pdf file
dev.copy2pdf(file = "Speed vs Distance fps.pdf", device = plot1)

##################################
###EXERCISE 4################
#Read the data into R
pottery <- read.table(file = "http://www.public.iastate.edu/~maitra/stat579/datasets/pottery.dat", header = TRUE)

#I use the aggregate() function to find summary statistics by site,
#much faster than subsetting and then finding summary statistics for each
aggregate(pottery[,-6], by = list(pottery$Site), FUN = summary)
aggregate(pottery[,-6], by = list(pottery$Site), FUN = sd)

#Need to get standard deviations as well
tapply(pottery$Al, pottery$Site, FUN = "sd")
tapply(pottery$Fe, pottery$Site, FUN = "sd")
tapply(pottery$Mg, pottery$Site, FUN = "sd")
tapply(pottery$Ca, pottery$Site, FUN = "sd")
tapply(pottery$Na, pottery$Site, FUN = "sd")

#Build boxplots for each chemical, subset by site
#First, set up the plotting area to include enough space for all of the plots and a title
par(mfrow = c(2,3), oma=c(0,0,2,0))

boxplot(pottery$Al~pottery$Site, main = "Aluminum", xlab = "Site", ylab = "Percentage")
boxplot(pottery$Fe~pottery$Site, main = "Iron", xlab = "Site", ylab = "Percentage")
boxplot(pottery$Mg~pottery$Site, main = "Magnesium", xlab = "Site", ylab = "Percentage")
boxplot(pottery$Ca~pottery$Site, main = "Calcium", xlab = "Site", ylab = "Percentage")
boxplot(pottery$Na~pottery$Site, main = "Sodium", xlab = "Site", ylab = "Percentage")

title("Percentage of Oxides of Various Chemicals by Site", outer = TRUE)

