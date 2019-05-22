


#  Code for creating probability plots in R
#  It is stored in the file 
#
#                     microwave.R  
#
#  Any line preceded with a pound sign is a 
#  comment that is ignored by the program.  
#  Data are read into a data frame from the file
#
#                     microwave.closed.dat
 
mdat <- read.table(file = "microwave.closed.dat",header=F)
 
#  Assign labels to the variables (columns 
#  of the data frame)

names(mdat) <- c('Oven','Radiation')

head(mdat)
 
#  Compute the sample mean and the
#  sample cvariance matrix and print 
#  the results

mmean<-mean(mdat$Radiation)
mmean
mvar<-var(mdat$Radiation)
mvar


# Create a normal probability plot. Note that
# this function does not avearge normal quantiles
# for tied data values


qqnorm(mdat$Radiation, pch = 20, main="Normal Probability Plot") 

#  Compute the Shapiro-Wilks' test statistic

shapiro.test(mdat$Radiation)


# Compute natural logarithm of radiation values
	
mdat$logRad <- log(mdat$Radiation)	
  
# Compute Q-Q plot and value of Shapiro-Wilk test
# for the natural logaritm of the radiation values
		
qqnorm(mdat$logRad, pch = 5, main="Normal Probability Plot") 

shapiro.test(mdat$logRad)



# dataset with both open and close door radiation

mdat1 <- read.table(file = "microwave.dat",header=F)

#  Assign labels to the variables (columns 
#  of the data frame)

names(mdat1) <- c('Oven','Radiation.close', 'Radiation.open')
# orignal means and variance

colMeans(mdat1[, -1])
var(mdat1[, -1])

# Box-Cox transformation

mdat.t = mdat1[, -1]^(0.25)
colMeans(mdat.t)
var(mdat.t)








