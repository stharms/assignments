setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 4")

############################################3
###Exercise 1#############################
#############
#a#
#Read in data
votes <- read.table(file = "http://maitra.public.iastate.edu/stat579/datasets/senate-109.txt", sep = "\t", header = TRUE)
names(votes$bill_type_bill_name_bill_ID)= "type.name.ID"
names(votes$missing_votes)= "missing"
votes$bill_type_bill_name_bill_ID <- as.character(votes$bill_type_bill_name_bill_ID)

#b#
billtype <- cbind(sub("_.*$", "", votes$bill_type_bill_name_bill_ID))
colnames(billtype) = "billtype"
#dim(billtype)
#nrow(billtype)
#class(billtype)
#mode(billtype)
#View(billtype)
table(billtype)

#c#
votesonly <- as.matrix(votes[,-(1:2)])
votecount1 <- votesonly%*%t(votesonly)
votecount <- diag(votecount1)
matches <- votecount + votes$missing_votes == 100
sum(matches)

#d#
relative1 <- subset(cbind(billtype,votes), votes$William.H..Bill.Frist..TN. != 0)
relative <- relative1[,-(1:3)]
relative[relative == relative$William.H..Bill.Frist..TN.] <- "With"
relative[relative != relative$William.H..Bill.Frist..TN. & relative != 0] <- "Against"
relative[relative == 0] <- "Indifferent"

#e#
library(reshape2)
party <- cbind(relative1[,1], relative)
names(party)[1] <- "billtype"

party2 <- melt(party, id.vars = "billtype", variable_name = "Senator", value.name = "ForAgainst")

table1 <- table(party2$billtype, party2$ForAgainst)
percentages <- prop.table(table1,1)
percentages


#######################################################################################################
####Exercise 2##########################################
require(grDevices); require(graphics); require(RColorBrewer)
#a#
pet <- matrix(scan(file = "http://maitra.public.iastate.edu/stat579/datasets/fbp-img.dat"), nrow = 128, ncol = 128, byrow = TRUE)
brain <- pet[,128:1]

#b#
image(z=brain, col = rev(brewer.pal(9, "YlGnBu")), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=brain, col = rev(terrain.colors(45)), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=brain, col = topo.colors(40), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
#C#
#i#
range(brain)
binwidth <- (max(brain) - min(brain))/8
midpts <- seq(from= min(brain)+ binwidth/2, by = binwidth, length = 8)
binned1 <- apply(brain, 2, cut, seq(from=min(brain), to=max(brain), length = 9), labels = midpts)
binned2 <- apply(binned1, 2, as.numeric)
table(binned2)
image(z=binned2, col = rev(terrain.colors(45)), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=binned2, col = rev(brewer.pal(9, "YlGnBu")), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=binned2, col = topo.colors(40), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)

#ii#
quants <- quantile(brain, probs = (seq(from = 0, to = 1, length = 9)))
qmids <- quants[1:8] + diff(quants)/2
binnedq <- apply(brain, 2, cut, seq(from=min(brain), to=max(brain), length = 9), labels = qmids)
binnedq2 <- apply(binnedq, 2, as.numeric)
table(binnedq2)
image(z=binnedq2, col = rev(gray(seq(0,1, length = 8))), main = "PET Scan of FDG-18 Intake", axes = F)
image(z=binnedq2, col = rev(terrain.colors(45)), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=binnedq2, col = rev(brewer.pal(9, "YlGnBu")), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=binnedq2, col = topo.colors(40), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)



bin <- function(b, n){
  b.qt <- quantile(b, probs = seq(0, 1, length = n))
  b.qt.mid <- (b.qt[-1] + b.qt[-length(b.qt)])/2
  #stack matrix 5x on top of each other
  b.arr <- array(b, dim = c(dim(b), length(b.qt.mid)))
  mid.arr <- array(rep(b.qt.mid,
                       each = prod(dim(b))), dim = dim(b.arr))
  sq.diff.arr <- (b.arr - mid.arr)^2
  
  min.idb1 <- apply(X = sq.diff.arr, MAR = c(1,2), FUN = which.min)
  min.idb2 <- apply(X = sq.diff.arr, MAR = c(1,2), FUN = order, decreasing = F)[1,,]
  b.quant <- array(b.qt.mid[min.idb1], dim = dim(b))
  return(b.quant)
}
brain.quant <- bin(brain, 9)
image(z=brain.quant, col = rev(gray(seq(0,1, length = 8))), main = "PET Scan of FDG-18 Intake", axes = F)
image(z=brain.quant, col = rev(terrain.colors(45)), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=brain.quant, col = rev(brewer.pal(9, "YlGnBu")), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)
image(z=brain.quant, col = topo.colors(40), main = "PET Scan of FDG-18 Intake", axes = F, asp = 1)



###############################################
##Exercise 3#################################
###########
#a#
auto <- read.table(file= "auto.txt", header=T, sep="")
auto

#b#
x <- cbind(rep(1, times = length(auto$horsepower)), auto$horsepower)
y <- auto$mpg
b0b1 <- solve(t(x)%*%x)%*%(t(x))%*%y
b0b1

#c#
model <- lm(auto$mpg~auto$horsepower)
model
