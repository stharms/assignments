library(tidyverse)
library(GGally)
library(RColorBrewer)
library(classdata)
library(andrews)
sig = 4*matrix(1,nrow=5,ncol=5)
sig
eigen(sig)$values

###########################
#####################################
#7
#scatterplot
g <- ggplot(data=iris)
g+ geom_point(aes(x=Sepal.Length, y=Sepal.Width, colour=Species)) +
  scale_colour_manual(values = brewer.pal(name="Dark2", n=3)) + theme_bw()
#scatterplot matrix
ggpairs(iris, mapping=aes(colour=Species) ,columns=1:4, axisLabels="show")
#means for each species
byspecies <- iris %>% group_by(Species) %>% summarise_all(funs(avg = mean)) 
byspecies
#stars
stars(t(byspecies[,-1]), labels=rownames(t(byspecies[,-1])), draw.segments=F, axes=F, radius=T,lty=1, lwd=1.5,
      key.labels=byspecies$Species, xlim=c(1,9), ylim=c(1,5),key.loc = c(8,4))

#8
gg <- ggplot(data=(fbiwide%>%filter(State %in% c("California","Colorado","Iowa","Illinois","New York", "District of Columbia"))))
gg + geom_point(mapping=aes(y=Burglary, x=log(Motor.vehicle.theft), colour=as.factor(Year)))+ facet_wrap(~State, scales = "free") +
   scale_color_discrete()
#now scaled by population
fbi2 <- fbiwide %>% mutate(popht = Population/100000) %>% mutate(logmottheft = log(Motor.vehicle.theft)/popht, burgrate = Burglary/popht)
summary(fbi2)
ggg <- ggplot(data=(fbi2%>%filter(State %in% c("California","Colorado","Iowa","Illinois","New York", "District of Columbia"))))
ggg + geom_point(mapping=aes(y=burgrate, x=logmottheft, colour=as.factor(Year)))+ facet_wrap(~State, scales = "free") +
  scale_color_discrete() + labs(x="log Motor Vehicle Theft rate (per 100,000 people)", y="Burglary Rate (per 100,000 people)")

#9
senate <- read_csv("senate-109.csv")
party <- read_csv("senate-109-party.csv", col_names=F)
names(party) <- "party"
#extract last names of senators
senators <- names(senate)[-c(1:2)]
splitup <- senators %>% str_split(pattern="\\s")
lastnames <- c()
i=0
repeat{
  i <- i+1
  j <- length(splitup[[i]])
  if(j<=5){lastnames[i] <- splitup[[i]][j-1]}
  if(j>5 || splitup[[i]][j-1] == "Jr."){lastnames[i] <- paste(splitup[[i]][j-2], splitup[[i]][j-1])}
  if(i==length(splitup)){break}
}


#combine data sets
senatet<- t(read.csv("senate-109.csv", header=T,row.names=1))
senatett <- data.frame(last.name=c("Missing",lastnames),senatet) %>% arrange(last.name)
senateparty <- party %>%  mutate(lastname=str_sub(party, end=-7L), state = str_sub(party, start=-3L, end=-2L),pty=as.factor(str_sub(party,start=-5L,end=-5L)))%>%
  select(lastname,state,pty) %>% rbind(c("Missing", "Missing","Missing")) %>% arrange(lastname) %>% data.frame(senatett) %>% select(-last.name)

View(senateparty)

andrews(df=(senateparty%>%filter(lastname !="Missing") %>% arrange(pty)), clr=3, type=4, ymax=3, step=30)

byparty <- senateparty[,-c(1:2)] %>% group_by(pty) %>% summarise_all(funs(m=sum)) #%>% View()

