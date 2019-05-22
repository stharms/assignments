library(tidyverse)
library(rvest)

get_nba_standings<-function(year){
urlt <- paste0('https://www.basketball-reference.com/leagues/NBA_',year,'_standings.html')
nbat <- read_html(urlt) %>% html_table() %>% lapply(data.frame)
colnames(nbat[[2]]) = colnames(nbat[[1]])
nbateams <- data.frame(rbind(nbat[[1]],nbat[[2]])) %>% as_tibble()
colnames(nbateams) <- c('Team', colnames(nbat[[1]])[-1])
nbateams <- nbateams %>% 
  select(-GB) %>% type_convert %>% 
  mutate(Team = str_sub(as.character(Team),end=str_locate(as.character(Team),'\\*?\\s*\\([:digit:]+\\)')[,1]-1)) %>%
  mutate(Team=as.factor(Team)) %>%
  mutate(year=as.factor(year)) %>% select(1,8,2:7)
return(nbateams)
}
#####################
#####################
get_player_stats <- function(year){
#basic stats
urlb <- paste0('https://www.basketball-reference.com/leagues/NBA_',year,'_totals.html')
nbaplayers <- read_html(urlb) %>% html_table() %>% data.frame() %>%
  filter(PF != 'PF', Tm!='TOT') %>% as_tibble() %>% type_convert()

#advanced statistics
urla <- paste0('https://www.basketball-reference.com/leagues/NBA_',year,'_advanced.html')
nbaadv <- read_html(urla) %>% html_table() %>% data.frame() %>%
  filter(Pos != 'Pos', Tm!='TOT')  %>% as_tibble() %>% type_convert()

#variables to use for analysis
playerdata <- nbaplayers %>% select(-c(1,9:11,12,15,18,19,24,30)) %>% full_join(nbaadv %>% select(2:5,8,19,24,29)) %>% 
  mutate_at(c(1,2,4), as_factor) %>% mutate(year = as.factor(year)) %>% select(1:4,25,5:24)
return(playerdata)
}

######################################

years<- c(2010:2019)

teamdata <- suppressWarnings(suppressMessages(
  years %>% lapply(get_nba_standings) %>% lapply(filter, !is.na(Team))%>% lapply(type_convert) %>%
  bind_rows %>% as_tibble() %>% type_convert %>% mutate(year=as.factor(year))
))
#team abbreviations
nodes <- read_html('https://www.basketball-reference.com/leagues/NBA_2019_standings.html') %>% html_nodes('body') %>% html_children() %>% html_nodes('a') %>%
  html_attr('href')
acros <- nodes %>% str_sub(start=str_locate(nodes,'\\/teams\\/')[,2]+1,end=str_locate(nodes,paste0('\\/',2019))[,1]-1) %>% unique
nbateams <- get_nba_standings(2019) %>% mutate(abbr = as.factor(acros[-1]))  %>% type_convert %>% select(Team,abbr)
nbateams

nbateamdata <- teamdata %>% left_join(nbateams, by=c('Team'='Team')) %>%
  mutate(abbr=as.character(abbr)) %>% select(1,9,2:8)
nbateamdata$abbr[which(nbateamdata$Team=='New Jersey Nets')] <- 'NJN'
nbateamdata


playerdata <- suppressWarnings(suppressMessages( 
  years %>% lapply(get_player_stats) %>% lapply(type_convert) %>%
    bind_rows %>% as_tibble() %>% type_convert %>% mutate(year=as.factor(year)) %>% 
    mutate(FT.=100*FT., X2P.=100*X2P., X3P.=100*X3P.)
  ))
player_perminute <- playerdata %>% mutate_at(c(9,11,13,15:21), funs(./playerdata$MP)) %>% filter(complete.cases(.))
playerdata <- playerdata %>% filter(complete.cases(.))
#write the data to csv
write_csv(nbateamdata, "nbateamdata.csv")
write_csv(playerdata, "nbaplayerdata.csv")
##########
##########
#visualization
library(fmsb)
pos <-playerdata %>% group_by(Pos) %>%
  summarise_at(c('ORB','DRB','BLK','STL','X2PA','X3PA','FTA'),mean)
bypos <- pos
bypos[,-1]<- sweep(bypos[,-1], 2, apply(pos[,-1],2,mean)) %>% sweep(2,apply(pos[,-1],2,sd), FUN = "/")
bypos[,-1] %>% radarchart(plty=1,plwd=2, pcol=c(2,4,6,7,8))
stars(sample_n(playerdata[,c('ORB','DRB','AST','BLK','STL','TOV','X2PA','X3PA','FTA')],16), add=F)



#raw data
ggplot(data=(playerdata %>% gather(key=stat, value=value, -c(1:8)))) +
  geom_histogram(aes(x=value)) +
  facet_wrap(.~stat, scales="free")

#per minute data looks more normal
ggplot(data=(player_perminute %>% gather(key=stat, value=value, -c(1:8)))) +
  geom_histogram(aes(x=value)) +
  facet_wrap(.~stat, scales="free")

#pairwise plots
library(GGally)
ggpairs(playerdata[,9:25])
ggpairs(player_perminute[,9:25])

#covariance matrices
library(reshape2)
covp <- playerdata %>% select(9:25) %>% var(na.rm=TRUE)
corp <- playerdata %>% select(9:25) %>% cor(use="complete.obs")
covpm <- player_perminute %>% select(9:25) %>% var(na.rm=TRUE)
corpm <- player_perminute %>% select(9:25) %>% cor(use="complete.obs")
pm_shotsc <- player_perminute %>% select(9,11,13,15:25) %>% cor(use="complete.obs")
shotsc <- playerdata %>% select(9,11,13,15:25) %>% cor(use="complete.obs")


ggplot(data = covp %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() #+ scale_fill_gradient2(limits=c(mn, mx), midpoint=mid)
ggplot(data = corp %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + ggtitle("Correlation Matrix - Unadjusted Player Statistics 2010-2019") #+ scale_fill_gradient2(limits=c(mn, mx), midpoint=mid)
ggplot(data = corpm %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + ggtitle("Correlation Matrix - Adjusted Player Statistics per minute 2010-2019") #+ scale_fill_gradient2(limits=c(mn, mx), midpoint=mid)
ggplot(data = pm_shotsc %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() #+ scale_fill_gradient2(limits=c(mn, mx), midpoint=mid)
ggplot(data = shotsc %>% melt, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#QQplots
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

QQs <- apply(playerdata[,-c(1:8)] %>% filter(complete.cases(.)),2,FUN=QQplot.normal) %>% as.data.frame()
QQs <- data.frame(sample.quantile = gather(QQs[,seq(from=1, to=33, by=2)])[,2], 
                  normal.quantile=gather(QQs[,seq(from=2, to=34, by=2)])[,2],
                  var= rep(names(player_perminute)[9:25], each=nrow(QQs)))
ggplot(data = QQs) + geom_point(aes(x=normal.quantile, y=sample.quantile)) + facet_wrap(.~var, scales='free')

QQs <- apply(player_perminute[,-c(1:7)] %>% filter(complete.cases(.)),2,FUN=QQplot.normal) %>% as.data.frame()
QQs <- data.frame(sample.quantile = gather(QQs[,seq(from=1, to=33, by=2)])[,2], 
                  normal.quantile=gather(QQs[,seq(from=2, to=34, by=2)])[,2],
                  var= rep(names(player_perminute)[9:25], each=nrow(QQs)))
ggplot(data = QQs) + geom_point(aes(x=normal.quantile, y=sample.quantile)) + facet_wrap(.~var, scales='free')

#Chi.square plot
chisqplot <- function(x){
  # x is an observed vector of a variable
  #get sample statistics
  xbar <- apply(x,2,mean); s <- cov(x)
  #get squared distances from x
  dis <- mahalanobis(x, center=xbar, cov=s)
  #sort
  di.sort = sort(dis)
  #get chi-squared quantiles
  n = nrow(x)
  p = ( c(1 : n) - 0.5 ) / n
  q = qchisq(p,df=ncol(x))
  #combine and output
  res = data.frame(cbind(di.sort, q))
  names(res) = c("sample.quantile", "chisq.quantile")
  return(res)
}

ggplot(data=chisqplot(playerdata[,-c(1:8)] %>% filter(complete.cases(.)))) +
  geom_point(aes(x=chisq.quantile,y=sample.quantile))
ggplot(data=chisqplot(player_perminute[,-c(1:8)] %>% filter(complete.cases(.)))) +
  geom_point(aes(x=chisq.quantile,y=sample.quantile))

#Plotting variable means by position
summed <- playerdata %>% select(-Player,-Tm,-Age,-G,-GS, -MP,-FT.,-X2P.,-X3P.) %>% melt(id=c('Pos','year')) %>% group_by(Pos,year,variable) %>% summarise(grmean=mean(value))
ggplot(data=(summed)) + geom_line(aes(x=as.factor(variable), group=Pos, y=grmean, color=Pos), size=1.3)+ facet_wrap(.~year)


###################################################
##################################################3
##################################################
#MANOVA
library(car)
#fit linear model with Position as the factor
fitlm <- lm(as.matrix(playerdata[,9:25])~playerdata$Pos)
manova.pos <- Manova(fitlm)
summary(manova.pos)

fitlmtwo <- lm(as.matrix(playerdata[,9:25])~playerdata$Pos * playerdata$Tm)
manova.team <- Manova(fitlmtwo)
summary(manova.team)

fitlmthree <- lm(as.matrix(playerdata[,9:25])~playerdata$Pos*as.factor(playerdata$year))
manova.year <- Manova(fitlmthree)
summary(manova.year)
#####################################################
###################################################
#Multiple Regression
mlrfit.y <- lm(as.matrix(playerdata[,9:25])~playerdata$MP+ playerdata$Age + playerdata$Pos)
summary(mlrfit.y)

mlrfit.r <- lm(as.matrix(playerdata[,9:25])~playerdata$MP+ playerdata$Age + as.factor(playerdata$year) +playerdata$Pos + playerdata$Tm)
summary(mlrfit.r)

mlrfit <- lm(as.matrix(playerdata[,9:25])~playerdata$MP+ playerdata$Age + as.factor(playerdata$year) +
               playerdata$Pos + playerdata$Tm + playerdata$Pos * playerdata$Tm)

anova(mlrfit,mlrfit.r, mlrfit.y)
summary(mlrfit)

ggplot(data=playerdata %>% filter(complete.cases(.)),aes(x=year, y=mlrfit.r$fitted.values[,2])) + geom_line(aes(col=Tm))+
  geom_point(aes(col=Tm)) + facet_wrap(.~Pos, scales='fixed') + 
  ggtitle("3PA by year for each position")+ labs(y="3 point Attempts", x="Year")

ggplot(data=playerdata %>% filter(complete.cases(.)),aes(x=Age, y=mlrfit.r$fitted.values[,14])) + geom_line(aes(col=Tm))+
  geom_point(aes(col=Tm)) + facet_wrap(.~Pos, scales='fixed', nrow=1) + 
  ggtitle("PER rating by Age fitted values")+ labs(y="PER rating", x="Age")

ggplot(data=playerdata %>% filter(complete.cases(.)),aes(x=Age, y=mlrfit.r$fitted.values[,6])) + geom_line(aes(col=Tm))+
  geom_point(aes(col=Tm)) + facet_wrap(.~Pos, scales='fixed') + 
  ggtitle("FT% by Age")+ labs(y="FT%", x="Age")
#####################################################
###################################################
#PCA/Factor Analysis
#center the response variables
playerdata.av <- playerdata[,9:25] %>% filter(complete.cases(.)) %>% summarise_all(mean)
playerdata.centered <- playerdata[,9:25] %>% filter(complete.cases(.)) %>% 
        sweep(MARGIN=2,
        apply(playerdata[,9:25] %>% filter(complete.cases(.)),2,mean)
        )
head(playerdata.centered)

#principal components
player.pc <- prcomp(playerdata.centered)
player.pc
prcomp(playerdata[,9:25], center=FALSE)
playerpca <- princomp(playerdata[,9:25])
#how many do we need?
csums <- cumsum(player.pc$sdev^2) / sum(player.pc$sdev^2)
csums

summary(playerpca)
csums <- cumsum(playerpca$sdev^2) / sum(playerpca$sdev^2)
plot(y=csums,x=seq(1:17), type='l')
PCs.proportion.variation.enuff(player.pc$sdev^2, q = 5, propn = 0.99,
                               nobs = length(playerdata$X3PA))

last.PCs.equal.variances(player.pc$sdev^2, r=12, df = 17)

#what do the first 5 PCs represent?
addback <- playerdata.av %>% matrix(ncol=length(playerdata.av), nrow=nrow(playerdata.centered), byrow=T) %>%
  unlist() %>% matrix(ncol=length(playerdata.av), nrow=nrow(playerdata.centered), byrow=F)

plpc.rot <- data.frame(Pos=(playerdata %>% filter(complete.cases(.)))$Pos,
                       ((player.pc$x %>% as.matrix) + addback) %*% player.pc$rotation[,1:5])
plpc.rot %>% group_by(Pos) %>% summarise_all(median)

plpc.rot <- data.frame(Pos=(playerdata %>% filter(complete.cases(.)))$Pos,
                       ((player.pc$x %>% as.matrix) + addback) %*% player.pc$rotation[,1:5])
plpc.rot %>% group_by(Pos) %>% summarise_all(mean)

plpca <- predict(playerpca) %>% data.frame(Pos=playerdata$Pos)
data.frame(Pos=playerdata$Pos, plpca) %>% group_by(Pos) %>% summarise_all(median)
#looking only at the first two PCs
#Transform data by PCs

pl1 <- ((player.pc$x %>% as.matrix)) %*% player.pc$rotation[,3]
pl2 <- ((player.pc$x %>% as.matrix)) %*% player.pc$rotation[,4]
pl.tr <- data.frame(Pos=(playerdata %>% filter(complete.cases(.)))$Pos, year=(playerdata %>% filter(complete.cases(.)))$year,
                    pc1=pl1, pc2=pl2)
ggplot(pl.tr) + geom_point(aes(x=pc1,y=pc2, col=Pos))

pl.tr <- pl.tr %>% filter(Pos%in% c("PG","C"))
ggplot(pl.tr) + geom_point(aes(x=pc1,y=pc2, col=Pos))

ggplot(plpca) + geom_point(aes(x=Comp.3, y=Comp.4, col=Pos))

#####################################################
###################################################
#Factor Analysis
playerfact <- factanal(x = playerdata[,9:25], factors = 3, scores = "regression", rotation = "varimax",     method = "mle")
playerfact
data.frame(Pos=playerdata$Pos,f1=playerfact$scores[,3], f2=playerfact$scores[,1]) %>% 
  ggplot() + geom_point(aes(x=f1,y=f2,col=Pos))

#####################################################
###################################################
#Classification
#split the data into 10 groups
#shuffle
library(MASS)
playershuf <- playerdata[sample(nrow(playerdata)),]
folds <- cut(seq(1,nrow(playershuf)),breaks=10,labels=F)
playershuf[which(folds==3),]

i=0
repeat{
  i=i+1
  #split data
  pdtrain <- playershuf[-which(folds==i),]
  pdtest <- playershuf[which(folds==i),]
  #lda and qda trained
  ld <- lda(pdtrain$Pos~as.matrix(pdtrain[,9:25]), method="mle")
  qd <- qda(pdtrain$Pos~as.matrix(pdtrain[,9:25]), method='mle')
  #test on held out data
  
  
}

#cv for lda and qda
linda <- lda(Pos~as.matrix(playerdata[,9:25]), data=playerdata,method='mle', CV=TRUE)
quada <- qda(Pos~as.matrix(playerdata[,9:25]), data=playerdata, method='mle', CV=TRUE)

#How do the two methods compare in predictions?
#lda does very slightly better
#main issue seems to be misclassifying SFs as SGs or PFs and PFs/Cs getting mixed up
table(playerdata$Pos, linda$class)
table(playerdata$Pos, quada$class)
mean(playerdata$Pos != linda$class)
mean(playerdata$Pos != quada$class)

#Do the two methods predict approximately the same classification for each observation?
#28% difference (seems like a lot)
table(linda$class, quada$class)
mean(linda$class != quada$class)

