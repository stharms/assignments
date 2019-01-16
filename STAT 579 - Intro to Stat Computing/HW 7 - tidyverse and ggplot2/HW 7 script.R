setwd("~/Documents/Microsoft Word/Grad School/STAT 579/Homeworks/HW 7")
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("purrr")
library(purrr)
library(magrittr)
install.packages('tidyverse')
library(tidyverse)
###########################################################
#Exercise 1
##a
congressd <- data.frame(read.csv(file = "https://raw.githubusercontent.com/fivethirtyeight/data/master/congress-age/congress-terms.csv",
                       header = T, sep = ","))
dim(congressd)

##b
congressd %>% summarize(mean(age))

##c
bychamber <- congressd %>% group_by(congress, chamber) %>% summarize(avg_age = mean(age)) %>%
  arrange(chamber) %>% select(chamber, congress, avg_age)

head(bychamber)

##d
dp <- ggplot(data = bychamber, aes(y= avg_age, x = congress, shape = chamber, linetype = chamber))
dpp <- dp + geom_point() + geom_line()
dpp

##e
partyaff <- congressd %>% group_by(congress, chamber, party) %>% filter(party == "R" | party == "D") %>%
  summarize(avg_age = mean(age), n = length(party))%>% arrange(chamber, congress, party) %>%
  select(chamber, congress, party, avg_age, n)

head(partyaff)

##f
fp <- ggplot(data = partyaff, aes(y= avg_age, x = congress, shape = chamber, linetype = chamber, col = party))
fpp <- fp + geom_line() + geom_point()
fpp

##g
byinc <- congressd %>% group_by(congress, chamber, party, incumbent) %>% filter(party == "R" | party == "D") %>%
  summarize(avg_age = mean(age), n = length(party))%>% arrange(chamber, congress, party, incumbent) %>%
  select(chamber, congress, party, incumbent, avg_age, n) %>% filter(chamber == "house")
head(byinc)

gp <- ggplot(data = byinc, aes(y= avg_age, x = congress, shape = incumbent, linetype = incumbent, col = party))
gpp <- gp + geom_line() + geom_point()
gpp

#Exercise 2
##a
iris.split <- split(iris, iris$Species)
head(iris.split)
dim(iris.split$setosa)

##b
mod <- iris.split %>% map(~ lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = .x))
#check one of the resulting list elements
mod$versicolor

##c
models <- mod %>% map(summary)
#check one of the resulting list elements
models$virginica

##d
coeff <- models %>% map(coef)
coeff

##e
newdf <- coeff %>% map_df( ~as.data.frame(.x), .id = 'Species')
newdf
