# Andrew Wang
# October 4
# Opiates script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 6") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(tidyverse)
library(gridExtra)

#input csv file
opiates <-read.csv("opiates.csv")
view(opiates)
summary(opiates)

#clean missing data out
opiates$crude <- ifelse(is.na(opiates$crude),mean(opiates$crude,na.rm=TRUE),opiates$crude)
opiates$adjusted <- ifelse(is.na(opiates$adjusted),mean(opiates$adjusted,na.rm=TRUE),opiates$adjusted)
#debug point to make sure all missing data is complete
print(sum(is.na(opiates$crude)))

#fix units on population
opiates$population <- opiates$population/1000000


#Boxplot of deaths vs. population
g2 <- ggplot(data = opiates, mapping = aes(x = population, y = deaths, group = division_name, xlim(12.5,25))) 
g2 <- g2 + geom_boxplot(varwidth = FALSE)
g2 <- g2 + labs(x = "Population (millions)", y = "Deaths", title = "Boxplot of deaths vs. population")
g2

#Scatterlot of population by deaths and region
g1 <- ggplot(data = opiates, mapping = aes(x = population, y = deaths, color = region))
g1 <- g1 + labs(x = "Population (millions)", y = "Deaths", title = "Scatterplot of population and deaths by region")
g1 <- g1 + geom_point()

#Violin Plot of deaths vs. population
g3 <- ggplot(opiates, aes(x = population, y = deaths, group = abbr))
g3 <- g3 + geom_violin()
g3 <- g3 + labs(x = "Population (millions)", y = "Deaths", title = "Violin plot of deaths vs. population")
g3

#Stripchart of deaths vs. population
g4 <- ggplot(opiates, aes(x = adjusted, y = adjusted_se, shape = region, color = fips))
g4 <- g4 + labs(x = "Population (millions)", y = "Deaths", title = "Stripchart of deaths vs. population")
g4 <- g4 + geom_jitter()
g4

#send to PDF
pdf("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 6/WK6L1.pdf")
#arranges the graphs
grid.arrange(g2,g1,g3,g4,ncol=2,nrow=2)
graphics.off()
