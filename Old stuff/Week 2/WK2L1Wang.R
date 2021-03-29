# Andrew Wang
# September 4
# titanic script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 2") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project

#input csv file
titanic <- read.csv("titanic.csv",T)
# display summary data for titanic
View(titanic)
names(titanic)
dim(titanic)
class(titanic)
str(titanic)
sapply(titanic,class)
summary(titanic)
# delete passengerId
titanic$PassengerId <- NULL
# update column values for survived to yes/no from 0,1
titanic$Survived <- factor(titanic$Survived, c(0,1), labels = c("No","Yes"))
#update class for upper/middle/lower from 1,2,3
titanic$Pclass <- factor(titanic$Pclass, c(1,2,3), labels = c("Upper","Middle","Lower"))
#update embarking point to Cherbourg/Queenstown/Southampton from C,Q,S
titanic$Embarked <- factor(titanic$Embarked, c("C","Q","S"), labels = c("Cherbourg","Queenstown","Southampton"))
#replace empty ages with average values
titanic$Age <- ifelse(is.na(titanic$Age),mean(titanic$Age,na.rm=TRUE),titanic$Age)
# updates the column names 
names(titanic) <- c("Survived","Pclass","Name","Sex","Age","Sibllings","ParentsChildren","Ticket","Fare","Cabin","Embarked")
#create numerics subset with number data
numerics <- titanic[,c(5:7,9)]
# makes plot comparing numerics data
pairs(numerics, upper.panel = panel.cor, diag.panel = panel.hist)