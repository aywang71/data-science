# Andrew Wang
# September 20
# Analyzing Mouse Data script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 4") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(dplyr)

#input csv file
mouseData <- read.csv("mouseData.csv")

#basic high-level structure
View(mouseData)
str(mouseData)
head(mouseData)
tail(mouseData)

#fix missing data
mouseData$Heart.Wt <- ifelse(is.na(mouseData$Heart.Wt),round(mean(mouseData$Heart.Wt,na.rm = TRUE),3),mouseData$Heart.Wt)
mouseData$Mean.HR <- ifelse(is.na(mouseData$Mean.HR),round(mean(mouseData$Mean.HR,na.rm = TRUE),1),mouseData$Mean.HR)
mouseData$Mean.BP <- ifelse(is.na(mouseData$Mean.BP),round(mean(mouseData$Mean.BP,na.rm = TRUE),1),mouseData$Mean.BP)

#debug to make sure no more empty values
print(sum(is.na(mouseData)))

#rename sex to gender and Heart.Wt to Heart.Kg
mouseData <- rename(mouseData, gender = sex, Heart.Kg = Heart.Wt)

#factor gender to words
mouseData$gender <- factor(mouseData$gender, levels = c(0,1), labels = c("Female","Male"))

#remove the mouse column - not useful for analysis
mouseData$mouse <- NULL

#histograms
par(mfrow = c(2,2)) # set up graphics grid
hist(mouseData$Mean.BP, main = "Histogram of blood pressure")
hist(mouseData$Mean.HR, main = "Histogram of heart rate")
hist(mouseData$Heart.Kg, main = "Histogram of heart kg")
rzHeart.Kg <- rz.transform(mouseData$Heart.Kg) #change to rank-z transformation to make the curve normal
hist(rzHeart.Kg, main = "Histogram of Rank-Z heart kg")
par(mfrow = c(1,1)) # reset graphics grid

#comparative plots
#blood pressure by heart rate
BPHR <- lm(mouseData$Mean.BP~mouseData$Mean.HR) # creates regression subset of Blood Pressure by Heart Rate
plot(mouseData$Mean.BP~mouseData$Mean.HR, main = "Plot of Mean BP vs. Mean HR", xlab = "Mean heart rate", ylab = "Mean blood pressure") #plots the blood pressure by heart rate
abline(BPHR) #creates a line of best fit off of the regression subset
summary(BPHR) #outputs stats of subset

#blood pressure by heart weight
BPHW <- lm(mouseData$Mean.BP~mouseData$Heart.Kg)
plot(mouseData$Mean.BP~mouseData$Heart.Kg, main = "Plot of Mean BP vs. Heart weight", xlab = "Heart weight (kg)", ylab = "Mean blood pressure")
abline(BPHW)
summary(BPHW)

#heart rate by heart weight 
HRHW <- lm(mouseData$Mean.HR~mouseData$Heart.Kg)
plot(mouseData$Mean.HR~mouseData$Heart.Kg, main = "Plot of Mean HR vs. Heart weight", xlab = "Heart weight (kg)", ylab = "Mean heart rate")
abline(HRHW)
summary(HRHW)

#BIC analysis

#want to see if heart weight has to do with heart rate
print(abs(BIC(lm(mouseData$Mean.HR~1))-BIC(lm(mouseData$Mean.HR~mouseData$Heart.Kg)))) #conclusion: decent relationship between the two
#explanation: we take the absolute value of the difference between the two BIC values

#want to see if blood pressure has to do with heart rate
print(abs(BIC(lm(mouseData$Mean.HR~1))-BIC(lm(mouseData$Mean.HR~mouseData$Mean.BP)))) #conclusion: good relationship between the two