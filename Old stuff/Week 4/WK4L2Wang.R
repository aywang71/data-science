# Andrew Wang
# September 22
# Data Analyses and Study of Melting Point Data*
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
chemData <- read.csv("dirtyMPdata.csv")

#basic high-level structure
View(chemData)
str(chemData)
head(chemData)
tail(chemData)
summary(chemData)

#cleaning data only one column with any missing data
print(sum(is.na(chemData)))
print(sum(is.na(chemData$energy)))
chemData$energy <- ifelse(is.na(chemData$energy),mean(chemData$energy,na.rm = TRUE),chemData$energy)
print(sum(is.na(chemData)))

#rename a few columns
chemData <- rename(chemData, structure = ï..structure, melting.point = mp)

#linear regressions
FCMP <- lm(chemData$melting.point~chemData$formal.charge)
FCMP
plot(chemData$melting.point~chemData$formal.charge, main = "Plot of melting point against formal charge", xlab = "Formal charge", ylab = "Melting point (Celsius)")
abline(FCMP)

VMP <- lm(chemData$melting.point~chemData$volume)
VMP
plot(chemData$melting.point~chemData$volume, main = "Plot of melting point against volume", xlab = "Volume (Liters)", ylab = "Melting point (Celsius)")
abline(VMP)

MRMP <- lm(chemData$melting.point~chemData$refractivity)
MRMP
plot(chemData$melting.point~chemData$refractivity, main = "Plot of melting point against refractivity", xlab = "Refractivity (cubic centimeters)", ylab = "Melting point (Celsius)")
abline(VMP)

# multi-linear regression
multiMP <- lm(chemData$melting.point~chemData$formal.charge+chemData$volume+chemData$refractivity)
multiMP
par(mfrow = c(2,2)) #setting up a graphics grid
plot(chemData$melting.point~(chemData$formal.charge+chemData$volume+chemData$refractivity))
par(mfrow = c(1,1)) #reset graphics grid

#BIC analysis volume -> melting point
baseMP <- BIC(lm(chemData$melting.point~1))
baseMP
volumeMP <- BIC(lm(chemData$melting.point~chemData$volume))
volumeMP
print(abs(volumeMP-baseMP))
