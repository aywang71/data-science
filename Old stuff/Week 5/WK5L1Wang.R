# Andrew Wang
# September 27
# Cervical Cancer Part 1 script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 5") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(tidyverse)

#input csv file
cancerData <- read.csv("cervicalCA.csv", na.strings = c("?"))
str(cancerData)
summary(cancerData) #to look at the empty values per column
View(cancerData)

cancerData[is.na(cancerData)] = 0

print(sum(is.na(cancerData))) #make sure there are no empty values anymore 

#replace column names 
names(cancerData) <- str_replace_all(names(cancerData), "\\.+", "-")
#turn to lower case
names(cancerData) <- str_to_lower(names(cancerData))
# removes the last "-" for certain column names
names(cancerData) <- str_replace_all(names(cancerData), "-$", "")

#sets up bool variable to hold columns which need to be switched to logical types 
bools <- c(5,8,10,12,14:25,29:36)
cancerData[bools] <- lapply(cancerData[bools], as.logical)

#normal distribution setup
cancerData <- mutate(cancerData, age.Z = rz.transform(cancerData$age), `first-sexual-intercourse.Z` = rz.transform(cancerData$`first-sexual-intercourse`), `number-of-sexual-partners.Z` = rz.transform(cancerData$`number-of-sexual-partners`), `num-of-pregnancies.Z` = rz.transform(cancerData$`num-of-pregnancies`),`smokes-years.Z` = rz.transform(cancerData$`smokes-years`),`smokes-packs-year.Z` = rz.transform(cancerData$`smokes-packs-year`), `hormonal-contraceptives-years.Z` = rz.transform(cancerData$`hormonal-contraceptives-years`),`iud-years.Z` = rz.transform(cancerData$`iud-years`), `stds-number.Z` = rz.transform(cancerData$`stds-number`), `stds-number-of-diagnosis.Z` = rz.transform(cancerData$`stds-number-of-diagnosis`), `stds-time-since-first-diagnosis.Z` = rz.transform(cancerData$`stds-time-since-first-diagnosis`), `stds-time-since-last-diagnosis.Z` = rz.transform(cancerData$`stds-time-since-last-diagnosis`))

write.csv(cancerData, "cervicalCAClean.csv")

#no rounding implemented because it would've impacted the accuracy of the data for the later markdown report