# Andrew Wang
# September 12
# Chicago weather script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 3") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project

#set up libraries
library(datasets)
library(dplyr)
espohdf <- esoph

#output espoh dataset
View(espohdf)

#display summary data
View(espohdf)
class(espohdf)
dim(espohdf)
str(espohdf)
summary(espohdf)
sum(is.na(espohdf))

#change column names  
names(espohdf) <- c("AgeGroup","Alcohol","Tobacco","Cases","Controls")

#rename columns for age, alcohol, and tobacco
espohdf$AgeGroup <- factor(espohdf$AgeGroup, levels = c("25-34","35-44","45-54","55-64","65-74","75+"), labels = c("Millenials","Xennials","GenX","Boomers","Seniors","Elderly"))
espohdf$Alcohol <- factor(espohdf$Alcohol, levels = c("0-39g/day","40-79","80-119","120+"), labels = c("Minimal","Moderate","Significant","Heavy"))
espohdf$Tobacco <- factor(espohdf$Tobacco, levels = c("0-9g/day","10-19","20-29","30+"), labels = c("Minimal","Moderate","Significant","Heavy"))

#new dataset for age groups + alcohol use
ageAlcohol <- select(espohdf,AgeGroup,Alcohol)
#head(ageAlcohol)

#new dataset for heavy smokers
heavyS <- filter(espohdf, Tobacco == "Heavy")
#head(heavyS)

#new dataset for light drinkers and moderate smokers
lightDmoderateS <- filter(espohdf, Alcohol == "Minimal", Tobacco == "Moderate")
#head(lightDmoderateS)

#new column for difference between controls and cases
espohdf <- mutate(espohdf, difference = abs(Controls - Cases))

#sort by difference and then age group
espohdf <- arrange(espohdf, -difference, AgeGroup)

#summarize mean controls and cases
averageControl <- summarize(espohdf, meanControl = round(mean(Controls),2))
#str(averageControl)
averageCases <- summarize(espohdf, meanCases = round(mean(Cases),2))
#str(averageCases)

#function to find furthest control value from mean
furthestFromMean <- function(vec) {
  absVal <- vec - mean(vec)
  vec[abs(absVal) == max(abs(absVal))]
}
summarize(espohdf, distControl = furthestFromMean(Controls))

#New dataset to group by alcohol use
alcoholSorted <- arrange(espohdf, Alcohol)
#View(alcoholSorted)

#New dataset group by age, averages of Control and cases
ageSorted <- espohdf %>% group_by(AgeGroup) %>% summarize(meanControl = mean(Controls), meanCases = mean(Cases))
#str(ageSorted)
#View(ageSorted)

#writing espohdf to a csv file
write.csv(espohdf, "C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 3/espohv2.csv")