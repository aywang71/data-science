#NOTE: "Per" is not exactly "per" its more like there are ___ amount of instances in a group of ___ size

# Andrew Wang
# October 18
# 2016 Violent Crime in America script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 7") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(tidyverse)
library(maps)
library(ggcorrplot)

crime <- read.csv("violentcrimes.csv")
#str(crime)
print(sum(is.na(crime))) #no missing data

#remove the "property" column - its just a sum of some other columns
crime$Property <- NULL

#takes out commas in columns we want to change
crime$Population <- gsub(",","",crime$Population)
crime$Annual <- gsub(",","",crime$Annual)
crime$Murder <- gsub(",","",crime$Murder)
crime$Rape <- gsub(",","",crime$Rape)
crime$Robbery <- gsub(",","",crime$Robbery)
crime$Assault <- gsub(",","",crime$Assault)
crime$Burglary <- gsub(",","",crime$Burglary)
crime$Larceny <- gsub(",","",crime$Larceny)
crime$Auto <- gsub(",","",crime$Auto)

#changes the columns to numeric
crime$Population <- as.numeric(crime$Population)
crime$Annual <- as.numeric(crime$Annual)
crime$Murder <- as.numeric(crime$Murder)
crime$Rape <- as.numeric(crime$Rape)
crime$Robbery <- as.numeric(crime$Robbery)
crime$Assault <- as.numeric(crime$Assault)
crime$Burglary <- as.numeric(crime$Burglary)
crime$Larceny <- as.numeric(crime$Larceny)
crime$Auto <- as.numeric(crime$Auto)

# correlogram of crimes
crimeSub <- crime[2:11]
crimeCor <- round(cor(crimeSub), 1)
corPlot <- ggcorrplot(crimeCor, 
                      hc.order = TRUE,
                      type = "upper",
                      lab = TRUE,
                      title="Correlogram of crime types")
corPlot

#import data
states <- map_data("state")
#new "region" column for the two datasets to be joined with
crime$region <- tolower(crime$State)
#join the state data into the crime data
stateCrime <- left_join(states,crime)

#population map
popPlot <- ggplot(stateCrime, aes(x=long, y=lat, group=group, fill=Population)) + 
  geom_polygon(color = "black", size = 0.1) + 
  coord_map(projection = "albers", lat0 =30, lat1 = 45) + 
  theme_map() +
  labs(title = "Population across the United States", fill = "Occurrences") +
  scale_fill_gradient(low =  "#ffffff", high = "#000000")
popPlot #output point

#robbery map
robPlot <- ggplot(stateCrime, aes(x=long, y=lat, group=group, fill=Robbery/Population*10000)) + 
  geom_polygon(color = "black", size = 0.1) + 
  coord_map(projection = "albers", lat0 =30, lat1 = 45) + 
  theme_map() +
  labs(title = "Robberies per 10,000 across the United States", fill = "Occurrences") +
  scale_fill_gradient(low =  "#ffffff", high = "#000000")
robPlot #output point

#larceny map
larPlot <- ggplot(stateCrime, aes(x=long, y=lat, group=group, fill=Larceny/Population*10000)) + 
  geom_polygon(color = "black", size = 0.1) + 
  coord_map(projection = "albers", lat0 =30, lat1 = 45) + 
  theme_map() +
  labs(title = "Larceny per 10,000 across the United States", fill = "Occurrences") +
  scale_fill_gradient(low =  "#ffffff", high = "#000000")
larPlot #output point

#murder map
murPlot <- ggplot(stateCrime, aes(x=long, y=lat, group=group, fill=Murder/Population*10000)) + 
  geom_polygon(color = "black", size = 0.1) + 
  coord_map(projection = "albers", lat0 =30, lat1 = 45) + 
  theme_map() +
  labs(title = "Murders per 10,000 across the United States", fill = "Occurrences") +
  scale_fill_gradient(low =  "#ffffff", high = "#000000")
murPlot #output point


#recalculate annual crime total because it is inaccurate
stateCrime$Annual <- stateCrime$Murder + stateCrime$Rape + stateCrime$Robbery + stateCrime$Assault + stateCrime$Burglary + stateCrime$Larceny + stateCrime$Auto

#total map (computed column)
totPlot <- ggplot(stateCrime, aes(x=long, y=lat, group=group, fill=Annual/Population*100)) + 
  geom_polygon(color = "black", size = 0.1) + 
  coord_map(projection = "albers", lat0 =30, lat1 = 45) + 
  theme_map() +
  labs(title = "Crime per 100 across the United States", fill = "Occurrences") +
  scale_fill_gradient(low =  "#ffffff", high = "#000000")
totPlot #output point

#boxplot of crime types (to look at variation)
# boxplot list for the indicators
boxplots <- ggplot(stack(crime[5:11]), aes(x = ind, y = values)) + 
  geom_boxplot() + 
  labs(x = "Type of crime", y = "Occurences", title = "Boxplots of crime in the United States")
boxplots