# Andrew Wang
# September 6
# Chicago weather script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 2") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
# install lubridate package for date
#install.packages("lubridate", repos="http://cran.rstudio.com")
library(lubridate)

#input csv file
weather <- read.csv("chicago.csv")
#display summary data
View(weather)
class(weather)
dim(weather)
str(weather)
#remove index and city
weather$indx <- NULL
weather$city <- NULL
#change column names
names(weather) <- c("Temp","DewPoint","Date","PM25","PM10","O3","NO3")
weather$Date <- mdy(weather$Date)
#normalization of the PM25 column
weather$PM25 <- ifelse(is.na(weather$PM25),mean(weather$PM25,na.rm=TRUE),weather$PM25)
weather$PM25 <- weather$PM25/max(weather$PM25)
#normalization of the PM10 column
weather$PM10 <- ifelse(is.na(weather$PM10),mean(weather$PM10,na.rm=TRUE),weather$PM10)
weather$PM10 <- weather$PM10/max(weather$PM10)
numerics <- weather[,c(1,2,4:7)]
pairs(numerics, upper.panel = panel.cor, diag.panel = panel.hist)