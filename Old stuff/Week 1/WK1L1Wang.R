# Andrew Wang
# August 29
# cat anatomy script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
# load a library
library(MASS)
#  We want to analyze some cat anatomy data, body weight, gender, heart weight
# let's get some data and do some EDA
# Exploratory Data Analysis
head(cats)
dim(cats)
names(cats)
summary(cats)
str(cats)
#
# lets do some plots
hist(cats$Bwt, main = "Histogram of Cats Body weight")
#  transform my data
# log transformation
hist(log10(cats$Bwt))
# Rank-Z transformation
hist(rz.transform(cats$Bwt))
hist(cats$Hwt, main = "Histogram of Cats heart weight")
#  transform my data
# log transformation
hist(log10(cats$Hwt))
# Rank-Z transformation
hist(rz.transform(cats$Hwt))
# 
# lets do some data cleanup
colnames(cats) <- c("Gender", "BodyWt", "HeartWt")
names(cats)
cats$Gender <- factor(cats$Gender, levels = c("F", "M"), labels = c("Female", "Male"))
head(cats)
# 
# use a a user-defined function from the myfunctions.R file
pairs(cats, upper.panel = panel.cor, diag.panel=panel.hist)
# 
plot(cats$BodyWt~cats$HeartWt, xlab="Heat wt", ylab = "Body wt", main = "Plot of weights")
fit <- lm(cats$BodyWt~cats$HeartWt)
fit
abline(fit)
