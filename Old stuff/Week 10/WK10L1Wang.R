# Andrew Wang
# November 7
# Machine Learning Classification
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 10") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")

options(warn = -1)

library(class)
library(ggvis)
library(gmodels)
library(tidyverse)
library(caret)
library(GGally)
library(gridExtra)

climate <- read.csv("pop_failures.csv")
View(climate)
#dim(climate)
#glimpse(climate)
#str(climate)
print(sum(is.na(climate))) #no missing data

#converting a few chr types to nums
climate$vertical_decay_scale <- as.numeric(as.factor(climate$vertical_decay_scale))
climate$convect_corr <- as.numeric(as.factor(climate$convect_corr))
climate$bckgrnd_vdc1 <- as.numeric(as.factor(climate$bckgrnd_vdc1))

#used as.factor() so that it wouldn't pick up unrecognizable characters and then replace them into NAs by coercion  

#look in background reading for column name descriptions



#pair plot for viscosity variables (to look at their relationship) + prandtl
viscos <- climate[, c(3:8, 20)]
ggpairs(viscos, aes(color = as.character(climate$outcome)))
# needs as.character because color doesn't accept numeric
#conclusion: none of the viscosity variables are related at all
#view(viscos)


#pair plot for diffusivity variables + prandtl
diff <- climate[, c(16:20)]
ggpairs(diff, aes(color = as.character(climate$outcome)))
#conclusion: no relationship between diffusivity variables either
#view(diff)

#basic plot
plot1 <- ggplot(climate, aes(x = vconst_corr, y = outcome, color = outcome)) +
  geom_point() + 
  guides(color = FALSE) +
  labs(title = "Viscosity with relation to outcome", x = "Viscosity", y = "Sucess")
plot1
#conclusion: High viscosity has a higher chance of simulation failure

#basic plot 2
plot2 <- ggplot(climate, aes(x = bckgrnd_vdc1, y = outcome, color = outcome)) +
  geom_point() + 
  guides(color = FALSE) +
  labs(title = "Vertical diffusivity with relation to outcome", x = "Vertical diffusivity", y = "Sucess")
plot2
#conclusion: No recognizable relationship between diffusivity and simulation outcome

table(climate$outcome) #outcome scenarios

#set seed
set.seed(1234)

# data preparation and splitting code
#randomized index
ind <- sample(2, nrow(climate), replace = TRUE, prob = c(0.7, 0.3))
# see webinar notes for specifications
#view(ind)
trainingSet <- climate[ind == 1, 1:21]
#view(trainingSet)
testingSet <- climate[ind == 2, 1:21]
#view(testingSet)
trainingLabels <- climate[ind == 1, 21]
#view(trainingLabels)
testingLabels <- climate[ind == 2, 21]
#view(testingLabels)

#KNN ML technique
predictionSet <- knn(train = trainingSet, test = testingSet, cl = trainingLabels, k = 9)
predictionSet

#merge data
mergedData <- data.frame(testingLabels, predictionSet)
dim(mergedData)
view(mergedData)

#create the final data
names <- colnames(testingSet) #stores the column names for reference in line 97
finalData <- cbind(testingSet, mergedData)
dim(finalData)
names(finalData) <- c(names, "Outcome", "Predicted Outcome")

#crosstable
CrossTable(x = testingLabels, y = predictionSet, prop.chisq = FALSE)
# conclusion: 17 misses out of 154 dataset
# many false positives - predicted success when outcome was failure (11/12 failures were predicted wrong)
