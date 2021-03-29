# Andrew Wang
# November 2
# Clustering Analysis of Vietnam War Casualties
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 9") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")

#library import
library(tidyverse)
library(cluster)
library(scatterplot3d)
library(gridExtra)
library(NbClust)
library(factoextra)

#options(warn = -1)

#basic cleaning + analysis
warData <- read.csv("warData.csv", header=TRUE, row.names = 1)
view(warData)
summary(warData)
str(warData)
print(sum(is.na(warData)))

#we need to get rid of the missing values
tail(warData)
warData <- na.omit(warData)
print(sum(is.na(warData)))

#set seed
set.seed(123)

#simple k-means clustering
# even though centers are broken I went with 8 just for a round clustering
group <- kmeans(warData, centers = 8, nstart = 25)
group$cluster
#simple plot
plot(warData$US.deaths, warData$Enemy.deaths, main = "US deaths vs. enemy deaths", xlab = "US deaths", ylab = "Enemy deaths", col = group$cluster, pch = 20)

#3d plot
scatterplot3d(warData$US.deaths, warData$Enemy.deaths, warData$South.Vietnamese.deaths,  type = "h", angle = 0, pch = 20,   main = "3D scatterplot Vietname war deaths", xlab = "US deaths", ylab = "Enemy deaths", zlab = "South Vietnamese deaths", grid = TRUE, col.grid = "blue", color = group$cluster)

#gets sample for better heatmap
subset <- warData[1:25, ]

#eudclidian heatmap
eucl <- dist(subset, method = "euclidean")
fviz_dist(eucl)
#heatmap number 2
#eucl2 <- dist(warData, method = "euclidean")
#fviz_dist(eucl2)

#hierarchical clustering dendogram
fviz_dend(hclust(dist(warData)), k = 6, as.ggplot = TRUE, show_labels = TRUE)
