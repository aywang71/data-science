# Andrew Wang
# October 25
# PCA of Launched Drug Data
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 8") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")

#library import
library(tidyverse)

drugs <- read.csv("drugs.csv", header=TRUE, row.names = 1)
str(drugs)
print(sum(is.na(drugs)))
drugPCA <- prcomp(drugs)
names(drugPCA)

#screeplot + plot
plot(drugPCA$x[,1],drugPCA$x[,2])
screeplot(drugPCA)

#transposed data + screeplot + plot
drugsT <- t(drugs)
drugTPCA <- prcomp(drugsT)
names(drugTPCA)
plot(drugTPCA$x[,1],drugTPCA$x[,2])
screeplot(drugTPCA)

#we like the transposed data better - lets keep it
drugPCA <- drugTPCA

#variance calculation
drugVariance <- drugPCA$sdev^2
drugVariance <- round(drugVariance/sum(drugVariance)*100,1)
drugVariance
barplot(drugVariance)

#contributing variable plot
drugPD <- data.frame(Sample=rownames(drugPCA$x), X=drugPCA$x[,1], Y=drugPCA$x[,2])
drugPD

drugP <- ggplot(data=drugPD, aes(x=X, y=Y, label=Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", drugVariance[1], "%", sep="")) +
  ylab(paste("PC2 - ", drugVariance[2]))+ 
  ggtitle("Influencing factor plot")
drugP
