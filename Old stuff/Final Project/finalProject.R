# Andrew Wang
# November 22
# Final project script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("~/GitHub/data-science/Old stuff/Final Project") # go to this folder

#load up myfunctions.R
source("~/GitHub/data-science/myfunctions.R")

#library import
library(maps)
library(tidyverse)
library(usmap)
library(gridExtra)

#data import
election <- read.csv("data/dataFresh.csv")
View(election)
dim(election)

#standardize data:
election$Poverty   <- round(election$TotalPop * election$Poverty / 100)
election$ChildPoverty <- NULL #no numbers for children
election$Professional <- round(election$TotalPop * election$Professional / 100)
election$Service <- round(election$TotalPop * election$Service / 100)
election$Office <- round(election$TotalPop * election$Office / 100)
election$Construction <- round(election$TotalPop * election$Construction / 100)
election$Production <- round(election$TotalPop * election$Production / 100)
election$Drive <- round(election$TotalPop * election$Drive / 100)
election$Carpool <- round(election$TotalPop * election$Carpool / 100)
election$Transit <- round(election$TotalPop * election$Transit / 100)
election$Walk <- round(election$TotalPop * election$Walk / 100)
election$OtherTransp <- round(election$TotalPop * election$OtherTransp / 100)
election$PrivateWork <- round(election$TotalPop * election$PrivateWork / 100)
election$PublicWork <- round(election$TotalPop * election$PublicWork / 100)
election$SelfEmployed <- round(election$TotalPop * election$SelfEmployed / 100)
election$FamilyWork <- round(election$TotalPop * election$FamilyWork / 100)
election$Unemployment <- round(election$TotalPop * election$Unemployment / 100)

election <- na.omit(election) # we want to get rid of everything without numbers

dim(election)

#define party colors for coloring of geographic plots
partyColors <- c("#2E74C0", "#CB454A","#0000FF")

#margin finding with mutate
election <- mutate(
  election,
  margin2016 = percentage16_Hillary_Clinton - percentage16_Donald_Trump,
  margin2020 = percentage20_Joe_Biden - percentage20_Donald_Trump,
  shift = margin2020 - margin2016
)

#list of flipped states
changed <- c("AZ","WI","MI","GA","PA") #only from R --> D

###PCA analysis for demographic factors``

#subset for demographics
demographics <- election[,16:50]

#PCA analysis function to save code length
customPCA <- function(data, description){
  pca <- prcomp(t(data))
  View(pca$x)
  #screeplot + plot
  plot(pca$x[,1],pca$x[,2])
  #variance calculation
  pcaV <- pca$sdev^2
  pcaV <- round(pcaV/sum(pcaV)*100,1)
  pcaV
  barplot(pcaV)
  #contributing variable plot
  pcaDF <- data.frame(Sample=rownames(pca$x), X=pca$x[,1], Y=pca$x[,2])
  pcaP <- ggplot(data=pcaDF, aes(x=X, y=Y, label=Sample)) +
    geom_text() +
    xlab(paste("PC1 - ", pcaV[1], "%", sep="")) +
    ylab(paste("PC2 - ", pcaV[2], "%", sep="")) + 
    ggtitle(paste("Demographic indicators in PCA analysis -",  description, sep="\n"))
  pcaP
}

customPCA(demographics, "United States")
## Conclusion: Total population, Private work, Drive, Voting age citizens, women

#as a group (all changed states)
changedStates <- filter(election, state == "AZ" | state == "WI" | state == "MI" | state == "GA" | state == "PA")
changedStates <- changedStates[,c(16:50)] 
customPCA(changedStates, "Flipped states")
## Conclusion: Total population, Private work, Drive, Voting age citizens, women

#arizona
arizona <- filter(election, state == "AZ")
arizona <- arizona[,c(16:50)] 
customPCA(arizona, "Arizona")
## Conclusion: Total population, Private work, Drive, Voting age citizens, women

#wisconsin
wisconsin <- filter(election, state == "WI")
wisconsin <- wisconsin[,c(16:50)] 
customPCA(wisconsin, "Wisconsin")
## Conclusion: Total population, Private work, Drive, Voting age citizens, women

#michigan
michigan <- filter(election, state == "MA")
michigan <- michigan[,c(16:50)] 
customPCA(michigan,"Michgain")
## Conclusion: Total population, Private work, Drive, Voting age citizens, women

#georgia
georgia <- filter(election, state == "GA")
georgia <- georgia[,c(16:50)] 
customPCA(georgia,"Georgia")
## Conclusion: Total population, Private work, Drive, Voting age citizens, women

#pennslyvania
pennslyvania <- filter(election, state == "PA")
pennslyvania <- pennslyvania[,c(16:50)] 
customPCA(pennslyvania,"Pennslyvania")
## Conclusion: Total population, Private work, Voting age citizens, Drive, women
 
#adding FIPS numbers to county dataset 
localCounty <- data.frame(countypop)
#a bunch of replacement for matching stuff
localCounty$county <- gsub(" county", "", localCounty$county, ignore.case=TRUE)
localCounty$county <- gsub(" parish", "", localCounty$county, ignore.case=TRUE)
localCounty$county <- gsub(" borough", "", localCounty$county, ignore.case=TRUE)
localCounty$county <- gsub(" census area", "", localCounty$county, ignore.case=TRUE)
localCounty$state <- localCounty$abbr
localCounty$abbr <- NULL
#join datasets
countyFIPS <- inner_join(localCounty, election, by = c("county","state"))
countyFIPS$pop_2015 <- NULL

###countyFIPS dataset VERY IMPORTANT

#Triple graph format for shift detection (20,shift,16)
main2020 <- plot_usmap(data = countyFIPS, values = "margin2020", color = "white") + 
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white", name = "Electoral shift", label = scales::comma) +
  theme(legend.position = "none")
main2020
mainShift <- plot_usmap(data = countyFIPS, values = "shift", color = "white") + 
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white", name = "Electoral shift", label = scales::comma) +
  theme(legend.position = "none")
mainShift
main2016 <- plot_usmap(data = countyFIPS, values = "margin2016", color = "white") + 
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white", name = "Electoral shift", label = scales::comma) +
  theme(legend.position = "none")
main2016

#sample plotting for indicators - by county, whole US
sample <- plot_usmap(data = countyFIPS, values = "TotalPop", color = "gray") + 
  scale_fill_gradient2(low = "white", mid = "gray", high = "black", na.value = "white", name = "Population", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Population by county")
sample

#sample plotting for indicators - by county, single state
singleState <- plot_usmap(data = countyFIPS, values = "TotalPop", include = "MI", color = "gray") +
  scale_fill_continuous( low = "white", high = "black", name = "Population 2017", label = scales::comma ) + 
  theme(legend.position = "right") +
  labs(title = "Population by county in Michagin")
singleState

#michgain example
miPop <- plot_usmap(data = countyFIPS, values = "TotalPop", include = "MI", color = "gray") +
  scale_fill_continuous( low = "black", high = "white", name = "Population 2017") + 
  theme(legend.position = "none") +
  labs(title = "Population by county")
#miPop
miShift <- plot_usmap(data = countyFIPS, values = "shift", include = "MI", color = "gray") +
  scale_fill_continuous( low = "white", high = "black", name = "Shift", label = scales::comma ) + 
  theme(legend.position = "none") +
  labs(title = "Voter shift by county")
#miShift
grid.arrange(miPop,miShift,nrow=1)

#Wisconsin example
wiPop <- plot_usmap(data = countyFIPS, values = "TotalPop", include = "WI", color = "gray") +
  scale_fill_continuous( low = "black", high = "white", name = "Population 2017") + 
  theme(legend.position = "bottom") #+
  #labs(title = "Population by county")
#wiPop
wiShift <- plot_usmap(data = countyFIPS, values = "shift", include = "WI", color = "gray") +
  scale_fill_continuous( low = "white", high = "black", name = "Shift", label = scales::comma ) + 
  theme(legend.position = "bottom") #+
  #labs(title = "Voter shift by county")
#wiShift
grid.arrange(wiPop,wiShift,nrow=1)

grid.arrange(miPop, miShift, wiPop, wiShift, nrow = 2, ncol = 2)

#sample plotting for indicators - by state, whole US
#pipe for group sorting
stateSet <- election %>% 
  group_by(state) %>% 
  summarize(
    stateMargin2016 = (sum(votes16_Hillary_Clinton)/sum(total_votes16)) - (sum(votes16_Donald_Trump)/sum(total_votes16)),
    stateMargin2020 = (sum(votes20_Joe_Biden)/sum(total_votes20)) - (sum(votes20_Donald_Trump)/sum(total_votes20)),
    totalPop = sum(TotalPop)
  )
stateSet$stateShift <- stateSet$stateMargin2020 - stateSet$stateMargin2016
stateSet$abbr <- stateSet$state
stateSet$state <- NULL
#formatting and join stuff for FIPS codes
stateSet <- inner_join(stateSet,statepop)
stateSet$pop_2015 <- NULL
#plot
stateShift2 <- plot_usmap(data = stateSet, values = "stateShift", color = "white") + 
  scale_fill_gradient2(low = "black", mid = "white", high = "black", na.value = "white") +
  theme(legend.position = "none")
statePop <- plot_usmap(data = stateSet, values = "totalPop", color = "white") + 
  scale_fill_gradient2(low = "white", mid = "gray", high = "black", na.value = "white") +
  theme(legend.position = "none")
#statePop
grid.arrange(stateShift2,statePop,nrow=1)
#plot
state2020 <- plot_usmap(data = stateSet, values = "stateMargin2020", color = "gray") +
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white") +
  theme(legend.position = "none")
#state2020
stateShift <- plot_usmap(data = stateSet, values = "stateShift", color = "gray") +
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white") +
  theme(legend.position = "bottom")
stateShift
#plot
state2016 <- plot_usmap(data = stateSet, values = "stateMargin2016", color = "gray") +
  scale_fill_gradient2(low = partyColors[2], mid = "white", high = partyColors[1], na.value = "white") +
  theme(legend.position = "none")
#state2016
grid.arrange(state2016,stateShift,state2020, nrow = 1)