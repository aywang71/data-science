# Andrew Wang
# October 17
# 2016 Election Results script
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
library(socviz)
library(gridExtra)

#devtools::install_github("kjhealy/socviz") #old install command

#define party colors for reference later
partyColors <- c("#2E74C0", "#CB454A")

#makes a ggplot for the republican point margin
pointMarginRepubs <- ggplot(election, aes(x = r_points, y = reorder(state, r_points), color = party)) + #sets up base ggplot 
  geom_vline(xintercept = 0, color = "gray10") + #vertical line at origin
  geom_point(size=2) + #dictates point size
  scale_color_manual(values = partyColors) +  # color scaling according to party colors
  scale_x_continuous(breaks = c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90), labels = c("90\n (Clinton)", "80", "70", "60", "50", "40", "30", "20", "10", "0", "10", "20", "30", "40", "50", "60", "70", "80", "90\n(Trump)")) + #labels on the x-axis
  facet_wrap(~census, ncol=1, scales="free_y") + #makes multiple graphs with 1 graph per row
  guides(color = FALSE) + # removes the legend on the side
  labs(title = "Point margin \n by Republicans", x = "Point Margin", y ="") + #assigns titles to various parts of graph
  theme(axis.text = element_text(size = 5)) #changes font size for axis

#pointMarginRepubs #output point

###

#makes a ggplot for democratic point margin
pointMarginDem <- ggplot(election, aes(x = -d_points, y = reorder(state, d_points), color = party)) + #sets up base ggplot 
  
  geom_vline(xintercept = 0, color = "gray10") + #vertical line at origin
  
  geom_point(size=2) + #dictates point size
  
  scale_color_manual(values = partyColors) +  # color scaling according to party colors
  
  scale_x_continuous(breaks = c(-90, -80, -70, -60, -50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50, 60, 70, 80, 90), labels = c("90\n (Clinton)", "80", "70", "60", "50", "40", "30", "20", "10", "0", "10", "20", "30", "40", "50", "60", "70", "80", "90\n(Trump)")) + #labels on the x-axis
  
  facet_wrap(~census, ncol=1, scales="free_y") + #makes multiple graphs with 1 graph per row
  
  guides(color = FALSE) + # removes the legend on the side
  
  labs(title = "Point margin \n by Democrats", x = "Point Margin", y ="") + #assigns titles to various parts of graph
  
  theme(axis.text = element_text(size = 5)) #changes font size for axis

#pointMarginDem #output point

#export to pdf
pdf("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 7/WK7L1P1.pdf")
grid.arrange(pointMarginRepubs,pointMarginDem, ncol = 2, nrow = 1)
graphics.off()

###

#import data
states <- map_data("state")
#change column
election$region <- tolower(election$state)
str(election)
#merge data
statesElection <- left_join(states,election)

#party map
party <- ggplot(statesElection, aes(x=long,y=lat,group=group,fill=party)) + 
  geom_polygon(color="gray10", size = 0.1) + #makes the outline colors between states
  coord_map(projection = "albers", lat0=25, lat1=50) + #makes a coordinate map
  scale_fill_manual(values=partyColors) + 
  labs(title = "2016 election results", fill = NULL) + #graph labels
  theme_map()

party #output point

#build ggplot for trump voters
trump <- ggplot(statesElection, aes(x=long, y=lat, group=group, fill=pct_trump)) +  #basic ggplot setup
  geom_polygon(color="white", size = 0.1) + #makes the outline colors between states
  coord_map(projection = "albers", lat0=30, lat1=45) + #makes a coordinate map
  labs(title = "Trump Election Results 2016", fill = "Trump \n %") + #graph labels
  theme_map() #applies the theme map

#trump #output point

#build ggplot for clinton voters
clinton <- ggplot(statesElection, aes(x=long, y=lat, group=group, fill=pct_clinton)) +  #basic ggplot setup
  geom_polygon(color="white", size = 0.1) + #makes the outline colors between states
  coord_map(projection = "albers", lat0=30, lat1=45) + #makes a coordinate map
  theme_map() + #applies the theme map
  labs(title = "Clinton Election Results 2016", fill = "Clinton \n %") #graph labels

#clinton #output point

#build ggplot for difference between trump and clinton
Diff <- abs(statesElection$pct_trump - statesElection$pct_clinton)
diff <- ggplot(statesElection, aes(x=long, y=lat, group=group, fill=Diff)) + 
  geom_polygon(color = "white", size = 0.1) + 
  coord_map(projection = "albers", lat0 =30, lat1 = 45) + 
  theme_map() +
  labs(title = "Difference Election Results 2016", fill = "Diff \n %")

#diff #output point

#export to pdf
pdf("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 7/WK7L1P2.pdf")
grid.arrange(party, trump, clinton, diff, ncol = 2, nrow = 2)
graphics.off()
