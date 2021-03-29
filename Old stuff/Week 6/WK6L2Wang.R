# Andrew Wang
# October 12
# Opiates script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 6") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(tidyverse)
library(ggcorrplot)
library(GGally)


#input csv file
water <- read.csv("ellerbee.csv")
#view(water)
#summary(water)

#fix columns CFS, DO, NH4, and saturation
water$CFS <- ifelse(is.na(water$CFS),mean(water$CFS,na.rm=TRUE),water$CFS)
water$DO <- ifelse(is.na(water$DO),mean(water$DO,na.rm=TRUE),water$DO)
water$NH4 <- ifelse(is.na(water$NH4),mean(water$NH4,na.rm=TRUE),water$NH4)
water$saturation <- ifelse(is.na(water$saturation),mean(water$saturation,na.rm=TRUE),water$saturation)
print(sum(is.na(water))) #debug point

# correlogram for all indicating variables
waterCor <- round(cor(water), 1)
corPlot <- ggcorrplot(waterCor, 
           hc.order = TRUE,
           type = "upper",
           lab = TRUE,
           title="Correlogram of water quality indicators")
corPlot

#scatterplot between DO and watertemp
DOWT <- ggplot(water, aes (x = WaterTemp, y = DO)) +
  geom_point(aes(color = WaterTemp)) +
  geom_smooth(method = "lm") + 
  labs(x = "Water temperature (Celsius)", y = "Dissolved Oxygen(mg/L)", title = "Scatterplot of water tempearture and dissolved oxygen") + 
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
DOWT

#scatterplot between NO3 and conductivity
NO3C <- ggplot(water, aes (x = Conduct, y = NO3)) +
  geom_point(aes(color = WaterTemp)) +
  geom_smooth(method = "lm") + 
  labs(x = "Conductivity (??S/cm)", y = "Nitrogen content(ppm)", title = "Scatterplot of conductivity and nitrogen content") + 
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
NO3C

#scatterplot between conductivity and water temperature
WTC <- ggplot(water, aes (x = Conduct, y = WaterTemp)) +
  geom_point(aes(color = WaterTemp)) +
  geom_smooth(method = "loess") + 
  labs(x = "Conductivity (??S/cm)", y = "Water temperature (Celsius)", title = "Scatterplot of conductivity and water temperature") + 
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
WTC

#scatterplot between pH and turbidity
PHT <- ggplot(water, aes (x = pH, y = Turb)) +
  geom_point(aes(color = WaterTemp)) +
  geom_smooth(method = "loess") + 
  labs(x = "pH (0-14)", y = "Turbidity (NTU)", title = "Scatterplot of water pH and water turbidity") + 
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
PHT

#scatterplot between pressure and strdepth
PSD <- ggplot(water, aes (x = StrDepth, y = Pressure)) +
  geom_point(aes(color = WaterTemp)) +
  geom_smooth(method = "lm") + 
  labs(x = "Stream depth (feet)", y = "Pressure (mmHg)", title = "Scatterplot of stream depth and pressure") + 
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))
PSD

# add size reference
PSD <- PSD + geom_count()
PSD

# heat map of NH4  by turbidity
NH4T <- ggplot(water, aes(NH4, Turb)) +
  geom_bin2d(bins = 20, color ="white") +
  scale_fill_gradient(low =  "#ffffff", high = "#000000") + 
  labs(x = "Ammonia (ppm)", y = "Turbidity (NTU)", title = "Rectangular bin plot of ammonia and turbidity")
NH4T

# boxplot list for the indicators
boxplots <- ggplot(stack(water), aes(x = ind, y = values)) + 
  geom_boxplot() + 
  labs(x = "Water quality indicator", y = "Numbers", title = "Boxplots of water quality indicators")
boxplots