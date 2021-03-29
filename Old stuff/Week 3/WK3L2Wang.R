# Andrew Wang
# September 13
# Heart Data Analyses script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 3") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(dplyr)

#input csv file
heartData <- read.csv("dirtyheart.csv")
View(heartData)
names(heartData)
str(heartData)

#rename columns
#thal = Thalassemia 
#fluoro is a method for heart disease diagnosis
heartData <- rename(heartData , gender = sex, chestPain = cp, RBP = trestbps, Cholesterol = chol, FBS = fbs, restECG = restecg, maxHR = thalach, exAng = exang, depressionST = oldpeak, thal. = thal, fluoroMV = ca, heartAttack = target)
names(heartData)

#fix missing data
# you don't want the or == 0 for the bool columns where zero stands for something
heartData$age <- ifelse(is.na(heartData$age) | heartData$age == 0 ,round(mean(heartData$age,na.rm=TRUE),0),heartData$age)
heartData$gender <- ifelse(is.na(heartData$gender),round(mean(heartData$gender,na.rm=TRUE),0),heartData$gender)
heartData$chestPain <- ifelse(is.na(heartData$chestPain) | heartData$chestPain == 0 ,round(mean(heartData$chestPain,na.rm=TRUE),0),heartData$chestPain)
heartData$RBP <- ifelse(is.na(heartData$RBP) | heartData$RBP == 0 ,round(mean(heartData$RBP,na.rm=TRUE),0),heartData$RBP)
heartData$Cholesterol <- ifelse(is.na(heartData$Cholesterol) | heartData$Cholesterol == 0 ,round(mean(heartData$Cholesterol,na.rm=TRUE),0),heartData$Cholesterol)
heartData$FBS <- ifelse(is.na(heartData$FBS),round(mean(heartData$FBS,na.rm=TRUE),0),heartData$FBS)
heartData$restECG <- ifelse(is.na(heartData$restECG),round(mean(heartData$restECG,na.rm=TRUE),0),heartData$restECG)
heartData$maxHR <- ifelse(is.na(heartData$maxHR) | heartData$maxHR == 0 ,round(mean(heartData$maxHR,na.rm=TRUE),0),heartData$maxHR)
heartData$exAng <- ifelse(is.na(heartData$exAng),round(mean(heartData$exAng,na.rm=TRUE),0),heartData$exAng)
heartData$depressionST <- ifelse(is.na(heartData$depressionST) | heartData$depressionST == 0 ,round(mean(heartData$depressionST,na.rm=TRUE),1),heartData$depressionST)
heartData$slope <- ifelse(is.na(heartData$slope) | heartData$slope == 0 ,round(mean(heartData$slope,na.rm=TRUE),0),heartData$slope)
heartData$fluoroMV <- ifelse(is.na(heartData$fluoroMV) | heartData$fluoroMV == 0 ,round(mean(heartData$fluoroMV,na.rm=TRUE),0),heartData$fluoroMV)
heartData$thal. <- ifelse(is.na(heartData$thal.),3,heartData$thal.)
heartData$heartAttack <- ifelse(is.na(heartData$heartAttack),mean(heartData$heartAttack,na.rm=TRUE),heartData$heartAttack)
print(sum(is.na(heartData)))

#updates numeric values with actual their word representations
heartData$heartAttack = factor(heartData$heartAttack, levels = c(0,1), labels = c("No","Yes"))
heartData$gender = factor(heartData$gender, levels = c(0,1), labels = c("Female","Male"))
heartData$chestPain = factor(heartData$chestPain, levels = c(1,2,3,4), labels = c("Typical","Atypical","Non-Anginal","Asymptomatic"))
heartData$restECG = factor(heartData$restECG, levels = c(0,1,2), labels = c("Normal","Abnormal","LVH"))
heartData$exAng = factor(heartData$exAng, levels = c(0,1), labels = c("No","Yes"))
heartData$slope = factor(heartData$slope, levels = c(1,2,3), labels = c("Up","Flat","Down"))
heartData$FBS = factor(heartData$FBS, levels = c(0,1), labels = c("False","True"))
heartData$thal. = factor(heartData$thal., levels = c(3,6,7), labels = c("Normal","Fixed","Reversible"))

#arrange dataset by age
heartData <- arrange(heartData, age)

#select numeric data, left out a few values because the margins were otherwise too large
#grab gender here for male and female references, delete later
numerics <- select(heartData, age, gender, RBP, Cholesterol, maxHR, fluoroMV)
# make subsets based on filtering gender
numericsMale <- filter(numerics, gender == "Male")
numericsFemale <- filter(numerics, gender == "Female")
numerics$gender <- NULL
numericsMale$gender <- NULL
numericsFemale$gender <- NULL
pairs(numerics, upper.panel = panel.cor, diag.panel = panel.hist)
pairs(numericsMale, upper.panel = panel.cor, diag.panel = panel.hist)
pairs(numericsFemale, upper.panel = panel.cor, diag.panel = panel.hist)

#group_by + summerize
chestPainSummary <- heartData %>% group_by(chestPain) %>% summarize(
  meanRBP = mean(RBP),
  meanCholesteral = mean(Cholesterol),
  meanMaxHR = mean(maxHR),
)
chestPainSummaryMale <- filter(heartData, gender == "Male") %>% group_by(chestPain) %>% summarize(
  meanRBP = mean(RBP),
  meanCholesteral = mean(Cholesterol),
  meanMaxHR = mean(maxHR),
)
chestPainSummaryFemale <- filter(heartData, gender == "Female") %>% group_by(chestPain) %>% summarize(
  meanRBP = mean(RBP),
  meanCholesteral = mean(Cholesterol),
  meanMaxHR = mean(maxHR),
)
head(chestPainSummary)
head(chestPainSummaryMale)
head(chestPainSummaryFemale)

#mutate
numericDev <- mutate(numerics,
                     ageDev = round(mean(age)-age,2),
                     RBPDev = round(mean(RBP)-RBP,2),
                     cholesterolDev = round(mean(Cholesterol)-Cholesterol,2),
                     maxHRDev = round(mean(maxHR)-maxHR,2),
                     fluoroMVDev = round(mean(fluoroMV)-fluoroMV,2))
numericDev[1:5] <- NULL
head(numericDev)