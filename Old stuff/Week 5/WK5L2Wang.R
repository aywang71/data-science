# Andrew Wang
# September 28
# Cervical Cancer Part 2 script
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/Assignments/Week 5") # go to this folder
#load up myfunctions.R
source("C:/Users/hyper/OneDrive/Desktop/Desktop Folders/Programming/R/myfunctions.R")
#
## Now we start with the project
#library import
library(tidyverse)

# Step 1
#input csv file
cancerData <- read.csv("cervicalCAClean.csv")
dim(cancerData)
str(cancerData)
summary(cancerData)
View(cancerData)
cancerData <- arrange(cancerData,age)

# delete useless columns
cancerData$X <- NULL
cancerData$dx <- NULL

#add new column for analysis
cancerData <- mutate(cancerData, total.smokes = cancerData$smokes.years * cancerData$smokes.packs.year)

# Step 2
# Histogram plots of various influencing factors (age, sex partners, total smokes, pregnancies, std amount) (use a graphical window to cut down on paper length)
par(mfrow = c(3,2)) #setting up a graphics grid
boxplot(cancerData$age, main = "Histogram of age", xlab = "Age (years)", ylab = "Number of occurences")
boxplot(cancerData$number.of.sexual.partners, main = "Histogram of sexual partners", xlab = "Number of sexual partners", ylab = "Number of occurences")
boxplot(cancerData$total.smokes, main = "Histogram of total cigarette packs smoked", xlab = "Cigarette packs smoked", ylab = "Number of occurences")
print(mean(cancerData$total.smokes))
boxplot(cancerData$num.of.pregnancies, main = "Histogram of pregnancies", xlab = "Number of pregnancies", ylab = "Number of occurences")
boxplot(cancerData$stds.number, main = "Histogram of STD count", xlab = "Numer of STDs", ylab = "Number of occurences")

#set up a subset for evaluated data
evalVar <- select(cancerData, age, number.of.sexual.partners, total.smokes, num.of.pregnancies, stds.number)
str(evalVar)
# pair correlation plot
pairs(evalVar, upper.panel = panel.cor, diag.panel = panel.hist)

# linear regression model (linear regression graphing included):
#: smoke years ~ smoke packs per year
SYSP <- lm(cancerData$smokes.years~cancerData$smokes.packs.year) # creates regression subset of smoked years by smoked packs per year
plot(cancerData$smokes.years~cancerData$smokes.packs.year, main = "Plot of smoked years vs packs/year", xlab = "Number of years smoking", ylab = "Number of packs smoked") #creates a scatterplot
abline(SYSP) #creates a line of best fit off of the regression subset
summary(SYSP) #outputs stats of subset

#: hormonal contraceptives ~ IUDs
HCIUD <- lm(cancerData$hormonal.contraceptives.years~cancerData$iud.years)
plot(cancerData$hormonal.contraceptives.years~cancerData$iud.years, main = "Plot of hormonal contraceptive usage vs IUD usage", xlab = "IUD years", ylab = "Contraceptive years")
abline(HCIUD)
summary(HCIUD)

#: age ~ sexual partners ~ pregnancies ~ std count (multi-regression)
mC <- lm(cancerData$biopsy~evalVar$age+evalVar$number.of.sexual.partners+evalVar$total.smokes+evalVar$num.of.pregnancies+evalVar$stds.number)
mC
mHPV <- lm(cancerData$dx.hpv~evalVar$age+evalVar$number.of.sexual.partners+evalVar$total.smokes+evalVar$num.of.pregnancies+evalVar$stds.number)
mHPV

# BIC analysis:
#: Cancer~HPV
print(abs(BIC(lm(cancerData$biopsy~1))-BIC(lm(cancerData$biopsy~cancerData$dx.hpv)))) #conclusion: Very good relationship between the two 
#explanation: we take the absolute value of the difference between the two BIC values

#: Cancer~CIN
print(abs(BIC(lm(cancerData$biopsy~1))-BIC(lm(cancerData$biopsy~cancerData$dx.cin)))) #conclusion: moderate relationship between the two

#: Cancer ~ age + sex partners + smoke total + pregnancies
print(abs(BIC(lm(cancerData$biopsy~1))-BIC(lm(cancerData$biopsy~cancerData$age+cancerData$number.of.sexual.partners+cancerData$total.smokes+cancerData$num.of.pregnancies)))) #conclusion: very good relationship between the two

#: HPV ~ age + sex partners + smoke total + pregnancies
print(abs(BIC(lm(cancerData$dx.hpv~1))-BIC(lm(cancerData$dx.hpv~cancerData$age+cancerData$number.of.sexual.partners+cancerData$total.smokes+cancerData$num.of.pregnancies)))) #conclusion: moderate relationship between the two