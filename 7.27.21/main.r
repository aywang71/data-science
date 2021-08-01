library(tidyverse)
library(statsr)
library(datasets)
library(readxl)

setwd("~/GitHub/data-science/7.27.21") # go to this folder

health_female = read_excel("health_female.xls")

ggplot(health_female, aes(x = WT, y = WAIST)) + geom_point(color = "blue") + ggtitle("Weight vs Waist Size")

ggplot(data = health_female, aes(x = AGE)) + geom_histogram(bins=30, color="black", fill="pink") + ggtitle("Histogram")

health_both_genders = read_excel("health_both.xlsx")

aggregate(x = health_both_genders$SYS, by = list(health_both_genders$GENDER), FUN = median)

ggplot(health_both_genders, aes(x = GENDER, y = SYS)) + 
  geom_boxplot(color = "red", fill = "pink") + ggtitle("Box plot")
by(health_both_genders$SYS, health_both_genders$GENDER, summary)
