rm(list=ls())

library(tidyverse)
library(dplyr)
library(class)
library(gmodels)
library(treemap)
library(d3treeR)



setwd("C:/Users/hyper/OneDrive/Documents/GitHub/data-science/9.15.23 Data Challenge")

df <- read.csv("fa23_datachallenge.csv")

#Part 1 EDA analysis
str(df)

#Attempts to clarify and understanding the data definitions
#1: DEP_DEL15: flags if there is a 15 minute or longer delay
#subset <- df %>% filter(DEP_DEL15 == 1)
#view(subset[, c('DEP_DELAY_NEW', 'DEP_DEL15')])

#drop columns with low/no data -- no point in guessing the data and will not help regression/classification either
sapply(df, function(x) sum(is.na(x)))
df <- subset(df, select = -c(PSUN, TSUN, CARRIER_DELAY, WEATHER_DELAY, NAS_DELAY, SECURITY_DELAY, LATE_AIRCRAFT_DELAY, CANCELLATION_CODE, ORIGIN_AIRPORT_NAME))

#try to fix snow column 
# subset <- df %>% filter(PRCP == 0)
# summary(subset$SNOW)
# summary(subset$SNWD)
#both summaries are overwhelmingly 0 for when PRCP = 0
df <- df %>% rowwise() %>% mutate(
  SNOW = ifelse(is.na(SNOW) && PRCP == 0, 0, SNOW), 
  SNWD = ifelse(is.na(SNWD) && PRCP == 0, 0, SNWD), 
)

#try to clean up empty rows but not lose too much data
# subset <- df %>% filter(!is.na(TMAX))
# sum(is.na(subset$TMAX))
# 17% cleaned is fine -- if given more time would just match average weather conditions at an airporto to other rows with the data from that airport
# complications: if certain airports don't report weather data, that whole move would be useless -- also just not time-wise right now given time constraints
df <- df %>% filter(!is.na(TMAX) && !is.na(TMIN))
df$TAVG <- (df$TMAX + df$TMIN)/2
#Same logic for removing rows without airport information -- could match with existing database but too time constrained
df <- df %>% filter(!is.na(AIRPORT_FLIGHTS_MONTH))

#remove null values for departure time -- as we are specifcally looking at only depature delays, we don't care about cancellcations (if a flight is cancelled, it will have NA for DEP_TIME and similar columns)
df <- df %>% filter(!is.na(DEP_TIME) && !is.na(DEP_DELAY_NEW))
df <- subset(df, select = -c(CANCELLED))

#Fix NA values for snow and snwd
df <- df %>% rowwise() %>% mutate(
  SNOW = ifelse(is.na(SNOW), 0, SNOW), 
  SNWD = ifelse(is.na(SNWD), 0, SNWD)
)

#remove remaining NA values, nothing of significant volume remains for cleaning
df <- na.omit(df)
df <- as.data.frame(df)

#Look at some static characteristics

#Break down some delays by flight factors
df$isDelayed <- ifelse(df$DEP_DELAY_NEW > 0, 1, 0)
subset <- df %>% filter(isDelayed == 1) %>% count(OP_UNIQUE_CARRIER)
pie(subset$n, labels = subset$OP_UNIQUE_CARRIER)

subset$n <- subset$n/sum(subset$n) * 100
ggplot(subset, aes(x = "", y = n, fill = OP_UNIQUE_CARRIER)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + theme_void() +
  geom_text(aes(label = paste0(round(n,1), "%")), position = position_stack(vjust=0.5)) +
  labs(x = NULL, y = NULL, fill = NULL) + 
  labs(title="Delayed flights by airline code")
#Southwest airlines, american airlines, delta

#Break down some delays by month
subset <- df %>% filter(isDelayed == 1) %>% count(MONTH)
pie(subset$n, labels = subset$MONTH)

df$isnow <- ifelse(df$SNOW > 0, 1, 0)
subset <- df %>% filter(isDelayed == 1) %>% count(isnow)
pie(subset$n, labels = subset$isnow)

#treemap generation
subset <- df %>% group_by(OP_UNIQUE_CARRIER, isnow) %>% count(isDelayed)

tree <- treemap(subset,
        index=c("OP_UNIQUE_CARRIER","isnow"),
        vSize="n",
        type="index"
)
tree2 <- d3tree2( tree ,  rootname = "Delays by Airline and Weather" )



#Time permitting, EDA could look more into these specific carries -- a lot is dependent on what exactly the client is looking for as far as action

#Classification -- KNN
#The idea here is to group influencing variables into major factor groups to determine groups of highest accuracy (i.e thereby most important)
#Another potential method would be PCA but that's not a classification algorithm so it cannot be used here
#label setup 

#need to decrease the number of obs being used for better knn model runtime -- actual model would prefer to be run with large obs but computational limitations
ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.5, 0.5))
df <- df[ind == 1, ]

ind <- sample(2, nrow(df), replace = TRUE, prob = c(0.7, 0.3))
trainingLabels <- df[ind == 1, 40]
testingLabels <- df[ind == 2, 40]


#group factors based on 'type' and run multiple n = 5 classification clusters on the data
dataSubset <- subset(df, select = -c(isDelayed))

customKNN <- function(dataSubset, listColumns){
  trainingData <- dataSubset[ind == 1, ]
  trainingData <- subset(trainingData, select = listColumns)
  print(dim(trainingData))
  testingData <- dataSubset[ind == 2, ]
  testingData <- subset(testingData, select = listColumns)
  print(dim(testingData))
  set.seed(123)
  predictions <- knn(train = trainingData, test = testingData, cl = trainingLabels, k = 5, use.all = FALSE)
  CrossTable(x = testingLabels, y = predictions, prop.chisq = FALSE)
}

#Testing various groups of variables -- commented out groups failed due to overly similar datapoints (too many ties)
#timeResults <- customKNN(dataSubset, c("MONTH", "DAY_OF_MONTH", "DAY_OF_WEEK"))
arrivalTimeResults <- customKNN(dataSubset, c("CRS_ARR_TIME", "ARR_TIME", "ARR_DELAY_NEW"))
flightResults <- customKNN(dataSubset, c("CRS_ELAPSED_TIME", "ACTUAL_ELAPSED_TIME", "DISTANCE"))
weatherResults <- customKNN(dataSubset, c("PRCP", "SNOW", "SNWD", "TMIN", "TMAX", "AWND"))
#snowResults <- customKNN(dataSubset, c("PRCP", "SNOW", "SNWD"))
demoResults <- customKNN(dataSubset, c("AIRPORT_FLIGHTS_MONTH", "AIRLINE_FLIGHTS_MONTH", "AIRLINE_AIRPORT_FLIGHTS_MONTH", "AVG_MONTHLY_PASS_AIRPORT", "AVG_MONTHLY_PASS_AIRLINE", "FLT_ATTENDANTS_PER_PASS", "GROUND_SERV_PER_PASS"))

#Conclusions
#KNN was largely unfruitful -- given more time would like to implement a component analysis alongside further EDA analysis 