install.packages("tidyverse")
install.packages("arules")
library(tidyverse)
library(arules)
health<-read.csv("Sleep_health_and_lifestyle_dataset.csv")
health

#data summary
summary(health)

#checking data for null values
health<-health[complete.cases(health), ]



summary(health$Age)
health$Age =  ifelse(health$Age<18, "Age below 18", 
              ifelse(health$Age<=24, "Age 18-24",
              ifelse(health$Age<=34, "Age 25-34",
              ifelse(health$Age<=44, "Age 35-44",
              ifelse(health$Age<=54, "Age 45-54",
              ifelse(health$Age<=64, "Age 55-64",
              ifelse(health$Age>=65, "Age 65 or above", NA)))))))

summary(health$Sleep.Duration)
## sleep time in our data has min=5.8 and max 8.5
## recommended sleep time for adults is 7-9 hours - in our data there are only adults
health$Sleep.Duration =  ifelse(health$Sleep.Duration<7, "Too short sleep", 
                         ifelse(health$Sleep.Duration<=9, "Enought sleep",
                         ifelse(health$Sleep.Duration>9, "Too long sleep"))) 

summary(health$Quality.of.Sleep)

health$Quality.of.Sleep =  ifelse(health$Quality.of.Sleep<=4, "Low quality sleep", 
                           ifelse(health$Quality.of.Sleep<=6, "Medium quality sleep",
                           ifelse(health$Quality.of.Sleep<=8, "Good quality sleep",
                           ifelse(health$Quality.of.Sleep>8, "Very good quality sleep")))) 


summary(health$Physical.Activity.Level)
## phisical activti level is given in minutes per day 
health$Physical.Activity.Level =  ifelse(health$Physical.Activity.Level<=30, "under 30 min of physical activity a day", 
                                  ifelse(health$Physical.Activity.Level<=60, "30-60 min of physical activity a day",
                                         ifelse(health$Physical.Activity.Level>60, "over 60 min of physical activity a day")))
                                                

summary(health$Blood.Pressure)
#Blood Preasue variable is charactr data type
