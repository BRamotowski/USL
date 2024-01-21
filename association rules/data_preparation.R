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
                           ifelse(health$Quality.of.Sleep>8, "Very good quality sleep",NA)))) 


summary(health$Physical.Activity.Level)
## phisical activti level is given in minutes per day 
health$Physical.Activity.Level =  ifelse(health$Physical.Activity.Level<=30, "Under 30 min of physical activity a day", 
                                  ifelse(health$Physical.Activity.Level<=60, "30-60 min of physical activity a day",
                                         ifelse(health$Physical.Activity.Level>60, "Over 60 min of physical activity a day",NA)))

summary(health$Stress.Level)

health$Stress.Level =  ifelse(health$Stress.Level<=2, "Very low stressed",
                            ifelse(health$Stress.Level<=4, "Low stressed", 
                                  ifelse(health$Stress.Level<=6, "Medium stressed",
                                         ifelse(health$Stress.Level<=8, "Stressed",
                                                ifelse(health$Stress.Level>8, "Very stressed",NA)))))                                                 

summary(health$Blood.Pressure)
#Blood Preasue variable is charactr data type

summary(health$Heart.Rate)
##https://www.physio-pedia.com/Heart_Rate
health$Heart.Rate =  ifelse(health$Heart.Rate<=55, "Excellent heart rate", 
                     ifelse(health$Heart.Rate<=65, "Good heart rate",
                     ifelse(health$Heart.Rate<=75, "Avrage heart rate",
                     ifelse(health$Heart.Rate<=85, "Poor heart rate",       
                     ifelse(health$Heart.Rate>85, "Bad heart rate",NA)))))

summary(health$Daily.Steps)
## https://www.10000steps.org.au/articles/healthy-lifestyles/counting-steps/
health$Daily.Steps =  ifelse(health$Daily.Steps<=5000, "Sedentary daily steps", 
                            ifelse(health$Daily.Steps<=7499, "Low daily steps",
                                   ifelse(health$Daily.Steps<=9999, "Somewhat active daily steps",
                                          ifelse(health$Daily.Steps<=12499, "Active daily steps",       
                                                 ifelse(health$Daily.Steps>12500, "Highly active daily steps",NA)))))

summary(health$BMI.Category)
unique(health$BMI.Category)
health$BMI.Category =ifelse(health$BMI.Category=="Normal", "Normal weight",
                            ifelse(health$BMI.Category=="Overweight", "Overweight",
                                   ifelse(health$BMI.Category=="Obese", "Obese",NA)))

health_clean=health[c(2:13)]

write.csv(health_clean, file = 'health_clean.csv')

transactions = read.transactions('health_clean.csv', format='basket', sep=',', skip=0)
inspect(transactions)
size(transactions) 
length(transactions)

round(itemFrequency(transactions),3) %>% sort(.,decreasing=T)

ctab<-crossTable(transactions, sort=TRUE) 
ctab<-crossTable(transactions, measure="count", sort=TRUE) 
head(ctab,5)

#standard rules
rules.transactions<-apriori(transactions, parameter=list(supp=0.1, conf=0.5))
rules.transactionsSorted<-sort(rules.transactions, by="lift", decreasing=TRUE)
inspect(head(rules.transactionsSorted))

#stressed rule
rulesStressed<-apriori(data=transactions, parameter=list(supp=0.1, conf=0.1), appearance=list(default="lhs", rhs="Stressed"), control=list(verbose=F)) 
rulesStressedSorted<-sort(rulesStressed, by="confidence", decreasing=TRUE)
inspect(head(rulesStressedSorted))
#overweight rule
rulesOverweight<-apriori(data=transactions, parameter=list(supp=0.1, conf=0.1), appearance=list(default="lhs", rhs="Overweight"), control=list(verbose=F)) 
rulesOverweightSorted<-sort(rulesOverweight, by="confidence", decreasing=TRUE)
inspect(head(rulesOverweightSorted))
#Insomnia rule
rulesInsomnia<-apriori(data=transactions, parameter=list(supp=0.1, conf=0.1), appearance=list(default="lhs", rhs="Insomnia"), control=list(verbose=F)) 
rulesInsomniaSorted<-sort(rulesInsomnia, by="confidence", decreasing=TRUE)
inspect(head(rulesInsomniaSorted))
