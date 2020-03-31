#Group Assignment
#For Class-Discussion-03-27

#Group assignment
#Make your own model to predict senate races.
#How does your model do in an out-of sample test compared to my simple model above? Use RMSE. And remember that smaller values of RMSE are better.
#Now go get the data at this link and use it to make predictions for 2018 (you will need to change some of the variable names)
#http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel2018.csv
#Use this data to make predicitons for 2018. Add into slack your predictions for: 2018MOMcCaskill, 2018OHBrown, and 2018WVManchin. Please put your predictions for these races up BEFORE the next scheduled class session.

rm(list = ls())
library(tidyverse)

########## Predicting 2016
SEN <-read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
View(SEN)
######### Jacob's model
SimpleModelFull<-lm(VotePercentage~pvi*Republican+Incumbent, data=SEN)
summary(SimpleModelFull)$r.squared
library(rsample)
split_senateData<-initial_split(SEN, prop=.8)
senate_train<-training(split_senateData)
senate_test<-testing(split_senateData)
SimpleModelTrain<-lm(VotePercentage~pvi*Republican+Incumbent, data=senate_train)
SimpleModelPredictions<-predict(SimpleModelTrain, newdata=senate_test)
sqrt(mean((SimpleModelPredictions-senate_test$VotePercentage)^2))

######## Group Model
  #a start
  groupmodel <- lm(VotePercentage ~ Incumbent + experienced + pvi + Democrat, data=SEN)
  summary(groupmodel)
  summary(groupmodel)$r.squared

  #let's include all predictors
  fullModel <- lm(VotePercentage ~ Incumbent + experienced + pvi + Democrat + weightexperience + PercentageRaised, data=SEN)
  summary(fullModel)  
  summary(fullmodel)$r.squared
  #including the extra variables reduced the residual standard error and increase R squared
  
  #let's let the computer do stepwise regression to pick our variables
  #this leaves one variable out at a time and lets R optimize the model
  library(MASS)
  step <- stepAIC(fullModel, direction="both")
  step
  #this analysis would suggest that we only use Incumbent + weightexperience + PercentageRaised as predictors
  
  #trying out the optimized regression:
  stepModel <- lm(VotePercentage ~ Incumbent + weightexperience + PercentageRaised, data=SEN)
  summary(stepModel)
  
  #lets cross validate to see if this actually predicts the data very well at all
  split_data<-initial_split(SEN, prop=.8)
  data_train<-training(split_data)
  data_test<-testing(split_data)
  groupmodel.train <- lm(VotePercentage ~ Incumbent + weightexperience + PercentageRaised, data=data_train)
  groupmodel.pred <- lm(VotePercentage ~ Incumbent + weightexperience + PercentageRaised, data=data_test)
  GroupModelPredictions<-predict(groupmodel.pred, newdata=data_test)
  sqrt(mean((GroupModelPredictions-data_test$VotePercentage)^2))
  #yay, we reduced MSE
  
###Predicting Senate Races
  #we can use the general model we found above to try to predict the outcome of Senate races
  
  #reading in new data
  sen18 <- read.csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel2018.csv")
  View(sen18)
  
  #lets try to predict the races using the regression from above
  #the regression equation we found is:
  #30.95772 + 2.40866(Incumbent) + 1.29763(weightexperience) + 0.26986(Percentage Raised)
  #predict our values based on that
  
  #2018MOMcCaskill
  predMO <- 30.95772+2.40866*sen18$Incumbent[15]+1.29763*sen18$Weightedexperience[15]+0.26986*sen18$Percentage.Raised[15]
  predMO
  
  #2018OHBrown
  predOH <- 30.95772+2.40866*sen18$Incumbent[8]+1.29763*sen18$Weightedexperience[8]+0.26986*sen18$Percentage.Raised[8]
  predOH
  
  #2018WVManchin
  predWV <- 30.95772+2.40866*sen18$Incumbent[19]+1.29763*sen18$Weightedexperience[19]+0.26986*sen18$Percentage.Raised[19]
  predWV
  
  #real values (from internet)
  realMO <- 45.6
  realOH <- 53.4
  realWV <- 49.6
  
  #rmse
  sqrt(mean((predMO-realMO)^2+(predOH-realOH)^2+(predWV-realWV)^2))
  
  
  
