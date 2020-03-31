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
######### Jacob's model
SimpleModelFull<-lm(VotePercentage~pvi*Republican+Incumbent, data=SEN)
summary(SimpleModelFull)$r.squared
install.packages('rsample')
library(rsample)
split_senateData<-initial_split(SEN, prop=.8)
senate_train<-training(split_senateData)
senate_test<-testing(split_senateData)
SimpleModelTrain<-lm(VotePercentage~pvi*Republican+Incumbent, data=senate_train)
SimpleModelPredictions<-predict(SimpleModelTrain, newdata=senate_test)
sqrt(mean((SimpleModelPredictions-senate_train$VotePercentage)^2))
######## Group Model
groupmodel <- lm(VotePercentage ~ Incumbent + experienced + Democrat * pvi, data=SEN)
summary(groupmodel)$r.squared
split_data<-initial_split(SEN, prop=.8)
data_train<-training(split_data)
data_test<-testing(split_data)
groupmodel.train <- lm(VotePercentage ~ Incumbent + experienced + Democrat * pvi, data=data_train)
groupmodel.pred <- lm(VotePercentage ~ Incumbent + experienced + Democrat * pvi, data=data_test)
GroupModelPredictions<-predict(groupmodel.pred, newdata=data_test)
sqrt(mean((GroupModelPredictions-data_test$VotePercentage)^2))
