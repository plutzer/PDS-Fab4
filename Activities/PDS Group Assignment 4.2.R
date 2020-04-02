####APRIL 2 PDS GROUP ASSIGNMENT######
rm(list=ls())
#reading in the data
library(readr)
senateData<-read_csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")

#making new data
library(tidyverse)
SenateTop<- senateData %>% group_by(RaceID) %>% top_n(1,VotePercentage)
SenateNotTop = senateData[!(senateData$Candidateidentifier %in% SenateTop$Candidateidentifier),]
SenateTop$won = 1
SenateNotTop$won = 0
senateData_all = bind_rows(SenateTop,SenateNotTop)
incumbents = senateData_all[senateData_all$Incumbent > 0,]
incumbents$won

test <- incumbents %>%
  filter(year == 2016)

training <- incumbents %>%
  filter(year != 2016)

training$won

fullModel <- lm(won ~ pvi*Democrat + weightexperience + PercentageRaised + GenericBallotSept, data=training)
library(MASS)
step <- stepAIC(fullModel, direction="both")
step

#fit linear model
linear<-glm(won ~ pvi*Democrat + weightexperience + PercentageRaised, family="binomial", data=training)
summary(linear)
linearPreds<-predict(linear, newdata=test, type="response")

#confusion matrix:
# true neg, false neg
#false pos, true pos
binaryLinearPreds<-(linearPreds>0.5)*1
linearConfusion <- table(binaryLinearPreds, test$won)
linearConfusion

#calculate precision, recall and accuracy of this model
#precision = true pos/(trus pos + false pos)
#recall = true pos/ (tru pos + false neg)
precisionLinear <- 25/(25+0)
precisionLinear
recallLinear <- 25/(25+2)
recallLinear

#fit random forest
library(randomForest)
forest<-randomForest(as.factor(won) ~ pvi*Democrat + weightexperience + PercentageRaised, data=training, ntree=500, mtry=2)
forest # This confusion matrix is "out of bag"
#ignore the given confusion matrix and make my own by
#messing with mtry, ntree, maxnodes

forestPreds<-predict(forest, newdata=test, type="response", nodes=TRUE)
forestPreds
#confusion matrix:
# true neg, false neg
#false pos, true pos
binaryForestPreds<-(as.numeric(forestPreds)>1)*1
forestConfusion <- table(binaryForestPreds, test$won)
forestConfusion

#calculate precision, recall and accuracy of this model
#precision = true pos/(trus pos + false pos)
#recall = true pos/ (tru pos + false neg)
precisionForest <- 26/(26+2)
precisionForest
recallForest <- 26/(26+1)
recallForest


#k nearest neighbors
library(class)
testX <- test[,c("VotePercentage","Republican","pvi","year","weightexperience")]
trainX = training[,c("VotePercentage","Republican","pvi","year","weightexperience")]
nearest = knn(train = trainX, test = testX, cl=training$won)

#confusion matrix
table(nearest,test$won)

#accuracy
accuracyKNN <- 26/(26+1)
accuracyKNN

#recall
recallKNN <- 26/(26+1)
recallKNN







