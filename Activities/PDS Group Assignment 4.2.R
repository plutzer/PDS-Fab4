####APRIL 2 PDS GROUP ASSIGNMENT######

#reading in the data
library(readr)
senateData<-read_csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
View(senateData)
dim(senateData)

#making new data
SenateTop<- senateData %>% group_by(RaceID) %>% top_n(1,VotePercentage)
View(SenateTop)

SenateTop <- SenateTop %>%
  filter(Incumbent > 0)

SenateTop$won <- 1
View(SenateTop)  

winners <- SenateTop$Candidateidentifier

senators <- full_join(SenateTop, senateData, by = "Candidateidentifier")
senators[is.na(senators$won),]$won <- 0
senators$won
colnames(senators)

#filtering down to just incumbents
incumbents <- senators %>%
  filter(Incumbent.y == 1)

test <- incumbents %>%
  filter(year.y == 2016)

training <- incumbents %>%
  filter(year.y != 2016)

colnames(training)

fullModel <- lm(won ~ pvi.y*Democrat.y + weightexperience.y + PercentageRaised.y + GenericBallotSept.y, data=training)
library(MASS)
step <- stepAIC(fullModel, direction="both")
step

dim(training)
#fit linear model
linear<-glm(won ~ pvi.y*Democrat.y + weightexperience.y + PercentageRaised.y, family="binomial", data=training)
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
forest<-randomForest(as.factor(won) ~ pvi.y*Democrat.y + weightexperience.y + PercentageRaised.y, data=training, ntree=700, mtry=1, importance=TRUE)
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


#k nearest neighbors
library(class)
trainingX<-training[,c("pvi.y", "Democrat.y", "PercentageRaised.y", "weightexperience.y")]
trainingX$inc<-turnoutX$inc+rnorm(length(turnoutX$inc), 0, .001)
mod1_knn<-knn(turnoutX, test=turnoutX, cl=turnout$turnout, k=10)
table(mod1_knn, turnout$turnout)












