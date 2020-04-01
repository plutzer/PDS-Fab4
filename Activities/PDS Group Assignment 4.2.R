####APRIL 2 PDS GROUP ASSIGNMENT######

#reading in the data
library(readr)
senateData<-read_csv("http://politicaldatascience.com/PDS/Datasets/SenateForecast/CandidateLevel.csv")
View(senateData)
dim(senateData)

#making new data
SenateTop<- senateData %>% group_by(RaceID) %>% top_n(1,VotePercentage)
View(SenateTop)

SenateBottom <- senateData %>% group_by(RaceID %>% )

SenateTop <- SenateTop %>%
  filter(Incumbent > 0)

SenateTop$won <- 1
View(SenateTop)  

winners <- SenateTop$Candidateidentifier

senators <- full_join(SenateTop, senateData, by = "Candidateidentifier")
senators[is.na(senators$won),]$won <- 0
senators$won
