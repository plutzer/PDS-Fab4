rm(list = ls())
library(tidyverse)

mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("~/Downloads/Tweets.csv")

#LydaKrewson
lydatweets = tweets %>%
  filter(ScreenName == "lydakrewson")

#Number of tweets
num_tweets = length(str_split(lydatweets$Text, pattern  = " "))

#Mean number of words
mean_words = length(unlist(str_split(lydatweets$Text, pattern  = " ")))/num_tweets[1]

#Number of unique words
num_unique_words = length(unique(unlist(str_split(lydatweets$Text, pattern  = " "))))

#First five characters of unique words
unique_words = unique(unlist(str_split(lydatweets$Text, pattern  = " ")))
sub_words = str_sub(unique_words,1,5)
length(unique(sub_words))

#Number of tweets with 'polic'
sum(str_detect(str_to_lower(lydatweets$Text),pattern = 'polic'))

#Number of tweets with 'police'
sum(str_detect(str_to_lower(lydatweets$Text),pattern = 'police'))

#Number of tweets with a link
sum(str_detect(str_to_lower(lydatweets$Text),pattern = 'http'))

#number of tweets that mention police
sum(str_detect(str_to_lower(tweets$Text),pattern = 'police|policing|cops|black lives matter|blm'))

#looking at those tweets
sum(str_detect(str_to_lower(tweets$Text), pattern = 'police|policing|cops|black lives matter|blm'))





