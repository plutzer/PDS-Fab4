rm(list = ls())

library(tidyverse)
library(data.table)
library(tm)
library(tidytext)
library(stm)
library(ggplot2)
library(quanteda)
library(dfm)
library(qdap)

#Loading in data and re-casting date column
iraTweets <- fread("~/PDS/eng_only_ira_tweets.csv")
iraTweets$tweet_time = as.Date(iraTweets$tweet_time)

#Splitting the tweets into 8 time categories (to become "documents" later)
group1 = iraTweets[((iraTweets$tweet_time >=  "2016-09-12") & (iraTweets$tweet_time < "2016-09-26")),]
group2 = iraTweets[((iraTweets$tweet_time >=  "2016-09-26") & (iraTweets$tweet_time < "2016-10-07")),]
group3 = iraTweets[((iraTweets$tweet_time >=  "2016-10-07") & (iraTweets$tweet_time < "2016-10-21")),]
group4 = iraTweets[((iraTweets$tweet_time >=  "2016-10-25") & (iraTweets$tweet_time < "2016-11-08")),]
group5 = iraTweets[((iraTweets$tweet_time >=  "2016-11-08") & (iraTweets$tweet_time < "2016-11-22")),]
group6 = iraTweets[((iraTweets$tweet_time >=  "2017-01-16") & (iraTweets$tweet_time < "2017-01-20")),]
group7 = iraTweets[((iraTweets$tweet_time >=  "2017-01-20") & (iraTweets$tweet_time < "2017-02-03")),]

data_list = list(group1,group2,group3,group4,group5,group6,group7)
dfm_list = vector(mode = "list", length = 7)

for (i in 1:length(data_list)) { #This for-loop takes ~15 minutes
  df = data_list[[i]]
  ### Initial text cleanup
  doc_string = unlist(str_split(tolower(df$tweet_text),pattern = " "))
  doc_string = gsub("#\\w+ *", "", doc_string) #Remove hashtags
  doc_string = gsub("@\\w+ *", "", doc_string) #Remove user mentions
  doc_string = gsub("t.co\\w+ *", "", doc_string) #Remove twitter condensed links
  doc_string = gsub("http\\w+ *", "", doc_string) #Remove links
  doc_string = gsub("[^\x01-\x7F]", "", doc_string) #Remove emojis and non-ascii characters
  doc_string = gsub('[[:digit:]]+', '', doc_string) #Removing numbers... not sure if it's a good idea...
  doc_string = removePunctuation(doc_string)
  doc_string = rm_stopwords(doc_string) #Removing common english stopwords
  doc_string = doc_string[doc_string != ""] #This is necessary because "" elements cause some problems in later steps
  doc_string = doc_string[doc_string != "rt"] #Getting rid of these because I think they are unlikely to be interesting and will slow down computation
  doc_string = doc_string[doc_string != "0"]
  doc_string = doc_string[doc_string != "("] #These are apparently widely used bit I don't know why
  doc_string = doc_string[doc_string != ")"]
  print("Finished initial processing:")
  print(i)
  
  ## Creating frequency matrix element for this document
  doc_corpus = corpus(paste(doc_string,collapse = " "))
  doc_dfm = dfm(doc_corpus)
  dfm_list[[i]] = doc_dfm
}

## Now assembling the document-frequency matrix to be used in the topic model
grouped_dfm = rbind(dfm_list[[1]],dfm_list[[2]],dfm_list[[3]],dfm_list[[4]],dfm_list[[5]],dfm_list[[6]],dfm_list[[7]])

## Now run the topic model - Takes ~15 mins
stm = stm(grouped_dfm, K = 10, verbose = F, init.type = "Spectral")
