rm(list = ls())
library(tidyverse)
## Taylor's code
#takes about 15 minutes to load in
library(data.table)
iraTweets <- fread("~/PDS/eng_only_ira_tweets.csv")

iraTweets$tweet_time = as.Date(iraTweets$tweet_time)
before_prim = iraTweets[iraTweets$tweet_time < "2016-07-21",]
after_prim = iraTweets[iraTweets$tweet_time >= "2016-07-21"]

before_corpus = unlist(str_split(removePunctuation(tolower(before_prim$tweet_text)),pattern = " "))
before_counts = count(as_tibble(before_corpus),value,sort = T)

test = dfm(before_corpus)
topic_model_before = stm(test,K = 6,verbose = F, init.type = "Spectral")

library(quanteda)
library(dfm)
before_dfm = before_corpus %>%
  count(sort = T)

unique_dates = unique(substr(iraTweets$tweet_time,1,10))
dates_frame = data.frame(date = unique_dates)

calc_word_freq = function(date,words) { ## words must be lowercase
  subsetted_ira = iraTweets[substr(tweet_time,1,10) == date,]
  corpus = unlist(str_split(removePunctuation(tolower(subsetted_ira$tweet_text)),pattern = " "))
  total_words = length(corpus)
  interest_words = sum(corpus %in% words)
  interest_words/total_words
}

add_freqs = function(testwords,column_title) {
  #freqs = rep(1,length(unique_dates))
  freqs = sapply(unique_dates,calc_word_freq,words = testwords)
  dates_frame[,column_title] = freqs
  dates_frame
}

#####
dates_frame = add_freqs(c("trump","donald"),'trumpwords')
cframe = dates_frame
cframe$date = as.Date(cframe$date)

library(ggplot2)
ggplot(cframe, aes(x = date, y = cframe$trumpwords)) +
  geom_point()

##### Testing
library(stm)
example_words = c("trump","clinton")
example_date = "2015-07-11"
calc_word_freq(example_date,example_words)

sapply(unique_dates,calc_word_freq,words = c("trump","clinton"))
