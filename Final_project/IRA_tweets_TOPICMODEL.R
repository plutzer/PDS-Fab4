rm(list = ls())

library(tidyverse)
library(data.table)
library(tm)
library(tidytext)
library(stm)
library(ggplot2)
library(quanteda)
library(qdap)

#Loading in data and re-casting date column
iraTweets <- fread("~/Downloads/eng_only_ira_tweets.csv")
iraTweets$tweet_time = as.Date(iraTweets$tweet_time)

#Splitting the tweets into 8 time categories (to become "documents" later)
group1 = iraTweets[((iraTweets$tweet_time >=  "2016-09-12") & (iraTweets$tweet_time < "2016-09-26")),]
group2 = iraTweets[((iraTweets$tweet_time >=  "2016-09-26") & (iraTweets$tweet_time < "2016-10-07")),]
group3 = iraTweets[((iraTweets$tweet_time >=  "2016-10-07") & (iraTweets$tweet_time < "2016-10-21")),]
group4 = iraTweets[((iraTweets$tweet_time >=  "2016-10-25") & (iraTweets$tweet_time < "2016-11-08")),]
group5 = iraTweets[((iraTweets$tweet_time >=  "2016-11-08") & (iraTweets$tweet_time < "2016-11-22")),]
group6 = iraTweets[((iraTweets$tweet_time >=  "2017-01-16") & (iraTweets$tweet_time < "2017-01-20")),]
group7 = iraTweets[((iraTweets$tweet_time >=  "2017-01-20") & (iraTweets$tweet_time < "2017-02-03")),]

data_list = list(group1,group2,group3,group4,group5,group6,group7) # Assembling a list to make iteration easier later

master_dfm = dfm(" ",groups = "empty") # Intialize a dfm with an empty document
docscount = 0 # To keep track of the total number of documents in the dfm
for (i in 1:length(data_list)) { #This outer loop iterates over the time group divisions
  group_df = data_list[[i]]
  unique_handles = unique(group_df$userid) #Find the unique userid's for this time group
  start = proc.time() #Timing each group for the function. This will print out the time it takes to do each group
  for (j in 1:length(unique_handles)) { #Iterate over each of the unique userid's
    df = group_df[group_df$userid == unique_handles[j]] #Subset the group dataframe down to just the current userid
    ### Initial text cleanup
    doc_string = unlist(str_split(tolower(df$tweet_text),pattern = " ")) #Create a vector of text for this user during this time period
    
    ######## PLAY AROUND WITH FILTERING AND TEXT CLEANING BETWEEN HERE AND THE NEXT BREAK ########
    
    doc_string = gsub("#\\w+ *", "", doc_string) #Remove hashtags
    doc_string = gsub("@\\w+ *", "", doc_string) #Remove user mentions
    doc_string = gsub("t.co\\w+ *", "", doc_string) #Remove twitter condensed links
    doc_string = gsub("tco\\w+ *", "", doc_string) #Remove twitter condensed links
    doc_string = gsub("http\\w+ *", "", doc_string) #Remove links
    doc_string = gsub("[^\x01-\x7F]", "", doc_string) #Remove emojis and non-ascii characters
    doc_string = gsub('[[:digit:]]+', '', doc_string) #Removing numbers... not sure if it's a good idea...
    doc_string = removePunctuation(doc_string)
    doc_string = gsub("tco\\w+ *", "", doc_string) #Remove twitter condensed links
    doc_string = doc_string[!(doc_string %in% stopwords("en"))] #Removing common english stopwords
    doc_string = doc_string[doc_string != ""] #This is necessary because "" elements cause some problems in later steps
    doc_string = doc_string[doc_string != "rt"] #Getting rid of these because I think they are unlikely to be interesting and will slow down computation
    doc_string = doc_string[doc_string != "0"]
    doc_string = doc_string[doc_string != "("] #These are apparently widely used bit I don't know why
    doc_string = doc_string[doc_string != ")"]
    doc_string = gsub("^amp$", "", doc_string) #remove all the random amps
    
    ########### END OF TEXT CLEANING AND FILTERING ###############################
    
    ## Creating frequency matrix element for this document
    doc_corpus = corpus(paste(doc_string,collapse = " "))
    doc_name = paste("g",i,"-",unique_handles[j],sep = "") #Making a name for each doc
    doc_dfm = dfm(doc_corpus,groups = doc_name)
    
    #Attaching this document to the master document-frequency matrix
    master_dfm = rbind(master_dfm,doc_dfm)
  }
  docscount = docscount + length(unique_handles)
  print("Finished group")
  print(proc.time()-start) #Prints out the time elapsed for the text processing for this group
}

####### YOU CAN RUN THIS CODE TO TEST OUT YOUR TEXT CLEANING WITHOUT RUNNING THE TOPIC MODEL #########
# You can also change the range of values for the above for-loops so that the text processing doesn't run on everything.
master_dfm
doc_corpus
doc_string

master_dfm <- dfm(master_dfm, remove = stopwords("english"), stem = TRUE)
topfeatures(master_dfm, 200)
######################################################################################################

#### Once you are more satisfied with your text cleaning, you can run the topic model...
### Keep playing with text cleaning, and the stm parameters and see if you can produce meaningful topics...

#Removing the empty doc we put in the dfm to start
master_dfm = dfm_subset(master_dfm,c(F,rep(T,docscount))) 

## Now run the topic model - Takes ~15 mins
stm = stm(master_dfm,K = 20, verbose = F, init.type = "Spectral")
stm[[5]]

#visualizing the topic model
topTopics <- plot(stm)

wordCloud <- textplot_wordcloud(master_dfm, min_count = 25, random_order = FALSE,
                   rotation = .25,
                   color = RColorBrewer::brewer.pal(8, "Dark2"))
