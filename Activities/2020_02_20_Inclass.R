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


##ACTIVITY FOR CLASS#####
# Repasting some of the code from the top so I don't have to scroll as much...

rm(list = ls())
library(tidyverse)

mayors<-read_csv(file="https://raw.githubusercontent.com/jmontgomery/jmontgomery.github.io/master/PDS/Datasets/Mayors.csv")
tweets<-read_csv("~/Downloads/Tweets.csv")



#1
#number of tweets that mention police
sum(str_detect(str_to_lower(tweets$Text),pattern = 'police|policing|cops|law enforcement'))
sum(str_detect(str_to_lower(tweets$Text),pattern = 'black lives matter|blm'))

#vector of logicals where these appear
copmatch <- str_detect(str_to_lower(tweets$Text),pattern = 'police|policing|cops|law enforcement')
blmmatch <- str_detect(str_to_lower(tweets$Text),pattern = 'black lives matter|blm')

#2
tweets <- rename(tweets, TwitterHandle=ScreenName)
#mayors$TwitterHandle

# These are datasets of just the tweets we're interested in
blmtweets = tweets %>%
  filter(blmmatch)

copstweets = tweets %>%
  filter(copmatch)

#Now ill count up the occurances of each mayors twitter name in these datasets
# and add them as new columns in the mayors dataset

blmcounts = blmtweets %>%
  count(TwitterHandle)
blmcounts = rename(blmcounts,blmcounts = n)
mayors = mayors %>% 
  left_join(blmcounts,by = 'TwitterHandle')

copscounts = copstweets %>%
  count(TwitterHandle)
copscounts = rename(copscounts,copscounts = n)
mayors = mayors %>%
  left_join(copscounts,by = 'TwitterHandle')

# We now have the blmcounts and copscounts in the mayors dataset...

# Now all we need to do is plot blmcounts and copscounts against the population column

# First getting rid of NA vals

library(tidyr)


mayors = rename(mayors,blm = blmcounts)
mayors = rename(mayors,cop = copscounts)
mayors = rename(mayors,pop = Population)

mayorsblm = drop_na(mayors,blm)
mayorscop = drop_na(mayors,cop)

## Feel free to play around with the plots from here down and see
# if you can make something cool, i just did some linear regressions

library(ggplot2)
## Plot the number of blm tweets against the population, add a linear regression
ggplot(mayorsblm, aes(x = pop,y = blm)) +
  geom_point() +
  #ylim(0,5) + 
  geom_smooth(method = lm)

fit = lm(blm~pop,mayors)
fit

ggplot(mayorscop,aes(x = pop,y = cop)) + 
  geom_point() +
  #xlim(0,500000) +
  #ylim(0,200) +
  geom_smooth(method = lm)

fit = lm(cop~pop,mayors)
fit
