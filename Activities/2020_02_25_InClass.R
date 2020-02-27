###Group Assignment 2
#Using the tools from this lecture:
#Go to the variables that start with inst, which represent the level of trust that Americans have in different insitutions.
#Write a simple set of code to recode these variables to accurately reflect NAs
#Find the mean level of support for each insitutution
#Find the median level of support for each institution
#Make a plot showing trust in institutions ordering institutiosn from least trusted to most trusted among the American public

#Read Dataset
rm(list=ls())
library(tidyverse)
VSG<-read_csv("~/Downloads/VOTER_Survey_Jan217_Release1-csv.csv")
#Just variables that start with inst
VSG<- VSG %>% 
  select(starts_with("inst_")) 
#Recode NAs
columnnames<-colnames(VSG)
VSG %>%
lapply(colnames(VSG),recode,8 = NA)

  #Similar code from Class
  #map(VSG.fav, mean, na.rm=TRUE)
    #VSG<- VSG %>% 
     # select(fav_sanders_2019, fav_biden_2019) %>%
     # mutate(fav_sanders_binary= na_if(fav_sanders_2019, fav_sanders_2019>4)) %>%
     # mutate(fav_sanders_binary = (fav_sanders_2019<=2)*1) %>%
     # mutate(fav_biden_binary= na_if(fav_biden_2019, fav_biden_2019>4)) %>%
     # mutate(fav_biden_binary = (fav_biden_2019<=2)*1)
   # library(magrittr)
    #VSG %$% 


