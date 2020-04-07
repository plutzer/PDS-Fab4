###PDS GROUP ASSIGNMENT 4.7###

#reading in data
pantsuits <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
View(pantsuits)

#making a model
model <- lm(water ~ reserved, data=pantsuits)
summary(model)

####Having reserved position for female leadership corresponds with 9.3 more water facilities. 

z.score = qnorm((0.975))
z.score

reservedCoef <- 9.7895
seReserved <- 3.6011 

lowerCI <- reservedCoef - z.score*seReserved
upperCI <- reservedCoef + z.score*seReserved
lowerCI
upperCI
##95% CI of the reserved coefficient estimate:
###(2.73, 16.8)




