#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 8 
#lab 8: Linear Modeling Homework - Making Predictions
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, March 21, 2018                
#Submit date : Wednesday, March 21, 2018 
#---------------------------------------------------

##############################################################################
#Step 1: Read in data from the following URL: 
##############################################################################
 # http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls
#This URL will enable you to download the dataset into excel. 
#If you view this in a spreadsheet, you will find that four columns of a small dataset. 
#The first column shows the number of fawn in a given spring (fawn are baby Antelope). 
#The second column shows the population of adult antelope, the third shows the annual precipitation that year, 
#and finally, the last column shows how bad the winter was during that year.
#2.	You have the option of saving the file to your computer and then read it into R, or reading the data directly from the web into a data frame.


#install.packages("gdata")
library(gdata)

excel<- read.xls ("http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls")

#3.	You should inspect the data using the str() command to make sure that all of the cases have been read in (n=8 years of observations) and that there are four variables.  
str(excel)
summary(excel)
View(excel)

 colnames(excel) <- c("number", "population","annual_precipitation","winter_weather")
 
##############################################################################
#4.	Create bivariate plots of the number of baby fawns versus adult antelope population, the precipitation that year, and the severity of the winter. 
#Your code should produce three separate plots. Make sure the X-axis and Y-axis are labeled. Keeping in mind that the number of fawns is the outcome (or dependent) variable, which axis should it go on in your plots?
##############################################################################
 
 plot(excel$population,excel$number,ylab="Number of Baby Fawns", xlab="Adult Antelope population")
 plot(excel$annual_precipitation,excel$number,ylab="Number of Baby Fawns", xlab="precipitation that year")
 plot(excel$winter_weather,excel$number,ylab="Number of Baby Fawns", xlab="severity of winter")

 #the number of fawns is the outcome (or dependent) variable, which axis should it go on in your plots?
 #Answer: Y-axis
 
##############################################################################
 # 5.	Next, create three regression models of increasing complexity using lm(). 
#5a. In the first model, predict the number of fawns from the severity of the winter.
 model1 <- lm(formula=number ~ winter_weather, data=excel)
 summary(model1) #R squared: 0.5459
 abline(model1)

 
#5b.In the second model, predict the number of fawns from two variables (one should be the severity of the winter). 
 model2 <- lm(formula=number ~ winter_weather + annual_precipitation , data=excel)
 summary(model2) #R squared: 0.90
 
 #5c. In the third model predict the number of fawns from all three variables. 
 model3 <- lm(formula=number ~ winter_weather + population+ annual_precipitation , data=excel)
 summary(model3) #R squared = 0.9743
 
#5d. Which model works best? 
 #Answer: 3rd model(model3) as R squared : 97%
 
#5e. Which of the predictors are statistically significant in each model? 
 
 #model1: winter_weather   -0.3379     0.1258  -2.686 0.036263 *  # winter weather is significant 
 
 #model2: winter_weather    0.2269     0.1490   1.522  0.18842   
  #  annual_precipitation   0.6357     0.1511   4.207  0.00843 ** # annual_precipitation is significant
   
 #model3: winter_weather    0.26295    0.08514   3.089   0.0366 * 
 #  population              0.33822    0.09947   3.400   0.0273 * 
  # annual_precipitation    0.40150    0.10990   3.653   0.0217 * 
       
 # In model 3 all three variables are statistically significant- annual_precipitation, population, winter_weather 
 
 
#5f. If you wanted to create the most parsimonious model (i.e., the one that did the best job with the fewest predictors), what would it contain?
 # Doing 7 combinations, to find out which combination gives the best R squared value with the least variables
 
 m1 <- lm(formula=number ~ winter_weather, data=excel)
 summary(m1)
 #Multiple R-squared:  0.5459,
 
 m2 <- lm(formula=number ~ population, data=excel) # highest R squared with least variables 
 summary(m2)
 #Multiple R-squared:  0.8813,
 
 m3 <- lm(formula=number ~ annual_precipitation, data=excel)
 summary(m3)
 #Multiple R-squared:  0.8536
 
 m4 <- lm(formula=number ~ winter_weather+population, data=excel)
 summary(m4)
 #Multiple R-squared:  0.8885
 
 m5 <- lm(formula=number ~ winter_weather+population+ annual_precipitation, data=excel) 
 summary(m5)
 #Multiple R-squared:  0.9743 [This is a very good model if all 3 predictors are used as it has the greatest R squared value closest to 1]
 
 m6 <- lm(formula=number ~ annual_precipitation+winter_weather, data=excel)
 summary(m6)
 #Multiple R-squared:    0.9
 
 m7 <- lm(formula=number ~ annual_precipitation+population, data=excel) #best model with fewest predictors is m7 ! 
 summary(m7) #BEST
 #Multiple R-squared:  0.913,	Adjusted R-squared:  0.8782 &  p-value: 0.002234
 # Answer: BEST model(parsimonious) with FEWEST predictors is m7. 

 
 ##############################################################################  
  
 
 
 
 
 