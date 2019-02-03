#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 2 
#lab 2 & HW 2: Data Frames & Sorting - RowCol-HW-Assignment
#Due date Jan 31, 2018
#Submit date Jan 31,2018
# Note to  grader: All my comments are original.



# Instructions: Explore the mtcars dataset, then put it in a new dataframe. Copy the mtcars dataset into a new variable (called it myCars), so that if you mess up, you can start again very easily (by copying mtcars into myCars again).
#my steps:
myCars <- mtcars
View(myCars)

myCars

str(myCars)
summary(myCars)

###################################################################
# Step 1: Which car has the best HP (hp stands for "horse power")  
#####################################################################

#1)	Is higher or lower HP best? 
#Answer: Higher is better




#2)	Which car has the best hp?
# Method 1: Calculating max hp and then displaying its information by storing it in a data frame
max(myCars$hp)
bestHPCar <- which.max(myCars$hp)
bestHPCar 
myCars[bestHPCar,] 
View(myCars) 
#maserati Bora has the best HP



#Method 2: different way to diplay the car through indexing
which.max(myCars$hp)
myCars[31,] 





#############################################################
# Step 2: Explore mpg (mpg stands for "miles per gallon")   
##############################################################

#3)	What is the highest mpg?
max (myCars$mpg)



#4)	Which car has the highest mpg?

bestMPGCar <- which.max(myCars$mpg)
bestMPGCar
myCars [20, ] 
#Toyota Corolla has the highest mpg




#5)	Create a sorted dataframe, based on mpg 

sortedMpg <- myCars[order(myCars$mpg),]
sortedMpg

#Other methods: head(sortedMpg) # head returns the top 6 (by default, you can specify n)
#create the sorted list (negative sign puts highest first)
  




################################################################
# Step 3: Which car has the "best" combination of mpg and hp?  
################################################################

#6)	What logic did you use?
myCars$mpgPerHP <-myCars$mpg /myCars$hp




#7)	Which car?

bestMpgHP <- which.max(myCars$mpgPerHP)
bestMpgHP
myCars [19,] #the answer is Honda Civic







################################################################################
# Step 4:  Which car has "best" car combination of mpg and hp, 
#where mpg and hp must be given equal weight? 
################################################################################
percentMPG <- 0.5
 
 mpgScaled <- (myCars$mpg - min(myCars$mpg)) /(max(myCars$mpg) - min(myCars$mpg))
 hpScaled <- (myCars$hp - min(myCars$hp)) /(max(myCars$hp) - min(myCars$hp))
 myCars$rating <-mpgScaled*percentMPG + hpScaled*(1.0-percentMPG)
 best4 <- which.max(myCars$rating)
 myCars[best4,] # look for the new column called "rating"
 #create the sorted list (negative sign will put the highest rating first )
 myCars <- myCars[order(-myCars$rating),]
 myCars
 # (self learning) other way to list the top 6 cars:
 head(myCars)
 
 
#####################################end########################################################
 