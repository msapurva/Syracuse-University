#Name: Apurva Sharma
#IST 687
#Homework 1 
#lab 1 - Intro lab Homework
#Due date Jan 24, 2018
#Submit date Jan 24,2018
#Section M006


#define two vectors
height <- c(59,60,61,58,67,72,70) 
weight <- c(150,140,180,220,160,140,130)


#define a variable
a <- 150

#################################################
# Step 1 : calculating means
#################################################

#1)	Compute, using R, the average height (called mean in R)
mean (height)

#2)	Compute, using R, the average weight (called mean in R)
mean (weight)

#3)	Calculate the length of the vector ‘height’ and ‘weight’
length(height)
length(weight)

#4)	Calculate the sum of the heights
sum(height)
sum(weight)

#5)	Compute the average height and weight, by dividing the sum by the length of the vector – how does this compare to the ‘mean’ function?
sum(height)/length(height)
mean(height)
#They compare the same


#################################################
# Step 2 : Using max/min functions
#################################################

#6)	Compute the max height, store the result in ‘maxH’
maxH <- max(height)
maxH

#7)	Compute the min weight, store the results in ‘minW’
minW <- min(weight)
minW

#################################################
# Step 3: Vector Math
#################################################

#8)	Create a new vector, which is the weight + 5 (every person gained 5 pounds)
newWeight <-weight+5
newWeight

#9)	Compute the weight/height for each person, using the new weight just created
newWeight/height


#################################################
# Step 4: Using Conditional if statements
#################################################

#10)	Write the R code to test if max height is greater than 60 (output “yes” or “no”)
if (maxH>60) "yes" else "no"

#11)	Write the R code to if min weight is greater than the variable ‘a’ (output “yes” or “no”)
if(minW > a) "yes" else "no"


