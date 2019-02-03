#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 4 
#lab 4: Sampling HW
# Note to  grader: All my comments are original.
#-----------------------------
#Due date : Wednesday, February 14, 2018
#Submit date : Wednesday, February 14, 2018
#-----------------------------


##############################################################################
# Step 1: Write a summarizing function to understand the distribution of a vector
##############################################################################

#1.	The function, call it 'printVecInfo' should take a vector as input
#2.	The function should print the mentioned information
#3.	Test the function with a vector that has (1,2,3,4,5,6,7,8,90,91,100). You should see something such as in the assignement question 

myVector <- c(1,2,3,4,5)

printVecInfo <- function (myVector)
{
  # myVector <- as.numeric(unlist(myVector)) #keep this either here in the function or use later with the data set
  Mean.appy <-  (mean (myVector) )
  Median.appy <-  (median(myVector))
  Min.appy <-  ( min(myVector))
  Max.appy <-  (max(myVector))
  StandardDeviation.appy <- (sd(myVector))
  Length.appy <- (length(myVector))
  
  
  cat("mean:",Mean.appy,"\n") #notice the difference between cat and print commands
  print(paste0("median:",Median.appy))
  print(paste0("min:",Min.appy))
  print(paste0( "max:",Max.appy))
  print(paste0("sd:",StandardDeviation.appy))
  print(paste0("length:",Length.appy))
  
}

printVecInfo (myVector)



#new vector : 

newVector <- c(1,2,3,4,5,6,7,8,90,91,100)
printVecInfo (newVector)



############################################################################## 
#Step 2: Explore the airquality dataset
##############################################################################
#4.	Store the 'airquality' dataset into a temporary variable

AQ <- airquality
View (AQ)
summary(AQ) #always do this before u go on to cleaning it
str(AQ)

#5.	Clean the dataset (i.e. remove the NAs)
clean.AQ <- na.omit(AQ) #removing NA's; replace with 0 or 1 incase u dont feel like deleting a lot of stuff
View (clean.AQ) 
row.names (clean.AQ) <- NULL #reindexing


#6.	Sample the ozone within the dataset 10 times, replacing the sampled item after each sample. Use your printVecInfo to see information of the samples. Also generate a histogram of the samples.
Sample1 <- sample (clean.AQ$Ozone,size = 10,replace = TRUE)
printVecInfo (Sample1)
hist(Sample1)

#7.	Sample the ozone again (same as described in the previous step), but this time, compute the mean of the sample.
Sample1 <- mean(sample (clean.AQ$Ozone,size = 10,replace = TRUE))
Sample1


#8.	Now, do the sampling 20 times, using the 'replicate' command. This should generate a list of 20 numbers. Each number is the mean of the 10  ozone samples. Use your printVecInfo to see information of the samples. Also generate a histogram of these means.

MofM.10 <-replicate(20,mean (sample (clean.AQ$Ozone,size = 10,replace = TRUE)))
MofM.10
printVecInfo(MofM.10)
hist(MofM.10)

#9.	Repeat the previous step, but this time, sample the ozone 100 times. You should get 20 numbers, this time each number represents the mean of the 100 ozone samples. Use your printVecInfo to see information of the samples. Also generate a histogram of the samples.

MofM.20 <-replicate(20, mean (sample (clean.AQ$Ozone,size = 100,replace = TRUE)))
MofM.20
printVecInfo(MofM.20)
hist(MofM.20)

#10.	Repeat the previous step, but this time, replicate the sampling 1000 times. You should get 1000 numbers, this time each number represents the mean of the 100 ozone samples. Use your printVecInfo to see information of the samples. Also generate a histogram of the samples.
MofM.1000 <-replicate(1000,mean (sample (clean.AQ$Ozone,size = 100,replace = TRUE)))
MofM.1000
str(MofM.1000)
printVecInfo(MofM.1000)
hist(MofM.1000)


#11.Q -	How does the distribution of the Ozone samples change during each of the previous steps? Which more representative of the actual Ozone values?

#Answer: We notice that :

#a)As we run a statistical process a large number of times, it converges closer to the actual mean of the data set.
#b)If we take 'Law of large numbers' into account, we find that the distribution of sampling means starts to create a bell shaped curve and the center of distribution(the mean) gets close to the actual population mean.



#---------------------------------------end-------------------------------------------------
  