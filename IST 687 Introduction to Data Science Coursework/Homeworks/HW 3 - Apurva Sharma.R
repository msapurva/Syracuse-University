#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 3 
#lab 3: Statistics and Functions Homework - Cleaning/Munging Dataframes
# Note to  grader: All my comments are original.
#-----------------------------
#Due date : Wednesday, February 7, 2018
#Submit date : Wednesday, February 7, 2018
#-----------------------------

  
##############################################################################
# Step 1: Create a function (named readStates) to read a CSV file into R
##############################################################################
# 1.	Note that you are to read a URL, not a file local to your computer.

readStates <- read.csv ("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv")


# 2.	The file is a dataset on state populations (within the United States).
# 3.	Clean the dataframe - Note that there are many issues that need to be fixed (removing columns, removing rows, changing column names).
# 4.	Within your function, make sure there are 51 rows (one per state + the district of Columbia). Make sure there are only 5 columns with the columns having the following names (stateName, Census, Estimates, Pop2010, Pop2011).
# 5.	Make sure the last four columns are numbers (i.e. not strings)
# 6.	The function should return the cleaned dataframe


readStates <- readStates [9:59,1:5] #removing unwanted columns and rows
View(readStates)
colnames(readStates) <- c("stateName", "Census", "Estimates", "Pop2010", "Pop2011")
colnames(readStates)

readStates$stateName <- gsub("\\.","",readStates$stateName)

numberize <- function (Census)

  {

  #get rid of commas
  Census <- gsub(",","",Census)
  #get rid of spaces
  Census <- gsub(" ","",Census)
  #convert to numeric
  return (as.numeric(Census))
  
}


readStates$Census <- numberize (readStates$Census)
readStates$Estimates <- numberize (readStates$Estimates) 
readStates$Pop2010 <- numberize (readStates$Pop2010)
readStates$Pop2011 <- numberize (readStates$Pop2011)


row.names(readStates) <- NULL #to reindex


###################################################################
# Step 2: Store and Explore the dataset
#####################################################################



#7.	Store  the dataset into a dataframe, called dfStates.  

dfStates <- data.frame(readStates)
View (dfStates)

#8.	Test your dataframe by calculating the mean for the 2010 data, by doing:
 class(dfStates$Pop2010) # just checking class
 
 
mean (dfStates$Pop2010) #answer is 6065298


###################################################################
# Step 3: Find the state with the Smallest Population 
#####################################################################
# 9.	Based on the 2011 data, what is the population of the state with the lowest population? What is the name of that state?

min (dfStates$Pop2011)       #answer : 568158
MinPopStateSname <- dfStates$stateName [which.min(dfStates$Pop2011) ]
MinPopStateSname      #answer : Wyoming



#  10.	Sort the dataframe, in decreasing order, based on the 2011 data. 

sorted_dfStates <- dfStates[ order(- dfStates$Pop2011),]
sorted_dfStates #answer: California on top

###################################################################
# Step 4: Explore the distribution of the states
####################################################################

#11.	Write a function that takes two parameters. The first is a vector and the second is a number.

#12.	The function will return the percentage of the elements within the vector that is higher than the specified value (i.e. the cumulative distribution above the value provided).

#13.	For example, if the vector had 5 elements (1,2,3,4,5), with 2 being the number passed into the function, the function would return 0.6 (since 60% of the numbers were more than 2).


tinyData <- c(1,2,3,4,5)
View (tinyData)
numB <- 2

PerNum <- function (myVector,a)
{
  greater <- myVector>a
  x<- as.numeric(greater)
  myCount <- length(myVector)
  Per <- (sum(x)/myCount)
  return(Per)
  
}

PerNum(tinyData,numB) #answer : 0.6



#14.	Test the function with the vector 'dfStates$Pop2010', and the mean of 'dfStates$Pop2010'.


PerNum (dfStates$Pop2010, mean (dfStates$Pop2010))

#answer : 0.33




