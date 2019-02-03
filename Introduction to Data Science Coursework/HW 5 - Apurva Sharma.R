#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 5 
#lab 5: JSON & tapply Homework: Accident Analysis
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, February 21, 2018 
#Submit date : Wednesday, February 21, 2018
#---------------------------------------------------


#I am commenting the 'install.packages' so that the code doesnt throw error incase these packages are already installed during evaluation
#NOTE : Please install the following packages incase these libraries are not found in your environment

#install.packages("sqldf") 
library(sqldf)
#install.packages("RCurl")
library(RCurl)
#install.packages("RJSONIO")
library(RJSONIO)
#install.packages("stringr")
library(stringr)

##############################################################################
# Step 1: Load the data
##############################################################################
 
TestFrame <- getURL("https://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD")
JSON <- fromJSON(TestFrame,simplify=FALSE,nullValue = NA)

#metaData = JSON[[1]]
actualData = JSON[[2]]
View(actualData)
numRows = length(actualData)
numRows

#converting json list into dataframe: 
finalData <-data.frame(matrix(unlist(actualData),nrow = numRows,byrow = T),stringsAsFactors = FALSE)

# byrow = T would tell R to fill the elements row wise. 
#stringsAsFactors = FALSE : should character vectors be converted to factors? The ‘factory-fresh’ default is TRUE, but this can be changed by setting options(stringsAsFactors = FALSE).

View(finalData)


##############################################################################
# Step 2: Clean the data
##############################################################################

#Remove first 8 columns 

finalData[,c(1:8)] <- NULL

#Renaming column names:
columnNames <- c("CASE_NUMBER","BARRACK","ACC_DATE","ACC_TIME","ACC_TIME_CODE","DAY_OF_WEEK","ROAD","INTERSECT_ROAD","DIST_FROM_INTERSECT","DIST_DIRECTION","CITY_NAME","COUNTY_CODE","COUNTY_NAME","VEHICLE_COUNT","PROP_DEST","INJURY","COLLISION_WITH_1","COLLISION_WITH_2")
names(finalData) = columnNames
View(finalData)

# Removing NA values
finalData <- na.omit(finalData)


#remove spaces, NA's, etc
numberize <- function (finalData)
  
{
  
  #get rid of commas
  finalData <- gsub(",","",finalData)
  #get rid of spaces
  finalData <- gsub(" ","",finalData)
  #convert to numeric
  return (finalData)
  
}

finalData$DAY_OF_WEEK <- numberize(finalData$DAY_OF_WEEK )


View (finalData)


#splitting time into hour and making a new column for it :

class (finalData$ACC_TIME)
library(stringr)
hour=str_split_fixed(finalData$ACC_TIME, '\\:',2)[,1]
View (hour)
finalData$hour <- hour




##############################################################################
# Step 3: Understand the data using SQL (via SQLDF)
##############################################################################

#1) How many accidents happen on SUNDAY  
sqldf("select count (CASE_NUMBER), DAY_OF_WEEK from finalData where DAY_OF_WEEK = 'SUNDAY'")

#2)How many accidents had injuries (might need to remove NAs from the data)
sqldf("select count (CASE_NUMBER), INJURY from finalData where INJURY = 'YES'")

#3)List the injuries by hour 
sqldf("select count(INJURY),hour  from finalData where INJURY = 'YES' group by hour")


##############################################################################
# Step 4:  Understand the data using tapply
##############################################################################

#1) How many accidents happen on SUNDAY  
#Not using Tapply :
#Acc_on_Sunday <- length(which(finalData$DAY_OF_WEEK=='SUNDAY'))
#Acc_on_Sunday


#Using Tapply :
Acc_on_Sunday <- tapply(finalData$CASE_NUMBER,finalData$DAY_OF_WEEK,length)
Acc_on_Sunday["SUNDAY"]

#2)How many accidents had injuries (might need to remove NAs from the data)
No_of_Injury <- tapply(finalData$CASE_NUMBER,finalData$INJURY,length)
No_of_Injury["YES"]

#3)List the injuries by hour
x <- tapply(finalData$INJURY=='YES',finalData$hour,sum)
x 



#-------------------------end--------------------------------------------



