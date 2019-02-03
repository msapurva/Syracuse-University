#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 7 
#lab 7: Viz Map Homework: Median Income
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, March 7, 2018                
#Submit date : Wednesday, March 7, 2018
#---------------------------------------------------


#Download the dataset from blackboard that has median income by zip code (an excel file).

##############################################################################
#Step 1: Load the Data
##############################################################################

#1)	1)	Read the data - using the read.csv function.   


#set working directory to use this command !!
install.packages("gdata")
library(gdata)
myData <- read.csv("MedianZip.csv")
View(myData)
str(myData)

#2)	Clean up the dataframe
#a.	Remove any info at the front of the file that's not needed
#b.	Update the column names (zip, median, mean, population)
Appy <- myData 
TestData <- Appy 
colnames(TestData) <- c("zip","median","mean","population")
View(TestData)

class(TestData$median)
class(TestData$zip)
class(TestData$population)
class(TestData$mean) #It looks like few columns are a 'factor' classtype. Hence need to use as.character before as.numeric. 

TestData$median<- gsub(",","",TestData$median)
TestData$zip<- gsub(",","",TestData$zip)
TestData$population<- gsub(",","",TestData$population)
TestData$mean  <- gsub(",","",TestData$mean)


#3)	Load the 'zipcode' package
install.packages("zipcode")
library(zipcode)

data(zipcode)
str(zipcode)
summary(zipcode)
View(zipcode)


#4)	Use the clean.zipcode function to get your zipcodes in proper format.
TestData$zip <- clean.zipcodes(TestData$zip)
str(TestData)
summary(TestData)

#5) Merge the zip code information from the two data frames (merge into one dataframe)
MergeData <- merge(zipcode,TestData,by.x = 'zip',by.y  = 'zip')
View(MergeData)

#6) Remove Hawaii, Alaska and District of Columbia (DC) (just focus on the 'lower 48' states)
MergeData <- subset(MergeData,MergeData$state != "AK")
MergeData <- subset(MergeData,MergeData$state != "HI")
MergeData <- subset(MergeData,MergeData$state != "DC")
FinalData <- MergeData
View(FinalData)


##############################################################################
#Step 2: Show the income & population per state
##############################################################################
#1)	Create a simpler dataframe, with just the average median income and the population for each state.

avgMedianIncomeDF <- aggregate(as.numeric(FinalData$median), by = list(FinalData$state),FUN= mean)
colnames(avgMedianIncomeDF) <- c("state","AvgmedianIncome")

sumOfPopulationDF <- aggregate(as.numeric(FinalData$population), by = list(FinalData$state),FUN=sum) #sum of population  #other way than using aggregate is using tapply
colnames(sumOfPopulationDF) <- c("state","TotalPopulation")

SimpleDF <- merge(avgMedianIncomeDF,sumOfPopulationDF, by ="state")

#2)	Add the state abbreviations and the state names as new columns (make sure the state names are all lower case) ('openintro' package)
# Manually install package (openintro)
library(openintro)
sort(unique(SimpleDF$state)) 

SimpleDF$State_Name<- abbr2state(SimpleDF$state)
SimpleDF$State_Name <- tolower(SimpleDF$State_Name)
SimpleDF$state <- tolower(SimpleDF$state)
View(SimpleDF) #48 states

#3)	Show the U.S. map, representing the color with the average median income of that state
library("ggmap")
library("ggplot2")

#Create a simple map.
us_map <- map_data("state")
simplemap	<- ggplot()		
simplemap	<- simplemap +	geom_map(data=us_map,aes(map_id=region),map=us_map,fill="white",	color="black")
simplemap <- simplemap+ expand_limits(x= us_map$long, y=us_map$lat)
simplemap

AvgIncomeMap	<- simplemap + 
  geom_map(data=SimpleDF,map=us_map,aes(color=AvgmedianIncome,map_id=State_Name,fill=AvgmedianIncome),na.rm=TRUE,show.legend=TRUE)
AvgIncomeMap <- AvgIncomeMap + labs(x = "Longitude", y= "Latitude")+
  ggtitle("Step 2: Average Median Income Per State")
AvgIncomeMap


#4)	Create a second map with color representing the population of the state
PopulationMap <- simplemap + 
  geom_map(data=SimpleDF,map=us_map,aes( color=TotalPopulation,fill=TotalPopulation,map_id=State_Name),na.rm=TRUE,show.legend=TRUE)
PopulationMap <- PopulationMap + labs(x = "Longitude", y= "Latitude")+
ggtitle("Step 2 : Population Per State")
PopulationMap


##############################################################################
#Step 3: Show the income per zip code
##############################################################################

#1)	Put a dot on a map for each zip code, where the color of the 'dot' is based on the median income. 
#To make the map look appealing, have the background of the map be black.

dotmap <- simplemap + 
  geom_point(data=FinalData,aes(longitude,latitude, color=as.numeric(FinalData$median)),na.rm=TRUE,alpha = 50/100,colour='darkred', show.legend=TRUE,shape=1, size=.1)+
  ggtitle("Step3 : Median Income Per Zip Code")+ theme(
    panel.background = element_rect(fill = "black",
                                    colour = "black",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "black"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "black")
  )
dotmap+labs(x="Longitude",y="Latitude") 

##############################################################################
#Step 4: Show Zip Code Density
##############################################################################
#1)	Now generate a different map, one where we can easily see where there are lots of zip codes, and where there are few (using the 'stat_density2d' function).
zipdensity <- simplemap + 
   stat_density2d(data=FinalData, aes(x=longitude, y=latitude,colour = as.numeric(FinalData$zip), fill=..level.., alpha=..level..),
                  geom='polygon',show.legend = TRUE,na.rm=TRUE)+
  scale_fill_gradient(low="black",high="green")+
  scale_alpha(range=c(0.00,0.70)) 

zipdensity+  ggtitle("Step 4: Zip Code Density") + labs(x="Longitude",y="Latitude")
#zipdensity+ geom_point(data=FinalData, aes(x=longitude, y=latitude,group=zip),colour = "red", alpha=20/100,na.rm=TRUE,shape=1, size=.1,show.legend = TRUE)+



##############################################################################
#Step 5: Zoom in to the region around NYC
##############################################################################
#1)	Repeat steps 3 & 4, but have the image / map be of the northeast U.S. (centered around New York).
NY = geocode("New York")
NYmap = get_map(location = NY, zoom = 6)
gcodeNY <- ggmap(NYmap)
gcodeNY

# Step 3:  Median Income Per Zip Code in and around NY

gcodeNYIncome <- gcodeNY + 
  geom_point(data=FinalData,aes(longitude,latitude, color=as.numeric(FinalData$median)),
             na.rm=TRUE,alpha = 50/100,shape=1, size=.5,colour='darkred',  show.legend=TRUE)+
  ggtitle("Step5a: Median Income Per Zip Code in and around NY state")
gcodeNYIncome +labs(x="Longitude",y="Latitude") 



# Step 4: Plot Density for Zipcode in and around NY
zipMap <- gcodeNY + 
  stat_density2d(data=FinalData, aes(x=longitude, y=latitude, colour = as.numeric(FinalData$zip), fill=..level.., alpha=..level..), 
                 geom='polygon',na.rm=TRUE) +
  scale_fill_gradient(low="black",high="green")+
  scale_alpha(range=c(0.00,0.70))
  zipMap+  ggtitle("Step5b: Plot Density for Zipcode") +
  theme (plot.title=element_text(lineheight=3.5,face="bold"))
 # geom_point(data=FinalData, aes(x=longitude, y=latitude,group=zip), color="red",alpha=1/25,na.rm=TRUE) 




#------------------------end---------------------------------------------------