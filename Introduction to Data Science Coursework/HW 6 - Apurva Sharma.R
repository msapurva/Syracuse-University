#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 6 
#lab 6: Viz Homework:  air quality Analysis
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, February 28, 2018 
#Submit date : Wednesday, February 28, 2018
#---------------------------------------------------
install.packages("ggplot2")
library(ggplot2)

##############################################################################
# Step 1: Load the data
##############################################################################
View(airquality) 
AirQuality <- airquality
View(AirQuality) #see which columns need NA removal

##############################################################################
# Step 2: Clean the data
##############################################################################
#remove NA's from columns that have NA's
AirQuality1 <- AirQuality[!is.na(AirQuality$Ozone) & !is.na(AirQuality$Solar.R) & !is.na(AirQuality$Wind),  ]
View (AirQuality1)

##############################################################################
# Step 3: Understand the data distribution
##############################################################################

#Create the following visualizations:
#1)	Histograms for each of the variables 

# Plot = Data+Aesthetics+Geometry 
ggplot(AirQuality1, aes(x=Ozone))+ geom_histogram(bins=5,color="black", fill="green")+ ggtitle("Histogram for Ozone")
ggplot(AirQuality1, aes(x=Solar.R))+ geom_histogram(bins=5,color="black", fill="orange")+ ggtitle("Histogram for Solar.R")
ggplot(AirQuality1, aes(x=Wind))+ geom_histogram(bins=5,color="black", fill="blue")+ ggtitle("Histogram for Wind")
ggplot(AirQuality1, aes(x=Temp))+ geom_histogram(bins=5,color="black", fill="red")+ ggtitle("Histogram for Temp")
ggplot(AirQuality1, aes(x=Month))+ geom_histogram(bins=5,color="black", fill="purple")+ ggtitle("Histogram for Month")
ggplot(AirQuality1, aes(x=Day))+ geom_histogram(bins=5,color="black", fill="pink")+ ggtitle("Histogram for Day")

#2) Boxplot for Ozone, and boxplots for different wind values (round the wind to get a good number of "buckets"
                                                             
#Boxplot for Ozone
ggplot(AirQuality1,aes(x=factor(0),Ozone)) + geom_boxplot(color="black", fill="green") +ggtitle("Box Plot for ozone")

#Box plots for different wind values (round the wind to get a good number of "buckets"
AirQuality1$RoundedWind <- round(AirQuality1$Wind) #rounding wind values
ggplot(AirQuality1, aes(x=factor(RoundedWind),Wind)) + geom_boxplot(color="black", fill="blue")+ggtitle("Box Plots for diff Wind values")

##############################################################################
# Step 4: Explore how the data changes over time
##############################################################################

#First, make sure to create appropriate dates (this data was from 1973)
AirQuality1$Date <- as.Date(ISOdate("1973", AirQuality1$Month, AirQuality1$Day)) #view new column Date

# Single Line chart for a single variable:
ggplot(AirQuality1, aes(x=Date, y=Ozone))+ geom_line(color= "green")+ ggtitle("Line Chart for ozone")
ggplot(AirQuality1, aes(x=Date,y=Solar.R))+ geom_line(color= "orange")+ ggtitle("Line Chart for Solar")
ggplot(AirQuality1, aes(x=Date,y=Wind))+ geom_line(color= "blue")+ ggtitle("Line Chart for Wind")
ggplot(AirQuality1, aes(x=Date,y=Temp))+ geom_line(color= "red")+ ggtitle("Line Chart for Temp")

# Single Line chart for multiple variables (diff colors):
ggplot(AirQuality1, aes(x= Date, y= Variables))+ 
  geom_line(aes(y = Ozone, color = "Ozone")) +
  geom_line(aes(y = Temp, color = "Temp")) + 
  geom_line(aes(y = Wind, color = "Wind")) +
  geom_line(aes(y = Solar.R, color = "Solar.R"))+
  scale_colour_manual(values=c("green","orange","red","blue"))+      #arrange these carefully to pick up the variables alphabetically !
  ggtitle("Single Line chart for multiple variables (diff colors)")

##############################################################################
# Step 5: Look at all the data via a Heatmap
##############################################################################

#Professor feedback : 
#For number 5, I wanted to make sure you could get the idea of scaling.  if one variable goes from 1-5 and another goes form 40 - 1000, they will look weird on a visualization together, as you note in your #7.  Sometimes you have to leave them and just re-scale the values...  make them more similar, but keeping their validity.  A simple example of this is we could of multiplied our smallest scale (wind) by 10... would have made the values more similar for all three.

install.packages("reshape2")
library(reshape2)
install.packages("scales")
library(scales)
install.packages("plyr")
library(plyr)

hot_air <- airquality 

for(i in 1:ncol(hot_air)) #function to handle nasty NA's
  {
  hot_air[is.na(hot_air[,i]), i] <- mean(hot_air[,i], na.rm = TRUE)
  }

hot_air$Date <- as.Date(ISOdate("1973", hot_air$Month, hot_air$Day)) #view new column Date


molten_air <- melt(hot_air, id.vars = c("Date","Day","Month")) #Basically,  the data is "melt"  so that each row has a unique id-variable combination. Then this melted data is "cast" into a shape using The Reshape Package.
View(molten_air)

molten_air<- ddply(molten_air, .(variable), transform, rescale = rescale(value)) #rescaling
ggplot(molten_air, aes(Date, variable)) + geom_tile(aes(fill=rescale))


##############################################################################
# Step 6: Look at all the data via a scatter chart
##############################################################################
ggplot(AirQuality1,aes(x=Wind,y=Temp)) + geom_point(aes(size=Ozone,color="Solar.R"))


##############################################################################
# Step 7: Final Analysis
##############################################################################
# Q- Do you see any patterns after exploring the data?  
# A- From the scatter chart it seems that amount of ozone at high temperatures is more at low windspeeds and it decreases with increase in speed of wind or low temperatures.

# Q- What was the most useful visualization?
# A- It seems that individual histograms and individual line plots are not ideal to gain an overall picture of multiple variables in a dataset, as the x and y axis scales are different for each variable of airquality dataset. 
#Hence, I feel that with a Single Line plot with 'multiple variables line plots using diff colors' is the most useful plot in this scenario as it provides better visualization of data.


#-----------------------end---------------------------



