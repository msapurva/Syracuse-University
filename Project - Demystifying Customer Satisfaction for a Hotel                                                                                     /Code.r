#IST 687 - Applied Data Science
#Final Project : Hyatt NPS Data Modelling with R
#By- Apurva Sharma
#-----------------------------------------------

#setwd("~/Downloads/IST687-data/Total_Hyatt")

#Packages used:
# install.packages("data.table")
# install.packages("modeest")
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("plyr")
# install.packages("rworldmap")
# install.packages("NPS")
# install.packages("wordcloud")
# install.packages("tm")
# install.packages("treemap")
# install.packages("countrycode")
# install.packages("lubridate")
# install.packages("arules")
# install.packages("arulesViz")
# install.packages("e1071")
# install.packages("kernlab")
# install.packages("openintro")
# install.packages("zipcode")
# install.packages("memisc")
# install.packages("openintro")
# install.packages("rminer")
# install.packages("ranger")
# install.packages("Boruta")
# install.packages("corrplot")


library('data.table')
library(modeest)
library(ggplot2)
library(ggmap)
library(plyr)
library(rworldmap)
library(NPS)
library(wordcloud)
library(tm)
library(treemap)
library(countrycode)
library(lubridate)
library(arules)
library(arulesViz)
library(e1071)
library(kernlab)
library(openintro)
library(zipcode)
library(memisc)
library(openintro)
library(rminer)
library(ranger)
library(Boruta)
library(corrplot)

##########################################################################################
# Data Import and Preprocessing
##########################################################################################

# Selecting Columns
#Choosing the columns from dataset(didnt choose columns with more than 90% blank columns and chose only 43 columns:)
columns<-c("POV_CODE_C", "Age_Range_H", "Gender_H", "Likelihood_Recommend_H", "Overall_Sat_H", "Guest_Room_H", "Tranquility_H",
           "Condition_Hotel_H", "Customer_SVC_H", "Staff_Cared_H", "Internet_Sat_H", "Check_In_H","Golf_PL",
           "City_PL", "State_PL", "Postal Code_PL","Country_PL", "Property Latitude_PL", "Property Longitude_PL",
           "Brand_PL", "Business Center_PL", "Convention_PL", "Limo Service_PL", "Pool-Indoor_PL", "Mini-Bar_PL",
           "Pool-Outdoor_PL", "Resort_PL", "Shuttle Service_PL", "Spa_PL", "Valet Parking_PL", "NPS_Type", "CHECK_IN_DATE_C",
           "CHECK_OUT_DATE_C", "ENTRY_TIME_R","NUM_ROOMS_R", "LENGTH_OF_STAY_R", "ADULT_NUM_R", "CHILDREN_NUM_R", "Booking_Channel","ROOM_TYPE_DESCRIPTION_C")

#--------------
# Data Import
# We used `fread` for every row to have the same number of columns.
Feb14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201402.csv", header=TRUE, select=columns, verbose=TRUE)
Mar14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201403.csv", header=TRUE, select=columns, verbose=TRUE)
Apr14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201404.csv", header=TRUE, select=columns, verbose=TRUE)
May14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201405.csv", header=TRUE, select=columns, verbose=TRUE)
Jun14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201406.csv", header=TRUE, select=columns, verbose=TRUE)
Jul14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201407.csv", header=TRUE, select=columns, verbose=TRUE)
Aug14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201408.csv", header=TRUE, select=columns, verbose=TRUE)
Sep14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201409.csv", header=TRUE, select=columns, verbose=TRUE)
Oct14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201410.csv", header=TRUE, select=columns, verbose=TRUE)
Nov14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201411.csv", header=TRUE, select=columns, verbose=TRUE)
Dec14 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201412.csv", header=TRUE, select=columns, verbose=TRUE)
Jan15 <- fread(file="/Users/apurva_sharma/Downloads/IST687-data/Total_Hyatt/out-201501.csv", header=TRUE, select=columns, verbose=TRUE)

#--------------
#Combining to one dataset
full_Test_dataset<-rbind(Feb14,Mar14,Apr14,May14,Jun14,Jul14,Aug14,Sep14,Oct14,Nov14,Dec14,Jan15)

colnames(full_Test_dataset)<- c('POV_CODE_C', 'Age_Range_H', 'Gender_H', 'Likelihood_Recommend_H', 'Overall_Sat_H', 'Guest_Room_H', 'Tranquility_H',
                                'Condition_Hotel_H', 'Customer_SVC_H', 'Staff_Cared_H', 'Internet_Sat_H', 'Check_In_H','Golf_PL',
                                'City_PL', 'State_PL', 'PostalCode_PL','Country_PL', 'PropertyLatitude_PL', 'PropertyLongitude_PL',
                                'Brand_PL', 'BusinessCenter_PL', 'Convention_PL', 'LimoService_PL', 'PoolIndoor_PL', 'MiniBar_PL',
                                'PoolOutdoor_PL', 'Resort_PL', 'ShuttleService_PL', 'Spa_PL', 'ValetParking_PL', 'NPS_Type', 'CHECK_IN_DATE_C',
                                'CHECK_OUT_DATE_C', 'ENTRY_TIME_R','NUM_ROOMS_R', 'LENGTH_OF_STAY_R', 'ADULT_NUM_R', 'CHILDREN_NUM_R', 'Booking_Channel','ROOM_TYPE_DESCRIPTION_C')

##-----------------------start------------------------------------------------------------------------------------
#write.csv(full_Test_dataset,"full_Test_dataset.csv",row.names=FALSE)  #writing combined dataset into a csv file

full_Test_dataset <- read.csv("full_Test_dataset.csv") #reading the clean hotel data file


# Handling Missing Values
# -----------------------
chunk<- full_Test_dataset
# Removing Blanks - [A good idea is to set all of the "" (blank cells) to NA before any further analysis.]
chunk[chunk==""] <- NA

chunk<-chunk[!(is.na(chunk$NPS_Type)),]
row.names(chunk)<-NULL
chunk<-chunk[!(is.na(chunk$Likelihood_Recommend_H)),]
row.names(chunk)<-NULL
chunk<-chunk[!(is.na(chunk$Country_PL)),]
row.names(chunk)<-NULL

full_cleanData<- chunk
summary(full_cleanData)
str(full_cleanData)


#################################
# Project Scope
#################################

# World Level Analysis
# --------------------
full_cleanData1viz1<- full_cleanData

# LTR data for each Country
Country<- count(full_cleanData1viz1$Country_PL) #56 countries
Country_LTR<- tapply(full_cleanData1viz1$Likelihood_Recommend_H, full_cleanData1viz1$Country_PL, mean, na.rm = TRUE)
LTR_df<- data.frame(Country_LTR,Country)
colnames(LTR_df)<- c("Country_LTR","Country_Name","No.ofobs")
LTR_df
#A few facts which can be observed from this plot have been stated below:
# - Poland stands out from the list of the countries and has the highest LTR of 9.64
# - Jamaica has the lowest LTR of 6.22

#NPS data for each country
#Get total number of observations in country
TotalbyCountry<- tapply(factor(full_cleanData1viz1$NPS_Type),full_cleanData1viz1$Country_PL,length)
TotalbyCountry<- to.data.frame(TotalbyCountry,as.vars=0,name="Freq")
colnames(TotalbyCountry)<- c("Country_name","TotalFreq")

#Get number of promoters by country
full_cleanData_PromoterOnly<-(full_cleanData1viz1[which(full_cleanData1viz1$NPS_Type=="Promoter"),])
PromoterTotalbyCountry<- tapply(factor(full_cleanData_PromoterOnly$NPS_Type),(full_cleanData_PromoterOnly$Country_PL),length)
PromoterTotalbyCountry<- to.data.frame(PromoterTotalbyCountry,as.vars=0,name="Freq")
colnames(PromoterTotalbyCountry)<- c("Country_name","PromFreq")

#Get number of detractors by country
full_cleanData_DetractorOnly<-(full_cleanData1viz1[which(full_cleanData1viz1$NPS_Type=="Detractor"),])
DetractorTotalbyCountry<- tapply(factor(full_cleanData_DetractorOnly$NPS_Type),full_cleanData_DetractorOnly$Country_PL,length)
DetractorTotalbyCountry<- to.data.frame(DetractorTotalbyCountry,as.vars=0,name="Freq")
colnames(DetractorTotalbyCountry)<- c("Country_name","DetractorFreq")

#Join the promoters and detractors using country
PromDet_byCountry<-join(PromoterTotalbyCountry,DetractorTotalbyCountry,by="Country_name")
colnames(PromDet_byCountry)<- c("Country_name","Promoter_Freq","Detractor_Freq")

#Join TotalbyCountry and PromDet_byCountry
FullNPSData_byCountry<-join(TotalbyCountry,PromDet_byCountry,by="Country_name")
FullNPSData_byCountry[is.na(FullNPSData_byCountry)]<-0
FullNPSData_byCountry$NPS_Score <- (FullNPSData_byCountry$Promoter_Freq - FullNPSData_byCountry$Detractor_Freq)/FullNPSData_byCountry$TotalFreq *100

# Merge the two data frames(LTR and NPS dataframes)
TotalNPSdata<- merge(LTR_df,FullNPSData_byCountry,by.x = 'Country_Name',by.y  = 'Country_name')
TotalNPSdata$NPCformula<- npc(round(TotalNPSdata$Country_LTR), breaks = list(0:6, 7:8, 9:10)) #NPC formula
#View(TotalNPSdata)
# - Total of 56 countries were found to have their NPS score more than their Goal Values(Goal NPS=53)
# - The average NPS throughout all countries is 57.7

#Plots
#loading ISO3 codes for countries from package 'countryExData'
data(countryExData)
Merged_Total_Dataframe <- merge(TotalNPSdata,countryExData,by.x = 'Country_Name',by.y  = 'Country')
sPDF <- joinCountryData2Map(Merged_Total_Dataframe, joinCode = "ISO3", nameJoinColumn = "ISO3V10", verbose=TRUE)
mapCountryData(sPDF, nameColumnToPlot="NPS_Score",catMethod='fixedWidth',mapTitle = "Net Promoter Score by Country") #NPS World map

# Population Data and plot
CountryTreeMap<- treemap(TotalNPSdata,index = c("Country_Name"),vSize="TotalFreq",type="index",
                         palette = "Dark2",title = "Number of Reservations by Country",fontsize.title = 14,
                         fontsize.labels = 12,border.col = "white")




# United States Analysis
# ----------------------

#Data preparation
usdata<-full_cleanData[full_cleanData$Country_PL=="United States",] #744959 obs. of  40 variables
#percentage of US
dim(usdata)[1]/dim(full_cleanData)[1]*100
#[1] 80.5903


# Brand level Analysis
# Which Hyatt hotel brand do different customer prefer? 
# What is the count of customers who visited hotels (hotel brand wise)?
# US hotel Hyatt brands count vs NPS_Type
tempBrandPLData<-data.frame(table(usdata$Brand_PL))
colnames(tempBrandPLData)<-c("Brand_PL","BrandStrength")
usdata2 <-merge(usdata,tempBrandPLData,by="Brand_PL")

BrandPlot <- ggplot(usdata2, aes(x = reorder(Brand_PL,-BrandStrength))) +
  geom_bar(aes(fill=NPS_Type), position="dodge") +
  scale_fill_manual(values=c("red", "green","blue")) +
  theme(axis.text.x = element_text(angle=60, hjust=1))+
  labs(x="Brand > NPS_Type",y="No. of Reservations")+
  ggtitle("Brands-NPS_Type by Number of Reservations")
BrandPlot



# California Analysis
# --------------------
CaliforniaData <- usdata[which(usdata$State_PL =="California"),]

# Data
CaliforniaData1<- CaliforniaData

#------------
# Brand Data Preparation
# ----------------------
#splitting CalifoniaData into Hyatt Brands:
California_Regency<-CaliforniaData1[CaliforniaData1$Brand_PL=="Hyatt Regency",]

#######################
# Model based analysis
#######################


# Data Preparation
# ----------------
#(Removing unneeded columns and super biased columns with mostly NA's or Yes's or No's)
Cal_Regency_modelling <- subset(California_Regency,select = -c(Brand_PL,State_PL,Country_PL,PostalCode_PL, #53762 obs. of  34 variables
                                                               PropertyLatitude_PL,PropertyLongitude_PL))

# Early checkin age. Early check-in is defined as people checking in from 7AM-2PM (inclusive) as usual chek-in is at 3PM
Cal_Regency_modelling$ENTRY_TIME_R <- as.integer(substr(Cal_Regency_modelling$ENTRY_TIME_R, 1, 2))

tempColByAP<-
  ifelse((Cal_Regency_modelling$ENTRY_TIME_R>=7) & (Cal_Regency_modelling$ENTRY_TIME_R<14) , "Early_Checkin",
         ifelse((Cal_Regency_modelling$ENTRY_TIME_R>=14) & (Cal_Regency_modelling$ENTRY_TIME_R< 15 ), "Usual_Checkin",
                "Late_Checkin"
         ))
Cal_Regency_modelling$ENTRY_TIME_R<-tempColByAP


Cal_Regency_modelling$CHECK_IN_DATE_C <- as.Date(Cal_Regency_modelling$CHECK_IN_DATE_C)
Cal_Regency_modelling$CHECK_IN_DATE_C<- wday(Cal_Regency_modelling$CHECK_IN_DATE_C, label=TRUE)

Cal_Regency_modelling$CHECK_OUT_DATE_C <- as.Date(Cal_Regency_modelling$CHECK_OUT_DATE_C)
Cal_Regency_modelling$CHECK_OUT_DATE_C<- wday(Cal_Regency_modelling$CHECK_OUT_DATE_C, label=TRUE)


Cal_Regency_modelling$ENTRY_TIME_R<-as.factor(Cal_Regency_modelling$ENTRY_TIME_R)
Cal_Regency_modelling$CHECK_IN_DATE_C<-as.factor(Cal_Regency_modelling$CHECK_IN_DATE_C)
Cal_Regency_modelling$CHECK_OUT_DATE_C<-as.factor(Cal_Regency_modelling$CHECK_OUT_DATE_C)

str(Cal_Regency_modelling) #53762 obs. of  33 variables:
summary(Cal_Regency_modelling)

#NA HANDLING :(Removing unneeded columns and super biased columns)
#Determine if columns have any NA's

#replacing NA's with mean values
Cal_Regency_modelling$Guest_Room_H[is.na(Cal_Regency_modelling$Guest_Room_H)] <- round(mean(Cal_Regency_modelling$Guest_Room_H, na.rm = TRUE))
Cal_Regency_modelling$Overall_Sat_H[is.na(Cal_Regency_modelling$Overall_Sat_H)] <- round(mean(Cal_Regency_modelling$Overall_Sat_H, na.rm = TRUE))
Cal_Regency_modelling$Tranquility_H[is.na(Cal_Regency_modelling$Tranquility_H)] <- round(mean(Cal_Regency_modelling$Tranquility_H, na.rm = TRUE))
Cal_Regency_modelling$Condition_Hotel_H[is.na(Cal_Regency_modelling$Condition_Hotel_H)] <- round(mean(Cal_Regency_modelling$Condition_Hotel_H, na.rm = TRUE))
Cal_Regency_modelling$Customer_SVC_H[is.na(Cal_Regency_modelling$Customer_SVC_H)] <- round(mean(Cal_Regency_modelling$Customer_SVC_H, na.rm = TRUE))
Cal_Regency_modelling$Staff_Cared_H[is.na(Cal_Regency_modelling$Staff_Cared_H)] <- round(mean(Cal_Regency_modelling$Staff_Cared_H, na.rm = TRUE))
Cal_Regency_modelling$Internet_Sat_H[is.na(Cal_Regency_modelling$Internet_Sat_H)] <- round(mean(Cal_Regency_modelling$Internet_Sat_H, na.rm = TRUE))
Cal_Regency_modelling$Check_In_H[is.na(Cal_Regency_modelling$Check_In_H)] <- round(mean(Cal_Regency_modelling$Check_In_H, na.rm = TRUE))
Cal_Regency_modelling$LENGTH_OF_STAY_R[is.na(Cal_Regency_modelling$LENGTH_OF_STAY_R)] <- round(mean(Cal_Regency_modelling$LENGTH_OF_STAY_R, na.rm = TRUE))
Cal_Regency_modelling$ADULT_NUM_R[is.na(Cal_Regency_modelling$ADULT_NUM_R)] <- round(mean(Cal_Regency_modelling$ADULT_NUM_R, na.rm = TRUE))
Cal_Regency_modelling$CHILDREN_NUM_R[is.na(Cal_Regency_modelling$CHILDREN_NUM_R)] <- round(mean(Cal_Regency_modelling$CHILDREN_NUM_R, na.rm = TRUE))

#Linear Modelling Data Preparation:#as it requires numeric variables . Saving this before converting to factor.
California_Regency_Cal_Regency_LMData<- Cal_Regency_modelling #Linear modelling main modelling data !
#summary(California_Regency_Cal_Regency_LMData)

# Converting all numerical columns into " High", " Medium"," Low"
str(Cal_Regency_modelling)

convertToCategorical<-function(givenDataFrame,numColName)
{
  givenDataFrame[[numColName]]<-as.numeric(givenDataFrame[[numColName]])
  tempCol<-ifelse(givenDataFrame[[numColName]]>=9, "High",
                  ifelse(givenDataFrame[[numColName]]>=7 , "Medium",
                         "Low" ))
  return (tempCol)
}

numericVariableList<-c("Likelihood_Recommend_H","Overall_Sat_H","Guest_Room_H","Tranquility_H","Condition_Hotel_H","Customer_SVC_H","Staff_Cared_H","Internet_Sat_H","Check_In_H")

for (colName in numericVariableList)
{
  Cal_Regency_modelling[[colName]]<-convertToCategorical(Cal_Regency_modelling,colName)
}
View(Cal_Regency_modelling)


#Converting to factor
Cal_Regency_modelling[ ] <- lapply(Cal_Regency_modelling, factor)

#Missing values in Categorical columns (we added a new level called NA in place of NA's by using following command)
Cal_Regency_modelling$Age_Range_H<- addNA(Cal_Regency_modelling$Age_Range_H)
Cal_Regency_modelling$Gender_H<- addNA(Cal_Regency_modelling$Gender_H)
Cal_Regency_modelling$Age_Range_H<- addNA(Cal_Regency_modelling$Age_Range_H)
Cal_Regency_modelling$BusinessCenter_PL<- addNA(Cal_Regency_modelling$BusinessCenter_PL)
Cal_Regency_modelling$Convention_PL<- addNA(Cal_Regency_modelling$Convention_PL)
Cal_Regency_modelling$LimoService_PL<- addNA(Cal_Regency_modelling$LimoService_PL)
Cal_Regency_modelling$MiniBar_PL<- addNA(Cal_Regency_modelling$MiniBar_PL)
Cal_Regency_modelling$PoolIndoor_PL<- addNA(Cal_Regency_modelling$PoolIndoor_PL)
Cal_Regency_modelling$PoolOutdoor_PL<- addNA(Cal_Regency_modelling$PoolOutdoor_PL)
Cal_Regency_modelling$ShuttleService_PL<- addNA(Cal_Regency_modelling$ShuttleService_PL)
Cal_Regency_modelling$Spa_PL<- addNA(Cal_Regency_modelling$Spa_PL)
Cal_Regency_modelling$Resort_PL<- addNA(Cal_Regency_modelling$Resort_PL)
Cal_Regency_modelling$ValetParking_PL<- addNA(Cal_Regency_modelling$ValetParking_PL)
Cal_Regency_modelling$Golf_PL<- addNA(Cal_Regency_modelling$Golf_PL)

#summary(Cal_Regency_modelling)  # All NA's in numerical columns have been replaced .


Cal_Regency_modelling1<- Cal_Regency_modelling

###########################
#POV level
###########################
#POV analysis:
#Comparing within groups of hotels allows lower performing hotels to learn from higher performing sister hotels.
HotelPlot <- ggplot(usdata, aes(x=Brand_PL)) +
  geom_bar(aes(fill=POV_CODE_C), position="dodge") +
  scale_fill_manual(values=c("Purple", "Orange")) +
  theme(axis.text.x = element_text(angle=60, hjust=1))+
  ggtitle("POV analysis on different brands of Hyatt hotels")
HotelPlot


###################
#Demographics Analysis:
###################
#Demographics and NPS: Purpose of Visit by Age
#pov, age bargraph
NPS_age<-ggplot(Cal_Regency_modelling, aes(Age_Range_H, fill = POV_CODE_C))+
  geom_bar()+
  ggtitle("Purpose of Visit by Age")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
NPS_age #maximum senior managers #age vs nps - max detractors: 46-55

#focus on males more:
Gender_Plot<-ggplot(Cal_Regency_modelling, aes(Gender_H, fill = POV_CODE_C))+
  geom_bar()+
  ggtitle("Purpose of Visit by Gender")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
Gender_Plot #maximum senior managers


#####################################################################################
#Booking Factors
#--------------------------------------------------------------------
# focus on booking channel- digital
Booking_Channelplot<-ggplot(Cal_Regency_modelling, aes(Booking_Channel, fill = NPS_Type))+
  geom_bar()+ scale_fill_manual(values=c("red", "blue","green"))+
  ggtitle("Booking_Channel vs NPS")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
Booking_Channelplot


#which day of the week most imp for check in ?- friday !
CHECK_IN_DATE_Cplot<-ggplot(Cal_Regency_modelling, aes(CHECK_IN_DATE_C, fill = NPS_Type))+
  geom_bar()+ scale_fill_manual(values=c("red", "blue","green"))+
  ggtitle("CHECK_IN_DATE_C vs NPS")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
CHECK_IN_DATE_Cplot

#sundays are critically imp for checkouts !
CHECK_OUT_DATE_Cplot<-ggplot(Cal_Regency_modelling, aes(CHECK_OUT_DATE_C, fill = NPS_Type))+
  geom_bar()+ scale_fill_manual(values=c("red", "blue","green"))+
  ggtitle("CHECK_OUT_DATE_C vs NPS")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
CHECK_OUT_DATE_Cplot

#most detractors in late checkin !
ENTRY_TIME_Rplot<-ggplot(Cal_Regency_modelling, aes(ENTRY_TIME_R, fill = NPS_Type))+
  geom_bar()+ scale_fill_manual(values=c("red", "blue","green"))+
  ggtitle("ENTRY_TIME_R vs NPS")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
ENTRY_TIME_Rplot

#Max people stay for 1 day and most detractors are in this time frame
lengthplot<-ggplot(Cal_Regency_modelling, aes(LENGTH_OF_STAY_R, fill = NPS_Type))+
  geom_bar()+
  ggtitle("Length of Stay")+
  theme(axis.text.x = element_text(angle=60, hjust=1))
lengthplot
#Max people stay for 1 day and most detractors are in this time frame


###################
#Facility Analysis:
###################
# What are the various services and parameters affect the promoters and detractors?

#Parameter: Guest Room
HotelGuest_Room_H <-
  ggplot(Cal_Regency_modelling, aes(x=Guest_Room_H)) +
  geom_bar(aes(fill=(NPS_Type)),na.rm = TRUE) +
  ggtitle("Guest Room Satisfaction vs Promoters, \n Passives & Detractors")
HotelGuest_Room_H

#Parameter: Condition of the hotel
HotelCondition_Hotel_H <- ggplot(Cal_Regency_modelling, aes(x=Condition_Hotel_H)) +
  geom_bar(
    aes(fill=NPS_Type),na.rm = TRUE,width=.56) +
  ggtitle("Condition of Hotel vs Promoters, \n Passives & Detractors")
HotelCondition_Hotel_H

# Effect of Good Customer Service
HotelCustomerService <- ggplot(Cal_Regency_modelling, aes(x=Customer_SVC_H)) +
  geom_bar(aes(fill=NPS_Type),na.rm = TRUE,width=.56)  +
  ggtitle("Customer Service vs Promoters, \n Passives & Detractors")
HotelCustomerService


#-------------------------------Modelling :----------------------------------------------------------
#############################################################################################################
#MODELLING
#############################################################################################################


#A)LINEAR MODELLING
#-------------------------------------------------------------------
#Linear Modelling:
colnames(California_Regency_Cal_Regency_LMData)
California_Regency_Cal_Regency_LMData1<-California_Regency_Cal_Regency_LMData
str(California_Regency_Cal_Regency_LMData1)
summary(as.factor(California_Regency_Cal_Regency_LMData1$Golf_PL))

#Removing Things that Hyatt cannot control:
Cal_Regency_LM <- subset(California_Regency_Cal_Regency_LMData1,select = -c(City_PL,CHECK_IN_DATE_C,CHECK_OUT_DATE_C,ENTRY_TIME_R,
                                                                            NUM_ROOMS_R,LENGTH_OF_STAY_R,ADULT_NUM_R,CHILDREN_NUM_R,
                                                                            NPS_Type,Booking_Channel))
summary(Cal_Regency_LM) 

lm_Total <- lm(formula=Likelihood_Recommend_H ~ Guest_Room_H+Tranquility_H +Condition_Hotel_H+Customer_SVC_H+
                 Staff_Cared_H+Internet_Sat_H+Check_In_H+BusinessCenter_PL+ Convention_PL+MiniBar_PL+PoolIndoor_PL+PoolOutdoor_PL+LimoService_PL+
                 Resort_PL+ShuttleService_PL+Spa_PL+ValetParking_PL,data=Cal_Regency_LM)
summary(lm_Total) #0.8336



#Business:
California_Regency_Business_lm<-Cal_Regency_LM[Cal_Regency_LM$POV_CODE_C=="BUSINESS",]
str(California_Regency_Business_lm) 

lm_Business <- lm(formula=Likelihood_Recommend_H ~ Overall_Sat_H+Internet_Sat_H+BusinessCenter_PL+ Convention_PL+MiniBar_PL+PoolOutdoor_PL+
                    ShuttleService_PL+LimoService_PL+Spa_PL+ValetParking_PL,data=California_Regency_Business_lm)
summary(lm_Business)
applySVMModel(lm_Business)

#Leisure:
California_Regency_Leisure_lm<-Cal_Regency_LM[Cal_Regency_LM$POV_CODE_C=="LEISURE",]
str(California_Regency_Leisure_lm) 

lm_Leisure <- lm(formula=Likelihood_Recommend_H ~ Overall_Sat_H+Internet_Sat_H+Spa_PL+ShuttleService_PL+ValetParking_PL+BusinessCenter_PL+PoolOutdoor_PL+
                   Convention_PL+MiniBar_PL+LimoService_PL,data=California_Regency_Leisure_lm)
summary(lm_Leisure)


#Booking factors
Cal_Regency_LM_BF<- subset(California_Regency_Cal_Regency_LMData1,select = c(Gender_H,Age_Range_H, Likelihood_Recommend_H,Overall_Sat_H,City_PL,ENTRY_TIME_R,
                                                                             NUM_ROOMS_R,LENGTH_OF_STAY_R,ADULT_NUM_R,CHILDREN_NUM_R,
                                                                             Booking_Channel))
summary(Cal_Regency_LM_BF)
str(Cal_Regency_LM_BF)

#Converting to factor
Cal_Regency_LM_BF$City_PL<- as.factor(Cal_Regency_LM_BF$City_PL)
Cal_Regency_LM_BF$Booking_Channel<- as.factor(Cal_Regency_LM_BF$Booking_Channel)
Cal_Regency_LM_BF$ENTRY_TIME_R<- as.factor(Cal_Regency_LM_BF$ENTRY_TIME_R)

lm_BookingF <- lm(formula=Likelihood_Recommend_H ~ Overall_Sat_H+ENTRY_TIME_R+NUM_ROOMS_R+LENGTH_OF_STAY_R+Booking_Channel+
                    ADULT_NUM_R+CHILDREN_NUM_R,data=Cal_Regency_LM_BF)
summary(lm_BookingF)


Corrdf<- subset(Cal_Regency_LM,select = c(Likelihood_Recommend_H,Guest_Room_H,Overall_Sat_H,Tranquility_H,Condition_Hotel_H,Customer_SVC_H,Staff_Cared_H,
                                          Internet_Sat_H,Check_In_H))

Cal_Regency_LM2<-Cal_Regency_LM

# 
# my_data<- Corrdf
# res <- cor(my_data)
# View(round(res, 2))
# cor(my_data, use = "complete.obs")
# res2 <- rcorr(as.matrix(my_data))

# 
# flattenCorrMatrix <- function(cormat, pmat) 
# {
#   ut <- upper.tri(cormat)
#   data.frame(
#     row = rownames(cormat)[row(cormat)[ut]],
#     column = rownames(cormat)[col(cormat)[ut]],
#     cor  =(cormat)[ut],
#     p = pmat[ut]
#   )
# }
# 
# library(Hmisc)
# res2<-rcorr(as.matrix(my_data))
# flattenCorrMatrix(res2$r, res2$P)
# symnum(res, abbr.colnames = FALSE)

# #Plot:
# corrplot(res, type = "upper", order = "hclust", 
#          tl.col = "black", tl.srt = 45)
# 
# # Insignificant correlation are crossed
# corrplot(res2$r, type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.01, insig = "blank")
# # Insignificant correlations are leaved blank
# corrplot(res2$r, type="upper", order="hclust", 
#          p.mat = res2$P, sig.level = 0.01, insig = "blank")
# 

#----------------------------------
model1 <- lm(formula=Likelihood_Recommend_H ~ Guest_Room_H, data=Cal_Regency_LM)
summary(model1) #R square= 0.5286 #important

model2 <- lm(formula=Likelihood_Recommend_H ~ Overall_Sat_H, data=Cal_Regency_LM)
summary(model2) #R square= 0.8169

model3 <- lm(formula=Likelihood_Recommend_H ~ Tranquility_H, data=Cal_Regency_LM)
summary(model3) #R square= 0.2068

model4 <- lm(formula=Likelihood_Recommend_H ~ Internet_Sat_H, data=Cal_Regency_LM)
summary(model4) #R square= 0.03152 #not important 

model5 <- lm(formula=Likelihood_Recommend_H ~ Condition_Hotel_H, data=Cal_Regency_LM)
summary(model5) #R square= 0.5066 #important !

model6 <- lm(formula=Likelihood_Recommend_H ~ Customer_SVC_H, data=Cal_Regency_LM)
summary(model6) #R square= 0.4838 #important !

model7 <- lm(formula=Likelihood_Recommend_H ~ Staff_Cared_H, data=Cal_Regency_LM)
summary(model7) #R square= 0.2303

model8 <- lm(formula=Likelihood_Recommend_H ~ Check_In_H, data=Cal_Regency_LM)
summary(model8) #R square= 0.1274

model9 <- lm(formula=Likelihood_Recommend_H ~ LimoService_PL, data=Cal_Regency_LM)
summary(model9) #R square= 0.002634

model10 <- lm(formula=Likelihood_Recommend_H ~ MiniBar_PL, data=Cal_Regency_LM)
summary(model10) #R square= 0.002735

model11 <- lm(formula=Likelihood_Recommend_H ~ BusinessCenter_PL, data=Cal_Regency_LM)
summary(model11) #R square= 0.001348

model12 <- lm(formula=Likelihood_Recommend_H ~ POV_CODE_C, data=Cal_Regency_LM)
summary(model12) #R square= 1.512e-05

model13 <- lm(formula=Likelihood_Recommend_H ~ PoolIndoor_PL, data=Cal_Regency_LM)
summary(model13) #R square= 0.0004427

model14 <- lm(formula=Likelihood_Recommend_H ~ PoolOutdoor_PL, data=Cal_Regency_LM)
summary(model14) #R square= 0.0001833

model15 <- lm(formula=Likelihood_Recommend_H ~ Resort_PL, data=Cal_Regency_LM)
summary(model15) #R square= 0.002776

model16 <- lm(formula=Likelihood_Recommend_H ~ ShuttleService_PL, data=Cal_Regency_LM)
summary(model16) #R square= 0.003479

model17 <- lm(formula=Likelihood_Recommend_H ~ ValetParking_PL, data=Cal_Regency_LM)
summary(model17) #R square=  3.79e-06

model19 <- lm(formula=Likelihood_Recommend_H ~ Age_Range_H, data=Cal_Regency_LM)
summary(model19) #R square=  0.008222

model20 <- lm(formula=Likelihood_Recommend_H ~ Gender_H, data=Cal_Regency_LM)
summary(model20) #R square= 0.002638

# #According to the linear model, Guest_Room_H +Condition_Hotel_H+Customer_SVC_H are all powerful columns
# Customer Service contributes the most to LTR followerd by Guest_Room_H, Condition_Hotel_H and Staff_Cared_H

#-------------------------------------------------------------------------------------------------------------

#B)Association Rule Mining to further prove the relationship between amanities that affect Business and Leisure people individually.
#--------------------------

#Splitting into business, leisure and Facility !

#Business:
California_Regency_Business<-Cal_Regency_modelling[Cal_Regency_modelling$POV_CODE_C=="BUSINESS",]
California_Regency_Business <- subset(California_Regency_Business,select = -c(POV_CODE_C))
str(California_Regency_Business) 

ModellingData_Business<- subset(California_Regency_Business,select = -c(Age_Range_H,Gender_H,Overall_Sat_H,Resort_PL,ShuttleService_PL,CHILDREN_NUM_R,
                                                                        Staff_Cared_H,ValetParking_PL,LimoService_PL,PoolOutdoor_PL,CHECK_IN_DATE_C,CHECK_OUT_DATE_C,
                                                                        ENTRY_TIME_R,NUM_ROOMS_R,LENGTH_OF_STAY_R,ADULT_NUM_R,CHILDREN_NUM_R,Booking_Channel,
                                                                        ROOM_TYPE_DESCRIPTION_C,Golf_PL))

str(ModellingData_Business)

#Leisure:
California_Regency_Leisure<-Cal_Regency_modelling[Cal_Regency_modelling$POV_CODE_C=="LEISURE",]
California_Regency_Leisure <- subset(California_Regency_Leisure,select = -c(POV_CODE_C))
str(California_Regency_Leisure)

ModellingData_Leisure <- subset(California_Regency_Leisure,select = -c(Age_Range_H,Gender_H,Overall_Sat_H,Convention_PL,BusinessCenter_PL,ValetParking_PL,
                                                                       CHECK_IN_DATE_C,CHECK_OUT_DATE_C,ENTRY_TIME_R,NUM_ROOMS_R,LENGTH_OF_STAY_R,ADULT_NUM_R,CHILDREN_NUM_R,Booking_Channel,
                                                                       ROOM_TYPE_DESCRIPTION_C,Likelihood_Recommend_H,Golf_PL))

str(ModellingData_Leisure) 


#Booking Factors Analysis:
#Business:
Modelling_Booking_Factors <- subset(Cal_Regency_modelling,select = c("Age_Range_H","Gender_H", "ENTRY_TIME_R","NUM_ROOMS_R","LENGTH_OF_STAY_R",
                                                                     "ADULT_NUM_R","CHILDREN_NUM_R","Booking_Channel","ROOM_TYPE_DESCRIPTION_C","NPS_Type"
                                                                     ))
str(Modelling_Booking_Factors) 



#A)Business
####################################

#Business_Promoters:
#---------------------------------

rules_Promoter <- apriori(ModellingData_Business,parameter=list(support=.53,confidence=0.92,maxlen=5),appearance =
                            list(rhs=c("NPS_Type=Promoter"),default="lhs"), control=list(verbose=F))
summary(rules_Promoter)

#good rules_Promoter
goodrules_Promoter <- rules_Promoter[quality(rules_Promoter)$lift > 1.58] #just 1 rule--- play around with it to get atleast 5-6 rules
#inspect(goodrules_Promoter)
summary(goodrules_Promoter)
plot(goodrules_Promoter)

#Pick the  most interesting & useful rules.
max_Promoter<- is.maximal(goodrules_Promoter) #Find Maximal Itemsets
#inspect(goodrules[max_Promoter])
MostInteresting_Promoter<-(goodrules_Promoter[max_Promoter])
summary(MostInteresting_Promoter)

 detach(package:tm, unload=TRUE)
library(arules)

#the NPS recommendation rules:
rules_conf_Promoter <- sort(MostInteresting_Promoter, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(rules_conf_Promoter) #98 % confidence
rules_lift_Promoter <- sort(MostInteresting_Promoter, decreasing = TRUE,by="lift") # high-lift rules
inspect(rules_lift_Promoter)

plot(rules_lift_Promoter, method="graph")



#Business_Detractors:
#---------------------------------
rules_Detractor <- apriori(ModellingData_Business,parameter=list(support=.086,confidence=0.95,maxlen=6),appearance =
                             list(rhs=c("NPS_Type=Detractor"),default="lhs"), control=list(verbose=F))
summary(rules_Detractor) #87 rules
#plot(rules_Detractor)
#inspect(rules_Detractor)

#good rules_Detractor
goodrules_Detractor <- rules_Detractor[quality(rules_Detractor)$lift > 6.339] 
#inspect(goodrules_Promoter) # 87
summary(goodrules_Detractor)
plot(goodrules_Detractor)

#Pick the 3 most interesting & useful rules.
max_Detractor<- is.maximal(goodrules_Detractor) #Find Maximal Itemsets
#inspect(goodrules[max_Detractor])
MostInteresting_Detractor<-(goodrules_Detractor[max_Detractor])
summary(MostInteresting_Detractor)#9 rules

#the NPS recommendation rules:
rules_conf_Detractor <- sort(MostInteresting_Detractor, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(rules_conf_Detractor)
rules_lift_Detractor <- sort(MostInteresting_Detractor, decreasing = TRUE,by="lift") # high-lift rules
inspect(rules_lift_Detractor)

plot(rules_lift_Detractor, method="graph")

#Business_Passive:
#---------------------------------
rules_Passive <- apriori(ModellingData_Business,parameter=list(support=.1,confidence=0.75),appearance =
                           list(rhs=c("NPS_Type=Passive"),default="lhs"), control=list(verbose=F))
summary(rules_Passive) 

#good rules_Passive
goodrules_Passive <- rules_Passive[quality(rules_Passive)$lift > 4] 
summary(goodrules_Passive)


#Pick the 3 most interesting & useful rules.
max_Passive<- is.maximal(goodrules_Passive) #Find Maximal Itemsets
MostInteresting_Passive<-(goodrules_Passive[max_Passive])
summary(MostInteresting_Passive)

#the NPS recommendation rules:
rules_conf_Passive <- sort(MostInteresting_Passive, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(rules_conf_Passive)
rules_lift_Passive <- sort(MostInteresting_Passive, decreasing = TRUE,by="lift") # high-lift rules
inspect(rules_lift_Passive)




#################################################################################################################

#B)Leisure
###################################
str(ModellingData_Leisure)
#------------------------------------
#Leisure_Promoters:
#---------------------------------

#Relation between purpose of visit, city, age range, NPS type and Gender

L_ruleSetH <- apriori(ModellingData_Leisure,parameter=list(support=0.14,confidence=0.96),appearance =
                        list(rhs=c("NPS_Type=Promoter"),default="lhs"), control=list(verbose=F))
summary(L_ruleSetH) 
#good rules_Promoter
L_goodrules_Promoter <- L_ruleSetH[quality(L_ruleSetH)$lift > 1.4] 
summary(L_goodrules_Promoter)

#Pick the  most interesting & useful rules.
L_max_Promoter<- is.maximal(L_goodrules_Promoter) 
L_MostInteresting_Promoter<-(L_goodrules_Promoter[L_max_Promoter])
summary(L_MostInteresting_Promoter)
L_rules_conf_Promoter <- sort(L_MostInteresting_Promoter, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(L_rules_conf_Promoter) 
L_rules_lift_Promoter <- sort(L_MostInteresting_Promoter, decreasing = TRUE,by="lift") # high-lift rules
inspect(L_rules_lift_Promoter)
plot(L_rules_lift_Promoter, method="graph")



#Leisure_Detractors:
#---------------------------------
L_ruleSetL <- apriori(ModellingData_Leisure,parameter=list(support=0.0119,confidence=1),appearance =
                        list(rhs=c("NPS_Type=Detractor"),default="lhs"), control=list(verbose=F))
summary(L_ruleSetL) 

#good rules_Detractor
L_goodrules_Detractor <- L_ruleSetL[quality(L_ruleSetL)$lift > 6] 
summary(L_goodrules_Detractor)
L_max_Detractor<- is.maximal(L_goodrules_Detractor) 
L_MostInteresting_Detractor<-(L_goodrules_Detractor[L_max_Detractor])
summary(L_MostInteresting_Detractor)
L_rules_conf_Detractor <- sort(L_MostInteresting_Detractor, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(L_rules_conf_Detractor)
L_rules_lift_Detractor <- sort(L_MostInteresting_Detractor, decreasing = TRUE,by="lift") # high-lift rules
inspect(L_rules_lift_Detractor)

plot(L_rules_lift_Detractor, method="graph")



################################################################################################################
#C)Booking Factors
####################################

#Business:


str(Modelling_Booking_Factors)
#A)Business_Booking Factors:
#---------------------------------------------------------

Business_Booking_Prom <- apriori(Modelling_Booking_Factors,parameter=list(support=0.11,confidence=0.65),appearance =
                                   list(rhs=c("NPS_Type=Promoter"),default="lhs"), control=list(verbose=F))
summary(Business_Booking_Prom)

#good rules_Promoter
BK_Bus_goodrules_Promoter <- Business_Booking_Prom[quality(Business_Booking_Prom)$lift > 1] 
summary(BK_Bus_goodrules_Promoter) 
plot(BK_Bus_goodrules_Promoter)

#Pick the  most interesting & useful rules.
BK_Bus_max_Promoter<- is.maximal(BK_Bus_goodrules_Promoter) 
BK_Bus_MostInteresting_Promoter<-(BK_Bus_goodrules_Promoter[BK_Bus_max_Promoter])
summary(BK_Bus_MostInteresting_Promoter)

BK_rules_conf_Promoter <- sort(BK_Bus_MostInteresting_Promoter, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(BK_rules_conf_Promoter) #99 # accuracy
BK_rules_lift_Promoter <- sort(BK_Bus_MostInteresting_Promoter, decreasing = TRUE,by="lift") # high-lift rules
inspect(BK_rules_lift_Promoter)

plot(BK_rules_lift_Promoter, method="graph")


#-----------------------------------------------------------------------------------

Business_Booking_Detr <- apriori(Modelling_Booking_Factors,parameter=list(support=0.0117,confidence=0.19),appearance =
                                   list(rhs=c("NPS_Type=Detractor"),default="lhs"), control=list(verbose=F))
summary(Business_Booking_Detr)

#good rules_Promoter
BK_Bus_goodrules_Detractor <- Business_Booking_Detr[quality(Business_Booking_Detr)$lift > 1]
summary(BK_Bus_goodrules_Detractor) 

#Pick the  most interesting & useful rules.
BK_Bus_max_Detractor<- is.maximal(BK_Bus_goodrules_Detractor) 
BK_Bus_MostInteresting_detractor<-(BK_Bus_goodrules_Detractor[BK_Bus_max_Detractor])
summary(BK_Bus_MostInteresting_detractor)

#, Insights: Booking_Channel=Digital Channels,Gender_H=Male ;NUM_ROOMS_R=1,and  ADULT_NUM_R=1,
#the NPS recommendation rules:

BK_rules_conf_Detractor <- sort(BK_Bus_MostInteresting_detractor, decreasing = TRUE,by="confidence") #high-confidence rules
inspect(BK_rules_conf_Detractor) #99 # accuracy
BK_rules_lift_Detractor <- sort(BK_Bus_MostInteresting_detractor, decreasing = TRUE,by="lift") # high-lift rules
inspect(BK_rules_lift_Detractor)

plot(BK_rules_lift_Detractor, method="graph")

#Booking_Channel=Digital Channels, is most contributing towards detractors.

#, Insights: NUM_ROOMS_R=1,
#CHILDREN_NUM_R=2,
#Likelihood_Recommend_H=2
#Booking_Channel=Global Contact Center,
#Give discounts to family children =2


#---------------------------------------------------------------------------------------------------------
#########################################################################################################
#C)SVM Modelling:


#A) Splitting into business, leisure 

Cal_Regency_modellingSVM<-  subset(Cal_Regency_modelling1,select = -c(City_PL,CHECK_IN_DATE_C,CHECK_OUT_DATE_C,ENTRY_TIME_R,
                                                                      NUM_ROOMS_R,LENGTH_OF_STAY_R,Golf_PL,
                                                                      Overall_Sat_H,Likelihood_Recommend_H,ROOM_TYPE_DESCRIPTION_C))
str(Cal_Regency_modellingSVM) 
#Business:
Cal_Regency_modellingSVM_Business<-Cal_Regency_modellingSVM[Cal_Regency_modellingSVM$POV_CODE_C=="BUSINESS",]
str(Cal_Regency_modellingSVM_Business)
summary(Cal_Regency_modellingSVM_Business)


#A)Business
##############

#Train and Test datasets
random_index<- sample(1:dim(Cal_Regency_modellingSVM_Business)[1])
cutPoint2_3 <- floor(2 * dim(Cal_Regency_modellingSVM_Business)[1]/3) #floor() function chops off any decimal part of the calculation. #We want to get rid of any decimal because an index variable needs to be an integer.
cutPoint2_3 #31378

#Test and training sets:
trainData <- Cal_Regency_modellingSVM_Business[random_index[1:cutPoint2_3],] #102 obs
testData<- Cal_Regency_modellingSVM_Business[random_index[(cutPoint2_3+1):dim(Cal_Regency_modellingSVM_Business)[1]],] #51 observations
str(trainData)

#Leisure:
Cal_Regency_modellingSVM_Leisure<-Cal_Regency_modellingSVM[Cal_Regency_modellingSVM$POV_CODE_C=="LEISURE",]
str(Cal_Regency_modellingSVM_Leisure) 


#Train and Test datasets
random_index<- sample(1:dim(Cal_Regency_modellingSVM_Leisure)[1])
cutPoint2_3 <- floor(2 * dim(Cal_Regency_modellingSVM_Leisure)[1]/3) #floor() function chops off any decimal part of the calculation. #We want to get rid of any decimal because an index variable needs to be an integer.
cutPoint2_3 #31378

#Test and training sets:
trainData <- Cal_Regency_modellingSVM_Leisure[random_index[1:cutPoint2_3],] 
testData<- Cal_Regency_modellingSVM_Leisure[random_index[(cutPoint2_3+1):dim(Cal_Regency_modellingSVM_Leisure)[1]],] 
str(trainData)


#SVM Modelling  to prove LM results
#--------

  svm_model<- svm(NPS_Type~Condition_Hotel_H,data=trainData,C=5)
  svm_Predicted<- predict(svm_model,testData,type="responses")
  comparison_Table_SVM <- data.frame(testData[["NPS_Type"]],svm_Predicted)
  colnames(comparison_Table_SVM) <- c('Test_NPS','Predicted_NPS')
  confusion_matrix_svm<- table(comparison_Table_SVM)
  print(confusion_matrix_svm)
  Accuracy_svm <-((confusion_matrix_svm[1,1]+confusion_matrix_svm[2,2]+confusion_matrix_svm[3,3])/nrow(comparison_Table_SVM))*100
  Accuracy_svm

  svm_model<- svm(NPS_Type~Guest_Room_H,data=trainData,C=5)
  svm_Predicted<- predict(svm_model,testData,type="responses")
  comparison_Table_SVM <- data.frame(testData[["NPS_Type"]],svm_Predicted)
  colnames(comparison_Table_SVM) <- c('Test_NPS','Predicted_NPS')
  confusion_matrix_svm<- table(comparison_Table_SVM)
  print(confusion_matrix_svm)
  Accuracy_svm <-((confusion_matrix_svm[1,1]+confusion_matrix_svm[2,2]+confusion_matrix_svm[3,3])/nrow(comparison_Table_SVM))*100
  Accuracy_svm
  
  svm_model<- svm(NPS_Type~Customer_SVC_H,data=trainData,C=5)
  svm_Predicted<- predict(svm_model,testData,type="responses")
  comparison_Table_SVM <- data.frame(testData[["NPS_Type"]],svm_Predicted)
  colnames(comparison_Table_SVM) <- c('Test_NPS','Predicted_NPS')
  confusion_matrix_svm<- table(comparison_Table_SVM)
  print(confusion_matrix_svm)
  Accuracy_svm <-((confusion_matrix_svm[1,1]+confusion_matrix_svm[2,2]+confusion_matrix_svm[3,3])/nrow(comparison_Table_SVM))*100
  Accuracy_svm
  
# ##BORUTA !----------------------Commented as it takes time. Please uncomment to run this-------------------------------------------
# dim(Cal_Regency_modellingSVM_Business)
# random_index<- sample(1:dim(Cal_Regency_modellingSVM_Business)[1])
# cutPoint2_3 <- floor(2 * dim(Cal_Regency_modellingSVM_Business)[1]/3) 
# cutPoint2_3
# 
# #Test and training sets:
# trainData <- Cal_Regency_modellingSVM_Business[random_index[1:cutPoint2_3],] #102 obs
# testData<- Cal_Regency_modellingSVM_Business[random_index[(cutPoint2_3+1):dim(Cal_Regency_modellingSVM_Business)[1]],] #51 observations
# 
# boruta.train <- Boruta(NPS_Type~., data = trainData, doTrace = 2)
# print(boruta.train)
# 
# plot(boruta.train, xlab = "", xaxt = "n")
# lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
#   boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
# names(lz) <- colnames(boruta.train$ImpHistory)
# Labels <- sort(sapply(lz,median))
# axis(side = 1,las=2,labels = names(Labels),
#      at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.6)
# 
# final.boruta <- TentativeRoughFix(boruta.train)
# 
# getSelectedAttributes(final.boruta, withTentative = F)
# boruta.df <- attStats(final.boruta)


#--------------------------------------------------------end---------------------------------------
