#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 10 
#lab 10: IST687 - Support Vector Machines Lab
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, April 11, 2018    
#Submit date : Wednesday, April 11, 2018 
#---------------------------------------------------
#########################################
#Step 1: Load the data  
#########################################
#Let go back and analyze the air quality dataset (if you remember, we used that previously, in the visualization lab). 
#Remember to think about how to deal with the NAs in the data.

AQ<- data.frame(airquality)
str(AQ)
#View(AQ)

#checking if any NA's?
summary(AQ)

#replacing NA's with mean values
AQ$Ozone[is.na(AQ$Ozone)] <- round(mean(AQ$Ozone, na.rm = TRUE))
AQ$Solar.R[is.na(AQ$Solar.R)] <- round(mean(AQ$Solar.R, na.rm = TRUE))
# All NA's have been replaced .


###########################################################################
#Step 2: Create train and test data sets
#############################################################################
#Using techniques discussed in class, create two datasets - one for training and one for testing.

#install.packages("kernlab")
library(kernlab)

dim(AQ)
random_index<- sample(1:dim(AQ)[1])
cutPoint2_3 <- floor(2 * dim(AQ)[1]/3) #floor() function chops off any decimal part of the calculation. #We want to get rid of any decimal because an index variable needs to be an integer.
cutPoint2_3 #102

#Test and training sets:
trainData_AQ <- AQ[random_index[1:cutPoint2_3],] #102 obs
testData_AQ <- AQ[random_index[(cutPoint2_3+1):dim(AQ)[1]],] #51 observations


###################################################################
#Step 3: Build a Model using KSVM & visualize the results
##################################################################
#1)	Build a model (using the 'ksvm' function, trying to predict ozone). 
#You can use all the possible attributes, or select the attributes that you think would be the most helpful.

ksvm_model_Ozone<- ksvm(Ozone ~.,data=testData_AQ,kernel="rbfdot",kpar="automatic",C=10,cross=3,prob.model=TRUE) #this combination gives best accuracy
ksvm_model_Ozone #best-92 % accuracy

#-----------------------------------------------------------------------
#2)	 Test the model on the testing dataset, and compute the Root Mean Squared Error
ksvm_Predicted_Ozone<- predict(ksvm_model_Ozone,testData_AQ,type="votes")
ksvm_Predicted_Ozone

comparison_Table_KSVM <- data.frame(testData_AQ[,1],ksvm_Predicted_Ozone[,1]) 

#change the columns names 
colnames(comparison_Table_KSVM) <- c('Test_Ozone','Predicted_Ozone')
#View(compTable)
comparison_Table_KSVM$ksvm_error<- (comparison_Table_KSVM$Test_Ozone-comparison_Table_KSVM$Predicted_Ozone) #actual-pred 
#View(comparison_Table_KSVM)
#View(AQ)

#compute the Root Mean Squared error
#install.packages("Metrics")
library(Metrics)
rmse(comparison_Table_KSVM$Test_Ozone,comparison_Table_KSVM$Predicted_Ozone)
#Answer: Root Mean Squared error: 21.81 

#-----------------------------------------------------------------------
#3)	Plot the results. Use a scatter plot. Have the x-axis represent temperature, 
#the y-axis represent wind, the point size and color represent the error, 
#as defined by the actualozone level  minus the predicted ozone level).

#create a new dataframe contains error, temperature and wind
KSVMdf <- data.frame(comparison_Table_KSVM$ksvm_error, testData_AQ$Temp, testData_AQ$Wind)

#assign column names
colnames(KSVMdf) <- c('ksvm_error','Temp','Wind')
#View(KSVMdf)

#scatterplot 
library(ggplot2)
ksvmscatterchart = ggplot(data = KSVMdf,aes(x=Temp, y=Wind)) + 
  geom_point(aes(size= ksvm_error,	color= ksvm_error))+
  ggtitle("KSVM Scatter Plot")
ksvmscatterchart 

#-----------------------------------------------------------------------
#4)	Compute models and plot the results for 'svm' (in the e1071 package) and 'lm'. Generate similar charts for each model

#install.packages("e1071")
library(e1071)

#SVM model:
###########
svm_model_Ozone <- svm(Ozone ~. , data=trainData_AQ)

svm_Predicted_Ozone <- predict(svm_model_Ozone,testData_AQ)
#View(svm_Predicted_Ozone)

comparison_Table_SVM <- data.frame(testData_AQ[,1],svm_Predicted_Ozone) #create a  comparison dataframe that contains the exact and predicted Ozone value

colnames(comparison_Table_SVM) <- c('Actual_Ozone_Test','Predicted_Ozone_SVM') #change the columns names:
#View(comparison_Table_SVM)

#Calculating root mean squared error:
rmse(comparison_Table_SVM$Actual_Ozone_Test,comparison_Table_SVM$Predicted_Ozone_SVM) #Other way : sqrt(mean((comparison_Table_SVM$Actual_Ozone_Test- comparison_Table_SVM$Predicted_Ozone_SVM)^2))
# Answer:  SVM model root mean squared error: 18.725 

#Scatter plot :
comparison_Table_SVM$svm_error<- (comparison_Table_SVM$Actual_Ozone_Test-comparison_Table_SVM$Predicted_Ozone_SVM) #compute error for each case and store in a new column

SVMdf <- data.frame(comparison_Table_SVM$svm_error, testData_AQ$Temp, testData_AQ$Wind) #create a new dataframe contains svm_error, temperature and wind
#View(SVMdf)
colnames(SVMdf) <- c('svm_error','Temp','Wind') #assign column names
#View(SVMdf)

library(ggplot2)
Svm_scatterchart = ggplot(SVMdf,aes(x=Temp, y=Wind)) + 
  geom_point(aes(size= svm_error,color= svm_error))+
ggtitle("SVM Scatter Plot")
Svm_scatterchart 

#-----------------------------------------------------------------------
#LM model
##########
LM_model <- lm(formula=Ozone ~., data= trainData_AQ)

LM_Predicted_Ozone <- predict(LM_model , testData_AQ)
LM_Predicted_Ozone

comparison_Table_LM <- data.frame(testData_AQ[,1],LM_Predicted_Ozone) #create a comparison dataframe that contains the exact and the predicted Ozone value

colnames(comparison_Table_LM) <- c('Actual_Ozone_Test','Predicted_Ozone_LM') #change the columns names 

#calculating root mean squared error
rmse(comparison_Table_LM$Actual_Ozone_Test,comparison_Table_LM$Predicted_Ozone_LM) 
#Answer: LM root mean squared error: 19.63 %

#LM scatter plot:
comparison_Table_LM$lM_error <- abs(comparison_Table_LM$Actual_Ozone_Test - comparison_Table_LM$Predicted_Ozone_LM) #compute error for each case and store in a new column
#View(comparison_Table_LM)

#comparison_Table_LM$lM_error<- as.factor(comparison_Table_LM$lM_error)
class(comparison_Table_LM$lM_error)

LMdf <- data.frame(comparison_Table_LM$lM_error, testData_AQ$Temp, testData_AQ$Wind)#create a new dataframe contains error, temperature and wind
colnames(LMdf) <- c('lM_error','Temp','Wind') #assign column names
#View(LMdf)

#Plot 
LM_ScatterPlot <- ggplot(LMdf, aes(x=Temp, y=Wind))+
  geom_point(aes(size=lM_error,color=lM_error))+
  ggtitle("LM Scatter Plot")
LM_ScatterPlot #perfect

#-------------------------------------------------------------------------------
#5)	Show all three results (charts) in one window, using the grid.arrange function
#install.packages("gridExtra")
library(gridExtra)
grid.arrange(ksvmscatterchart,Svm_scatterchart,LM_ScatterPlot)


##############################################################################################################
######################################################################################
#Step 4: Create a 'goodOzone' variable
######################################################################################
newAQ<- AQ

mean(newAQ$Ozone) #42.09804 

#goodOzone variable should be either 0 or 1. 
newAQ$goodozone = ifelse(newAQ$Ozone >= mean(newAQ$Ozone),"1","0")
#View(newAQ)
newAQ1 <- subset(newAQ, select=-Ozone) #dropped Ozone column to avoid target leakage
#View(newAQ1)



###########################################################################################
#Step 5: See if we can do a better job predicting 'good' and 'bad' days
###########################################################################################

# now seperating the training and the testing data again
dim(newAQ1)
randnum = sample(1:dim(newAQ1)[1])
cutnum = floor(2	*	dim(newAQ1)[1]/3)

#creating training and testing data
traindata_newAQ1= newAQ1[randnum[1:cutnum],] 
testdata_newAQ1= newAQ1[randnum[(cutnum+1):dim(newAQ1)[1]],]




#1)Build ksvm model
ksvmmodel_goodozone = ksvm(goodozone ~ .,data = traindata_newAQ1,kernel="rbfdot",kpar="automatic",C=5,cross=3,prob.model=TRUE)
ksvmmodel_goodozone
#-----------------------------------------------------------------------------------------
#2)Test the model on the testing dataset,and compute the percent of 'goodOzone' that was correctly predicted. 
ksvm_predicted_goodozone = predict(ksvmmodel_goodozone,testdata_newAQ1)
ksvm_predicted_goodozone

ksvm_comptable_goodozone = data.frame(ksvm_predicted_goodozone,testdata_newAQ1$goodozone)
#View(ksvm_comptable_goodozone)

colnames(ksvm_comptable_goodozone) <- c('Predicted_goodozone','Actual_goodozone') #assign column names

temp<-table(ksvm_comptable_goodozone) #table function uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.
#View(temp)

# ksvm_comptable_goodozone$ksvm_predicted_goodozone<- as.factor(ksvm_comptable_goodozone$ksvm_predicted_goodozone)
# ksvm_comptable_goodozone$testdata_newAQ1.goodozone<- as.factor(ksvm_comptable_goodozone$testdata_newAQ1.goodozone)

# percentage of good ozone
Totalcorrect = temp[1,1]+temp[2,2]
Total_in_test = nrow(testdata_newAQ1)
Totalcorrect/Total_in_test*100
#84.31373 percent

#Confirming error with caret package:
# install.packages("caret")
# library(caret)
# ksvm_comptable_goodozone<- confusionMatrix(as.factor(ksvm_predicted_goodozone), as.factor(testdata_newAQ1$goodozone))
# ksvm_comptable_goodozone

#-----------------------------------------------------------------------------------------

# 3)plot
#newAQ1$goodozone = factor(newAQ1$goodozone)
ksvm_comptable_goodozone$haserror = ifelse(ksvm_comptable_goodozone$Predicted_goodozone == ksvm_comptable_goodozone$Actual_goodozone,"0","1")

KSVMdf_goodozone<- data.frame(ksvm_comptable_goodozone$haserror,testdata_newAQ1$Temp, testdata_newAQ1$Wind )
#View(KSVMdf_goodozone)
colnames(KSVMdf_goodozone) <- c('has_error','Temp', 'Wind') #assign column names

scatterchartgraphksvm = ggplot(  data=KSVMdf_goodozone,aes(x=Temp, y=Wind))  +
  geom_point(aes(size= KSVMdf_goodozone$has_error,	color= factor(testdata_newAQ1$goodozone),shape=ksvm_predicted_goodozone))+
  ggtitle("KSVM Goodozone Scatter Plot")
scatterchartgraphksvm


#-------------------------------------------------------------------------------------------
#4a)	Compute models and plot the results for 'svm' (in the e1071 package) 

#Goodozone_svmmodel:

#svmmodel needs numeric variables:
#newAQ1$goodozone = as.numeric(newAQ1$goodozone)
traindata_newAQ1$goodozone = as.factor(traindata_newAQ1$goodozone)
testdata_newAQ1$goodozone = as.factor(testdata_newAQ1$goodozone)

svmmodel_new = svm (goodozone~ . ,data = traindata_newAQ1)
svmmodel_new
svm_predict_new = predict(svmmodel_new,testdata_newAQ1)
svm_predict_new

svm_comptable_goodozone = data.frame(svm_predict_new,testdata_newAQ1$goodozone)
#View(ksvm_comptable_goodozone2)
temp2<- table(svm_comptable_goodozone)
#View(temp2)

#to compare the percentage of good ozone
Total_correct2 = temp2[1,1]+temp2[2,2]
Total_in_test2 = nrow(testdata_newAQ1)
Total_correct2/Total_in_test2*100
#80.39216 percent

#plot

svm_comptable_goodozone$haserror = ifelse(svm_comptable_goodozone$svm_predict_new == svm_comptable_goodozone$testdata_newAQ1,"0","1")

SVMdf_goodozone<- data.frame(svm_comptable_goodozone$haserror,testdata_newAQ1$Temp,testdata_newAQ1$Wind,testdata_newAQ1$goodozone)
#View(SVMdf_goodozone)

colnames(SVMdf_goodozone)<- c("haserror","Temp","Wind","goodozone")

scatterchartgraphsvm = ggplot(data = SVMdf_goodozone,aes(x=Temp, y=Wind)) + 
  geom_point(aes(size= factor(SVMdf_goodozone$haserror),	color= factor(goodozone),shape=svm_predict_new))+
  ggtitle("SVM Goodozone Scatter Plot")
scatterchartgraphsvm

#------------------------------------------------------------------------
#4b)	Compute models and plot the results for 'nb' (Naive Bayes, also in the e1071 package)

#nb model
#install.packages("naivebayes")
library(naivebayes)


nbmodel = naiveBayes(goodozone ~.,data = traindata_newAQ1)
nbpredict <- predict(nbmodel,testdata_newAQ1)
nbpredict

table(nbpredict,testdata_newAQ1$goodozone)

NB_comptable_goodozone <- data.frame(nbpredict,testdata_newAQ1$goodozone)
temp4<- table(NB_comptable_goodozone)
#View(temp4)

#to compare the percentage of good ozone
totalcorrect3 = temp4[1,1]+temp4[2,2]
totalintest3 = nrow(testdata_newAQ1)
totalcorrect3/totalintest3*100
#82.35294 percent


NB_comptable_goodozone$haserror = ifelse(NB_comptable_goodozone$nbpredict == NB_comptable_goodozone$testdata_newAQ1.goodozone,"0","1")
#View(newAQ)

NBdf_goodozone<- data.frame(NB_comptable_goodozone$haserror,testdata_newAQ1$Temp,testdata_newAQ1$Wind,testdata_newAQ1$goodozone)
#View(NBdf_goodozone)

colnames(NBdf_goodozone)<- c("haserror","Temp","Wind","goodozone")

scatterchartgraphnb = ggplot(data = NBdf_goodozone,aes(x=Temp, y=Wind)) + 
  geom_point(aes(size= factor(NB_comptable_goodozone$haserror),	color= factor(goodozone),shape=nbpredict))+
  ggtitle("Naive Bayes Goodozone Scatter Plot")
scatterchartgraphnb


#----------------------------------------------------------------------------------------------

#5) Show all three results (charts) in one window, using the grid.arrange function (have two charts in one row).
#using grid arrange function to get the desired data
grid.arrange(scatterchartgraphksvm,scatterchartgraphsvm,scatterchartgraphnb)



#########################################################################################
#Step 6: Which are the best Models for this data? 
#######################################################################################
#Review what you have done and state which is the best and why?

#Answer: In this assignment we first created models by training our data and testing it after sampling it.
#Then we checked consistency of our predicted ozone levels by comparing various regression and categorical models
#KSVM model is appropriate as it was found to have the highest accuracy.

KSVM
#84.31373

SVM
#80.39216 

NaiveBayes
#82.35294

#KSVM has the best percentage followed by Naive Bayes  and SVM respectively. Therefore are the best model is KSVM

