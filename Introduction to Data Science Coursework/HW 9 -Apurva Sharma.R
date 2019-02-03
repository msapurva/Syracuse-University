#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 9 
#lab 9: aRULES Homework: The Titanic
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, March 24, 2018    -------------------------check !            
#Submit date : Wednesday, March 21, 2018 
#---------------------------------------------------

#Download the titanic dataset from blackboard.
setwd("~/Desktop")
load("titanic.raw.rdata")
t<- titanic.raw
View(t)
summary(t)

#install.packages("arules")
#library(arules)
##############################################################################
#Step 1: Descriptive Stats
##############################################################################
#1)	Compute the percentage of people that survived.  
People_survived <- (length(t$Survived[t$Survived=='Yes'])/length(t$Survived))*100
People_survived #Answer: 32.3035

#2)	Compute the percentage of people that were children
Child_Per <- (length(t$Age[t$Age=='Child'])/length(t$Age))*100
Child_Per #Answer: 4.952294

#3)	Compute the percentage of people that were female
Female_Per <- (length(t$Sex[t$Sex=="Female"])/length(t$Sex))*100
Female_Per #Answer: 21.35393

#4)	Finally, compute the percentage of people that were in first class
First_Per <- (length(t$Class[t$Class=="1st"])/length(t$Class))*100
First_Per #Answer: 14.76602


##############################################################################
#Step 2: More Descriptive Stats
##############################################################################

#1)	What percentage of children survived?
Children_survived<- tapply(t$Survived=='Yes', t$Age=='Child', sum)
View(Children_survived)
Children_survived[[2]]
#or
Children_survived<- tapply(t$Survived=='Yes', t$Age=='Child', sum) [[2]]
Children_survived

Total_Children <- (length(t$Age[t$Age=="Child"]))
Total_Children

Children_survived_Per <- (Children_survived/Total_Children)*100
Children_survived_Per      #Answer: 52.2 %

#2)	What percentage of female survived?
Female_Survived<- tapply(t$Sex=="Female",t$Survived=="Yes",sum)[[2]]
Female_Survived

Total_Female<- (length(t$Sex[t$Sex=="Female"]))
Total_Female

Female_Survived_Per<- Female_Survived/Total_Female*100
Female_Survived_Per    #Answer: 73.19 %

#3)	What percentage of first class people survived?
First_Survived<- tapply(t$Class=="1st",t$Survived=="Yes",sum)[[2]]
First_Survived

Total_First<- (length(t$Class[t$Class=="1st"]))
Total_First

First_Survived_Per<- First_Survived/Total_First*100
First_Survived_Per    #Answer: 62.46 %

#4)	What percentage of 3rd class people survived?
Third_Survived<- tapply(t$Class=="3rd",t$Survived=="Yes",sum)[[2]]
Third_Survived

Total_Third<- (length(t$Class[t$Class=="3rd"]))
Total_Third

Third_Survived_Per<- Third_Survived/Total_Third*100
Third_Survived_Per    #Answer: 25.21 %

##############################################################################
#Step 3: Writing a Function
##############################################################################
titanic<- titanic.raw

#1)	Write a function that returns a new dataframe of people that satisfy the specified criteria of sex, age, class and survived as parameters

Titanic_function <- function(s,a,c,surv)
{
  df <- data.frame(t[t$Sex==s & t$Age==a & t$Class==c &  t$Survived==surv,])
  return(df)
}
Titanic_function

#2)	Write a function, using the previous function, that calculates the percentage (who lives, who dies) for a 
# specified (parameters) of age, class and sex.

Titanic_Live_Dead_Rate <- function(s,a,c)
{
  survived_Yes <- data.frame(Titanic_function(s,a,c,'Yes'))
  survived_No <- data.frame(Titanic_function(s,a,c,'No'))
  Survived_Per <- (nrow(survived_Yes)/(nrow(survived_Yes)+nrow(survived_No)))*100
  print(paste("Percentage of Survivors =",Survived_Per))
  Dead_Per <- (nrow(survived_No)/(nrow(survived_Yes)+nrow(survived_No)))*100
  print(paste("Percentage of people who died =",Dead_Per))
}

Titanic_Live_Dead_Rate

#3)	Use the function to compare age & 3rd class male survival rates
Titanic_Live_Dead_Rate('Male','Child','3rd')
Titanic_Live_Dead_Rate('Male','Adult','3rd')


#4)	Use the function to compare age & 1st class female survival rates
Titanic_Live_Dead_Rate('Female','Child','1st')
Titanic_Live_Dead_Rate('Female','Adult','1st')


##############################################################################
#Step 4: Use aRules
##############################################################################
#First step: Association mine ruling
#4a)	Use arules to calculate some rules (clusters) for the titanic dataset

install.packages("arules")
library(arules)

titanicnew<- as(t,"transactions")
summary(titanicnew)

itemFrequencyPlot(titanicnew,support=0.1) # When using the itemFrequencyPlot() function, you must specify the minimum level of support needed to include an item in the plot.

itemFrequencyPlot(titanicnew,support=0.05,cex.names=0.5) #in this case no diff as dataset is too small.

rules_01 <- apriori(titanicnew, parameter=list (support=0.01,confidence=0.50)) #lowest support and 50% confidence to understand what is good
summary(rules_01)
inspect(rules_01) #120 rules are too much so increasing support to 0.1


rules_1 <- apriori(titanicnew, parameter=list (support=0.1,confidence=0.50)) 
summary(rules_1)
inspect(rules_1) #48 rules generated

#---------------------

rules_3 <- apriori(titanicnew, parameter=list (support=0.3,confidence=0.50)) 
summary(rules_3)
inspect(rules_3) #26 rules generated

rules_5 <- apriori(titanicnew, parameter=list (support=0.5,confidence=0.50)) 
summary(rules_5)
inspect(rules_5) #12 rules generated

#Now it is possible that we have set our parameters for confidence and support too stringently, 
#and as a result we have missed some truly novel combinations that might lead us to better insights.
#We can use a data visualization package to help explore this possibility.

########################
#4b)	Visualize the results
###########################
install.packages("arulesViz")
library(arulesViz)

#Now let’s return to our apriori() command, but we will be much more lenient this time in our minimum support 
#and confidence parameters:

rules_lenient <- apriori(titanicnew, parameter=list (support=0.001,confidence=0.35)) 
summary(rules_lenient)
inspect(rules_lenient) #142 rules generated. That is way too many rules to examine manually, so let’s use 
# the arulesViz package to see what we have. We will use the plot() command to do so

plot(rules_lenient) #Support is on the X-axis and confidence is on the Y-axis and lift serves as a measure of interestingness
#so, high support, high confidence, high lift

#The darker the dot, the closer the lift of that rule is to 8.0, which appears to be the highest lift value among these 142 rules.


#The other thing we can see from this plot is that although the support of rules ranges from somewhere below 1% 
# to all the way up above 8%, all of the rules with high lift seem to have support below 1%.
#On the other hand, there are rules with high lift and high confidence, which sounds quite positive. 
#Based on this evidence, let’s focus on a smaller set of rules that have only the very highest levels of lift. 
#The following command makes a subset of the larger set of rules by choosing only those rules that have lift 
#higher than 6.5:

goodrules <- rules_lenient[quality(rules_lenient)$lift > 6.5] #just 1 rule--- play around with it to get atleast 5-6 rules
inspect(goodrules) #just 1 rule so lower the lift


goodrules <- rules_lenient[quality(rules_lenient)$lift > 3.5]
inspect(goodrules) #4 rules

plot(rules_lenient)

plot(rules_lenient, method = "grouped") #this picture when enlarged is very useful !

#Support :
# of rows having both A AND B / Total # of rows

#Confidence:
# of rows having both A AND B / # of rows with A

#Lift
#Lift: Confidence / Probability of second item
#Confidence / Expected confidence
#Expected confidence = # of rows with B / Total # of rows
  

######################################################
#4c)	Pick the 3 most interesting & useful rules.
######################################################
#Other way :
bestrules<- rules_lenient[quality(rules_lenient)$lift>1]
inspect(bestrules)

max<- is.maximal(bestrules)
inspect(bestrules[max])

sorted_rules <- sort((bestrules[max]), by="lift")
inspect(sorted_rules)

plot(bestrules)
plot(sorted_rules)

plot(sorted_rules, method="graph",
     control=list(nodeCol="red", edgeCol="blue", type="items")) #43 rules

rules_lift <- sort (sorted_rules, by="lift", decreasing=TRUE) # 'high-lift' rules.
inspect(head(rules_lift))

#    lhs                                    rhs          support     confidence lift     count
#[1] {Class=2nd,Sex=Male,Survived=Yes}   => {Age=Child}  0.004997728 0.4400000  8.884771  11  
#[2] {Class=2nd,Age=Adult,Survived=Yes}  => {Sex=Female} 0.036347115 0.8510638  3.985514  80  
#[3] {Sex=Female,Age=Child,Survived=Yes} => {Class=2nd}  0.005906406 0.4642857  3.585589  13  
#[4] {Class=1st,Age=Adult,Survived=Yes}  => {Sex=Female} 0.063607451 0.7106599  3.328005 140  
#[5] {Sex=Female,Age=Child,Survived=No}  => {Class=3rd}  0.007723762 1.0000000  3.117564  17  
#[6] {Sex=Male,Age=Child,Survived=No}    => {Class=3rd}  0.015901863 1.0000000  3.117564  35 

most_intersting_3 <- head(sort((sort.rule), by="lift"), 3) #most important 3 rules
inspect(most_intersting_3)

#Answer:

#lhs                                   rhs          support    confidence lift     count
#[1] {Class=2nd,Sex=Male,Survived=Yes}   => {Age=Child}  0.004997728 0.4400000  8.884771  11  
#[2] {Class=2nd,Age=Adult,Survived=Yes}  => {Sex=Female} 0.036347115 0.8510638  3.985514  80  
#[3] {Class=2nd,Survived=Yes}            => {Sex=Female} 0.042253521 0.7881356  3.690822  93


plot(rules_lift, method="paracoord", control=list(alpha=0.8, col=rainbow(7))) #most important

# get rules that lead to finding 'Survived=Yes'
rules_Surv_Yes <- apriori (data=titanicnew, parameter=list (supp=0.001,conf = 0.08), appearance = list (default="lhs",rhs="Survived=Yes"), control = list (verbose=F))
rules_conf <- sort (rules_Surv_Yes, by="confidence", decreasing=TRUE) # 'high-confidence' rules.
inspect(head(rules_conf))

########################################################
#4d) How does this compare to the descriptive analysis we did on the same dataset? 
#########################################################
# I think that using aprior is a better method to better examine the results from a data se as it enhances the 
# process of sorting through the evidence and helps us making sense of it. For example, if we were to find out actionable
#insights from such data, we will find out that The " women and chilren shall go first " saying holds true according to this data as
#the top most interesting rules from apriori results and rules that lead to finding 'Survived=Yes' indicate the same analysis.
#Also descriptive analysis just gives us percentage of survival and doesnt help us understand teh bigger picture.

#-------------------------------------------------end------------------------------------------------------------
