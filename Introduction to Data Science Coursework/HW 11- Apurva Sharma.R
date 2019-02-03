#Name: Apurva Sharma
#IST 687 
#Section M006
#Homework 11 
#lab 11: IST687 - Text Mining Homework
# Note to  grader: All my comments are original.
#---------------------------------------------------
#Due date : Wednesday, April 18, 2018    
#Submit date : Wednesday, April 18, 2018 
#---------------------------------------------------

setwd("~/Google Drive/itsappys/Academic docs/MS IM + CAS DS @ SU /3) IST 687 - Introduction to Data Science - /R/Text mining")

install.packages('tm')
install.packages('readtext')
install.packages("ggplot2")
install.packages("wordcloud")


library(readtext)
library(tm)
library(ggplot2)
library(wordcloud)

###############################################################
# Step 1: Read in the positive and negative word files  
###############################################################

#1)	Create two vectors of words, one for the positive words and one for the negative words
#2)	Note that when reading in the word files, there might be lines at the start and/or the end that will need to be removed 
#(i.e. you should clean you dataset).


#read full file
posWords <- scan("positive-words.txt", character(0),sep = "\n")

#remove the introductory header
posWords <- posWords[-1:-34]
print("Positive Words Head:")
print(head(posWords))
#Answer: [1] "a+"         "abound"     "abounds"    "abundance"  "abundant"   "accessable"

#read full file
negWords <- scan("negative-words.txt", character(0),sep = "\n") 

#remove the introductory header
negWords <- negWords[-1:-34]
print("Negative Words Head:")
print(head(negWords))
#Answer: [1] "2-faced"    "2-faces"    "abnormal"   "abolish"    "abominable" "abominably"



#############################################################################
#Step 2: Process in the MLK speech
###########################################################################
#3)	Read the text file

#Read the full file by lines
speech<-readLines("MLK-speech.txt")

#Drop empty lines that can be seen in output
speech<-speech[-c(6,7)]
print("Speech Head:")
print(head(speech))


# 4)	Create a term matrix

speechSentenceFeed<-VectorSource(speech)#Enable reading sentences as vectors
speechCorpus<-Corpus(speechSentenceFeed)#Convert the speech into text corpus

#Cleaning the text: standardize by converting to lower case
speechCorpus <- tm_map(speechCorpus, content_transformer(tolower))
#Cleaning the text: standardize by removing punctuation
speechCorpus <- tm_map(speechCorpus, removePunctuation)
#Cleaning the text: standardize by removing numbers
speechCorpus <- tm_map(speechCorpus, removeNumbers)
#Cleaning the text: standardize by dropping common words
speechCorpus <- tm_map(speechCorpus, removeWords, stopwords("english"))

#Creating the term matrix
termMatrix <- TermDocumentMatrix(speechCorpus)
print('Term Matrix:')
print(as.matrix(termMatrix))



# 5)	Create a list of counts for each word
#-------------
#Create and print word counts
wordCounts <- rowSums(as.matrix(termMatrix))
print('All Words with counts:')
print(wordCounts)

#wordcloud:
cloud_Frame<-data.frame(word=names(wordCounts),freq=wordCounts)
cloud_Frame
wordcloud(cloud_Frame$word,cloud_Frame$freq)
wordcloud(names(wordCounts), wordCounts, min.freq=2,max.words=50, rot.per=0.35, colors=brewer.pal(8, "Dark2")) # colorful wordcloud

###################################################################################
#Step 3: Determine how many positive words were in the speech
###################################################################################

# 6)	Scale the number based on the total number of words in the speech
# 7)	Hint: one way to do this is to use the 'match' function and then 'which' function
#-------------
#Get total word count
totalWords <- sum(wordCounts)
print('total count:')
print(totalWords)
#841


#Make a vector that just has all the words
allWords <- names(wordCounts)

#Make non positive words have counts=0
matchedPositiveCounts <- match(allWords, posWords, nomatch = 0) 

#Figure out the positive words
positiveOnlyCounts<-wordCounts[which(matchedPositiveCounts != 0)]

#Count positive words identified
positiveSpeechWords <- names(positiveOnlyCounts)
finalPosCount <- sum(positiveOnlyCounts)

print('Final Positive Count:')
print(finalPosCount)
#95


#Scale 
scalePos <- finalPosCount/totalWords

print('Scaled Positive Count:')
print(scalePos)
#0.1129608

#######################################################################
#Step 4: Determine how many negative words were in the speech
######################################################################
# 8)	Scale the number based on the total number of words in the speech
# 9)	Hint: this is basically the same as step (3)

#-------------

#Make non negative words have counts=0
matchedNegativeCounts <- match(allWords, negWords, nomatch = 0) 

#Figure out the positive words
negativeOnlyCounts<-wordCounts[which(matchedNegativeCounts != 0)]

#Count positive words identified
negativeSpeechWords <- names(negativeOnlyCounts)
finalNegCount <- sum(negativeOnlyCounts)

print('Final Negative Count:')
print(finalNegCount)
#63


#Scale
scaleNeg <- finalNegCount/totalWords


print('Scaled Negative Count:')
print(scaleNeg)
#0.07491082


###############################################################################
#Step 5: Redo the 'positive' and 'negative' calculations for each 25% of the speech
####################################################################################
# 10)	Compare the results  (ex. a simple barchart of the 4 numbers).
#-------------
redoAssignment<-function(givenSpeech)
{
  #read full file
  posWords <- scan("positive-words.txt", character(0),sep = "\n")
  #remove the introductory header
  posWords <- posWords[-1:-34]
  #read full file
  negWords <- scan("negative-words.txt", character(0),sep = "\n") 
  #remove the introductory header
  negWords <- negWords[-1:-34]
  #Enable reading sentences as vectors
  speechSentenceFeed<-VectorSource(givenSpeech)
  #Convert the speech into text corpus
  speechCorpus<-Corpus(speechSentenceFeed)
  #Clean the text: standardize by converting to lower case
  speechCorpus <- tm_map(speechCorpus, content_transformer(tolower))
  #Clean the text: standardize by removing punctuation
  speechCorpus <- tm_map(speechCorpus, removePunctuation)
  #Clean the text: standardize by removing numbers
  speechCorpus <- tm_map(speechCorpus, removeNumbers)
  #Clean the text: standardize by dropping common words
  speechCorpus <- tm_map(speechCorpus, removeWords, stopwords("english"))
  #Creating the term matrix
  termMatrix <- TermDocumentMatrix(speechCorpus)
  #Create and print word counts
  wordCounts <- rowSums(as.matrix(termMatrix))
  #Get total word count
  totalWords <- sum(wordCounts)
  #Make a vector that just has all the words
  allWords <- names(wordCounts)
  #Make non positive words have counts=0
  matchedPositiveCounts <- match(allWords, posWords, nomatch = 0) 
  #Figure out the positive words
  positiveOnlyCounts<-wordCounts[which(matchedPositiveCounts != 0)]
  #Count positive words identified
  positiveSpeechWords <- names(positiveOnlyCounts)
  finalPosCount <- sum(positiveOnlyCounts)
  #Scale 
  scalePos <- finalPosCount/totalWords
  #Make non negative words have counts=0
  matchedNegativeCounts <- match(allWords, negWords, nomatch = 0) 
  #Figure out the positive words
  negativeOnlyCounts<-wordCounts[which(matchedNegativeCounts != 0)]
  #Count positive words identified
  negativeSpeechWords <- names(negativeOnlyCounts)
  finalNegCount <- sum(negativeOnlyCounts)
  #Scale
  scaleNeg <- finalNegCount/totalWords
  return(c(scalePos,scaleNeg)) 
}

#evaluate assignment for different parts of the speech
first=redoAssignment(speech[1:7])
second=redoAssignment(speech[8:14])
third=redoAssignment(speech[15:21])
fourth=redoAssignment(speech[22:29])

print('Positive and Negative in part first 25%:')
print(first)
#[1] 0.09717868 0.10344828
print('Positive and Negative in part second 25%:')
print(second)
#[1] 0.11836735 0.08163265
print('Positive and Negative in part third 25%:')
print(third)
#[1] 0.09219858 0.06382979
print('Positive and Negative in part last 25%:')
print(fourth)
#[1] 0.161764706 0.007352941

 
#plot the outcome
quarters <- c('Quar1','Quar2','Quar3','Quar4')
positiveQCount <- c(first[1],second[1],third[1],fourth[1])
negativeQCount <- c(first[2],second[2],third[2],fourth[2])

df1 <- data.frame(quarters,positiveQCount)
df2 <- data.frame(quarters,negativeQCount)

#Plot
ggplot(df1,aes(x=quarters,y=positiveQCount)) + geom_bar(stat = 'identity',color='red',fill='black')
ggplot(df2,aes(x=quarters,y=negativeQCount)) + geom_bar(stat = 'identity',color='black',fill='blue')

#The plot shows that wiht increasing quarters:
#  1. the percentage of positive words are highest in last quarter and second highest in second quarter
#  2. the percentage of negative words decreases.
