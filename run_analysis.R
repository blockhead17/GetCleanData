##Getting and Cleaning Data
##Course assignment
#run_analysis.R

#Empty the console and environment
cat("\014")
rm(list=ls()) 

## Set the working directory
setwd("~/Documents/R Working Directory")

## Get data
## If file UCI HAR Dataset.zip does not exist, download it
zipfile <- "UCI HAR Dataset.zip"
if (!file.exists(zipfile)) {
      zipurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
      download.file(url = zipurl, destfile = zipfile, method = "curl")
}

## Extract files from the zip archive
## If extracted files already exist, do not overwrite them
unzip(zipfile, overwrite = FALSE)
      
library(data.table)
library(plyr)
library(dplyr)

##Read in the relevant data files; default delimiter is white space,
##which seems appropriate after viewing data.
subjTrain<-read.table("./UCI HAR Dataset/train/subject_train.txt",header=FALSE)
subjTest<-read.table("./UCI HAR Dataset/test/subject_test.txt",header=FALSE)
yTrain<-read.table("./UCI HAR Dataset/train/y_train.txt",header=FALSE)
yTest<-read.table("./UCI HAR Dataset/test/y_test.txt",header=FALSE)
xTrain<-read.table("./UCI HAR Dataset/train/x_train.txt",header=FALSE)
xTest<-read.table("./UCI HAR Dataset/test/x_test.txt",header=FALSE)
activityLabels<-read.table("./UCI HAR Dataset/activity_labels.txt",header=FALSE)
features<-read.table("./UCI HAR Dataset/features.txt",header=FALSE)

##Add the test data rows to the training data rows at the bottom
subjStack=rbind(subjTrain,subjTest)
xStack=rbind(xTrain,xTest)
yStack=rbind(yTrain,yTest)

##Add some sensible variable names to the data set
setnames(activityLabels,1:2,c("activityID","activityName"))
setnames(features,1:2,c("featureID","featureName"))
setnames(subjStack,1,c("subjectID"))
featureVector<-as.character(features$featureName)
setnames(xStack,1:561,featureVector)
setnames(yStack,1,c("activityID"))

##Trim the measurements data to just include simple means and standard deviations.
##These variables contain "mean()" and "std()" as part of their names per the 
##data documentation.
xStackTrim<-xStack[,grep("mean\\(\\)|std\\(\\)", colnames(xStack))]

##Add a variable representing the row number to prep for merging data frames
subjStack<-mutate(subjStack,obsnum=as.integer(rownames(subjStack)))
xStackTrim<-mutate(xStackTrim,obsnum=as.integer(rownames(xStackTrim)))
yStack<-mutate(yStack,obsnum=as.integer(rownames(yStack)))

##Create a data frame representing the full dataset
dfList=list(subjStack,xStackTrim,yStack)
fullStack=join_all(dfList,by="obsnum")
fullStack=join(fullStack,activityLabels,by="activityID")

##Drop the activityID and obsnum variables.  They are no longer needed after the join.
dropvars <- names(fullStack) %in% c("activityID", "obsnum")
fullStack <- fullStack[!dropvars]

##Create an independent tidy data set with the average of each variable for each
#activity and each subject.  A tidy data set meets three key criteria: 
      #1. Each variable you measure should be in one column. 
      #2. Each different observation of that variable should be in a different row.
      #3. There should be one table for each "kind" of variable. This tidy table
      #     contains all summary statistics of interest.

library(dplyr)
#Get summary statistics across compound groups using dplyr
groupColumns<-c("activityName","subjectID")
dots <- lapply(groupColumns, as.symbol)
tidyData <- fullStack %>%
      group_by_(.dots=dots) %>%
      summarise_each(funs(mean))

#Write the tidy data to a text file
write.table(tidyData,file="tidyData.txt",row.names = FALSE)
