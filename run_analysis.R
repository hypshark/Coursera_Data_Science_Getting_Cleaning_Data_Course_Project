# Coursera - Getting and Cleaning Data
# John Hopkins University- Final Course Project
# Author- Mark McKowen, 5 November 2016
# Wearable Computing Data Acquisition/ Tidying

# note, the practice below is used extensively by the Prof Leek in Lecture,
# if(!=file.exists("./data")){dir.create("./data)}

# UCI HAR Dataset
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
path1 <- "./R/Coursera/Getting and Setting Data- JHU/dataset"
download.file(fileUrl, path1)
unzip(path1)
# Training Data Set
x_train <- read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header=F)
y_train <- read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header=F)
subject_train <- read.csv("UCI HAR Dataset/train/subject_train.txt", sep= "", header = F)

# Testing Data Set
x_test <- read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=F)
y_test <- read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=F)
subject_test <- read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=F)
# Feature Labeling
Features = read.csv("UCI HAR Dataset/features.txt", sep="", header=F)
# Read Activity Labels
Labels <- read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header=F)
# Analysis of Labels
names(Labels)
head(Labels) # cuts off at 6, which is default, test for further Lables by specifying larger number
head(Labels, n= 10) # confirmation of 6 total labels
# combine testing, and training by rows using rbind
TestTrain <- rbind(testing, training) 

# Assign Names to Columns
# §1- training
colnames(x_train) <- Features[,2]
colnames(y_train) <- "ActivityId"
colnames(subject_train) <- "SubjectID"
# §2 testing
colnames(x_test) <- Features[,2]
colnames(y_test) <- "ActivityId"
colnames(subject_test) <- "SubjectID"

# OBJECTIVE #1
# Merge into one dataset
TrainMerge <- cbind(y_train, subject_train, x_train)
TestMerge <- cbind(y_test, subject_test, x_test)
TestTrain <- rbind(TrainMerge, TestMerge)

#OBJECTIVE #2 EXTRACT ONLY MEAN and STDEV per MEASUREMENT
colNames <- colnames(TestTrain)
# specify only the colnames of TestTrain
str(colnames(TestTrain))
# yields lengthy list of names, we are interested in Mean and STDev only


MeanSTD <- (grepl("ActivityId", colNames) |
              grepl("SubjectID", colNames) |
               grepl("mean..", colNames) |
               grepl("std..", colNames)
            )
# Above Thanks to Coursera "Geting and Cleaning Data §Week4 lecture- "editing text variables" (J.Leek), 
# and rpubs.com (Zanin Pavel). the grepl function serves as a very concise way to set up our mean and stdev

MeanSTDSet <- TestTrain [, MeanSTD == T]
head(MeanSTDSet)
str(MeanSTDSet)

#OBJECTIVE #3 & #4 USE DESCRIPTIVE ACTIVITY NAMEs TO NAME ACTIVITIES IN DATA SET
#----------------------------------------------------------------------------
ActivityNameSet <- merge(MeanSTDSet, Labels,all.x= T)

#OBJECTIVE #5 CREATE 2nd INDEPENDENT TIDY DATA SET WITH AVG of EACH VARIABLE
#----------------------------------------------------------------------------
TidySet <- aggregate(.~SubjectID + ActivityId, ActivityNameSet, mean)
TidySet <- TidySet[order(TidySet$SubjectID, TidySet$ActivityId), ]
table(is.na(TidySet)) # no readings marked as "NA"
summary(TidySet)

#Submission Objective: Create txt file and submit.
write.table(TidySet, "TidySet.txt", row.names = F)
#-----------------------------------------------------------------------
#Further Improvements (time permitting): 
# 1. Instead use/implement gather() and spread() functioning from the 'tidyr' package: 
#    My original revision attempted to use this package, but I lost data (due to programmer error)
# 2. Evaluate "extreme" data variables (not enough information to perform this),
#    but check for illogical outliers within scope of observation
#-----------------------------------------------------
#Acknowledgements: 
# https://www.coursera.org/learn/data-cleaning (author: Jeff Leek, Roger Peng, and Brian Caffo)
# https://www.datacamp.com
# https://www.rpubs.com (author: Zanin Pavel)
