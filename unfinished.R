## Name: MULETA, Rajyl P.
## Please consider that this project (run_analysis.R) is more of 
## a personal reminder to me and guide rather than a submission,
## that is why there are plenty of personall comments and reminders
## along the way. 

## Part i. Although this is not part of the assignment
## but I usually do this in all my programming practice.
## Programmers Do's before starting to code.
## A. Check your environment variables
ls()
## B. If there exists, try clearing them out.
rm(list = ls())
## C. Next, restart your r studio 
.rs.restartR()
## D. Get to know the directory you are working with
getwd()
## "/Users/lapuk/Data_Science/datasciencescoursera/Module-3-Exam"
## It is rather easier to start a new R project and
## declare a new directory than to setwd() a new one. 
## Again,to make the project neat -- including the variables on the memory, 
## start a new one. 

Part ii. Load the necessary library
library(plyr)
## https://cran.r-project.org/web/packages/plyr/index.html
## A set of tools that solves a common set of problems: 
## you need to break a big problem down into manageable pieces, 
## operate on each piece and then put all the pieces back together. 
library(knitr)
## https://cran.r-project.org/web/packages/knitr/index.html
## Provides a general-purpose tool for dynamic report 
## generation in R using Literate Programming techniques.

## Part iii. Checking, Creating and Unzipping the file to 
## the desired directory.
if(!file.exists("./dataFolder")){ 
                  dir.create("./dataFolder")
                  }
## setting fileLink to the target link of the file to be downloaded
fileLink <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

## checking if the file exist (zip) already exist inside the directory
if(!file.exists("./dataFolder/dataFoldersetDownloaded.zip")){
                  download.file(fileLink,destfile="./dataFolder/dataFoldersetDownloaded.zip",mode = "wb")
                  }
## if not, library(downloader) calls on the to function download.file with 
## parameters fileLink as the target to the destination folder/file in zip. 
## mode="wb https://stat.ethz.ch/R-manual/R-devel/library/utils/html/download.file.html
## Useful values are "w", "wb" (binary)

## check if the file is already unzip
## else unzip it at zipfile="/directory/file.zip to existing directory (exdir)
if(!file.exists("./dataFolder/UCI HAR Dataset")){
                  unzip(zipfile="./dataFolder/dataFoldersetDownloaded.zip",exdir="./dataFolder")
                  }

## Part iv. Checking and listing all the files inside the UCI HAR Dataset folder
pathFolder <- file.path("./dataFolder" , "UCI HAR Dataset")
filesFolder <- list.files(pathFolder, recursive=TRUE)


# ASSIGNMENT 1: Merges the training and the test sets to create one data set.
## Laoding the data inside the txt files to a variable  
## load /UCI HAR Dataset/test/Y_test.txt to loadedActivityTest 
## load /UCI HAR Dataset/train/Y_train.txt to loadedActivityTrain
loadedActivityTest  <- read.table(file.path(pathFolder, "test" , "Y_test.txt" ),header = FALSE)
loadedActivityTrain <- read.table(file.path(pathFolder, "train", "Y_train.txt"),header = FALSE)
## load the /UCI HAR Dataset/test/subject_tests.txt to loadedSubjectTest
## load the /UCI HAR Dataset/train/subject_train.txt to loadedSubjectTrain
loadedSubjectTest  <- read.table(file.path(pathFolder, "test" , "subject_test.txt"),header = FALSE)
loadedSubjectTrain <- read.table(file.path(pathFolder, "train", "subject_train.txt"),header = FALSE)
## load the /UCI HAR Dataset/test/X_test.text to laodedFeaturesTest
## load the /UCI HAR Dataset/train/X_train.txt to loadedFeaturesTrain
loadedFeaturesTest  <- read.table(file.path(pathFolder, "test" , "X_test.txt" ),header = FALSE)
loadedFeaturesTrain <- read.table(file.path(pathFolder, "train", "X_train.txt"),header = FALSE)
## After passing all the data inside the text files into data table,
## it is ime to combine the loaded data (by rows)
combinedActivity <- rbind(loadedActivityTrain, loadedActivityTest)
combinedSubject <- rbind(loadedSubjectTrain, loadedSubjectTest)
combinedFeatures <- rbind(loadedFeaturesTrain, loadedFeaturesTest)
## Assigning the data table combinedSubject to "subject" using the name() function. 
names(combinedSubject) <- c("subject")
## Assigning the data table combinedActivity to "activity" using the name() function.
names(combinedActivity) <- c("activity")
## Passing the values from the file "features.txt to combinedFeaturesNames
combinedFeaturesNames <- read.table(file.path(pathFolder, "features.txt"), head=FALSE)
## Assigning combinedFeatures to combinedFeaturesNames using the name() function.
names(combinedFeatures) <- combinedFeaturesNames$V2
## Combining the data tables combinedSubject and combinedActivity using cbind()
combineSubAct <- cbind(combinedSubject, combinedActivity)
## The combination of the subject under the combinedSubject
## and the combination of the activity under the combinedActivity
## is passed throught combinedSubAct
## which merge with combinedFeatures and pass to dataFinal
dataFinal <- cbind(combinedFeatures, combineSubAct)

# ASSIGNMENT 2: Extracts only the measurements on the mean and standard deviation for each measurement.
## Calling the names of the combinedFeatures with its measurements 
## Using mean() and std() deviation
subdataFeaturesNames <- combinedFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", combinedFeaturesNames$V2)]
## using as.character to concatenate subdataFeaturesNames for "subject" and "activity"
selectedNames <- c(as.character(subdataFeaturesNames), "subject", "activity" )
dataFinal <- subset(dataFinal, select=selectedNames)


# ASSIGNMENT 3: Uses descriptive activity names to name the activities in the data set
## Reading the "activity_labels.txt" and passing its values to activityLabels
activityLabels <- read.table(file.path(pathFolder, "activity_labels.txt"), header = FALSE)
dataFinal$activity <- factor(dataFinal$activity, labels = activityLabels[,2])


# ASSIGNMENT 4: Appropriately labels the data set with descriptive variable names
## Using gsub(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
## Pattern Matching and Replacement
names(dataFinal)<-gsub("^t", "time", names(dataFinal))
names(dataFinal)<-gsub("^f", "frequency", names(dataFinal))
names(dataFinal)<-gsub("Acc", "Accelerometer", names(dataFinal))
names(dataFinal)<-gsub("Gyro", "Gyroscope", names(dataFinal))
names(dataFinal)<-gsub("Mag", "Magnitude", names(dataFinal))
names(dataFinal)<-gsub("BodyBody", "Body", names(dataFinal))


# ASSIGMENT 5: From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyData <- aggregate(. ~subject + activity, dataFinal, mean)
tidyData <- tidyData[order(tidyData$subject, tidyData$activity),]
write.table(tidyData, file = "tidydata.txt", row.name=FALSE,quote = FALSE, sep = '\t')

