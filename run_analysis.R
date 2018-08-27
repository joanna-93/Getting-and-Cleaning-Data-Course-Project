setwd("C:/Users/joanna.aksiuto/Desktop/PD/Data Science Specialization/3. Getting & Cleaning Data")

## 1 Merge the training and the test sets to create one data set

## TRAINING DATA
## a) Read and label subject_train txt file

subject_train <- read.table(file="./UCI HAR Dataset/train/subject_train.txt",header=F)
str(subject_train)
names(subject_train)=c("subjectID")
head(subject_train)

## b) Read X_train txt file

X_train <- read.table(file="./UCI HAR Dataset/train/X_train.txt",header=F)
head(X_train)

## c) Read and label y_train file 

y_train <- read.table(file="./UCI HAR Dataset/train/y_train.txt",header=F)
names(y_train) <- c("activityID")
head(y_train)

## d) Merge all 3 training files into one dataset

trainingData <- cbind(subject_train,y_train, X_train)
head(trainingData)[1:2]

## TEST DATA
## a) Read and label subject_test txt file

subject_test <- read.table(file="./UCI HAR Dataset/test/subject_test.txt",header=F)
names(subject_test)=c("subjectID")
head(subject_test)

## b) Read X_test txt file

X_test <- read.table(file="./UCI HAR Dataset/test/X_test.txt",header=F)
head(X_test)

## c) Read and label y_test file

y_test <- read.table(file="./UCI HAR Dataset/test/y_test.txt",header=F)
names(y_test) <- c("activityID")
head(y_test)

## d) Merge all 3 test files into one dataset

testData <- cbind(subject_test, y_test, X_test)
head(testData)[1:2]

## COMBINED DATA SET
## Combine training and test dataset together

fullData <- rbind(trainingData,testData)
head(fullData)[1:3]

## 2 Extract only the measurements on the mean and standard deviation for each observation

## a) Reading features file into R and storing is as a data table

labelsFull <- read.table(file="./UCI HAR Dataset/features.txt",header=F)
features <- labelsFull[,2] ## extract only the column that contains label names
class(features)


library(data.table)
DT <- data.table(features)
DT

## b) Extracting rows from features table that contains value "mean" or "std"

mean <- grep("mean",DT$features)
length(mean)

std <- grep("std",DT$features)
length(std)

## c) Creating a data table that contains labels for measures on the mean and standard dev

xLabels <- DT[c(mean, std), ]

## d) Limit fullData data set to the relevant columns

measures <- fullData[,c(mean,std)+2] ## +2 is added as the first 2 columns in fullData are subjectID and activityID
colnames(measures) <- t(xLabels) ## transposing xLabels so that dimentions of measures and xLabels are equal 

## e) Create labelled dataset with measurements on mean and std dev

dataset <- cbind(fullData[,1:2],measures)
dataset

## 3 Use descriptive activity names to names the activities in the data set

## a) Read activity_labels file into R

activityLabels <- read.table(file="./UCI HAR Dataset/activity_labels.txt",header=F)
names(activityLabels) <- c("activityID","activityName")
activityLabels

## b) Do the join between dataset and activityLabels

library(plyr)
data <- data.table(arrange(join(dataset,activityLabels),activityID))

##c) Replace activityID column with activityName

library(dplyr)
data <- select(data, subjectID, -(activityID), activityName, 3:81) ## arrange the columns in the correct order

## 4 Label the data set with descriptive variavle names

## Rename the variables that start with t/f to time/frequency 

names(data) <- gsub("^[t]","time",names(data))
names(data) <- gsub("^[f]","frequency",names(data))
data

## 5 From the data set in step 4 create a second, independent tidy data set with the average
  ## of each variable for each activity and each subject

View(filter(data,subjectID==1)) ## look at the data for one subject
colSums(is.na(data)) ##check for missing values

dataGrouped <-
data %>%
  group_by(subjectID,activityName) %>%
  summarize_all(funs(mean)) %>%
print(dataGrouped)  
  
View(dataGrouped)
 
write.table(dataGrouped, file="tidydata.txt", col.names= T, row.names = F)
read.table("tidydata.txt",header = T)
 
      