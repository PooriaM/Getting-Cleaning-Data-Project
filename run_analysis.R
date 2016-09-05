dir.create("tiday_data_project")
setwd("tiday_data_project")
#-------------------------------------------------------------------------------
# 1. Merge the training and the test sets to create one data set.

## step 1: download zip file from website
url<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%
20Dataset.zip"
download.file(url,destfile = "zipped.zip")

## step 2: unzip data
unzip("zipped.zip")

## step 3: load data into R
features<-read.table("UCI HAR Dataset/features.txt",stringsAsFactors = FALSE)[,2]
X_train<-read.table("UCI HAR Dataset/train/X_train.txt",col.names = features)
Y_train<-read.table("UCI HAR Dataset/train/Y_train.txt",col.names = "Activity")
subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt",
                          col.names = "Subject")
X_test<-read.table("UCI HAR Dataset/test/X_test.txt",col.names = features)
Y_test<-read.table("UCI HAR Dataset/test/Y_test.txt",col.names = "Activity")
subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt",
                         col.names = "Subject")

## step 4: merge train and test data
test_data<-cbind(subject_test,Y_test,X_test)
train_data<-cbind(subject_train,Y_train,X_train)
full_data<-rbind(test_data,train_data)

#-------------------------------------------------------------------------------
# 2. Extract only the measurements on the mean and standard deviation for each 
#    measurement. 

## step 1:  extract mean and standard deviation of each measurements
features_index<-grep(("mean\\(\\)|std\\(\\)"),features)
finaldata<-full_data[,c(1,2,features_index+2)]

#-------------------------------------------------------------------------------
#3.Uses descriptive activity names to name the activities in the data set


##step 1: introduce activity names
activity_labels<-c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING",
                   "STANDING","LAYING")
##step 2: replace Test.lablels 1 to 6 with activity names
finaldata$Activity<-factor(finaldata$Activity,labels = activity_labels)

#-------------------------------------------------------------------------------
#4.Appropriately labels the data set with descriptive variable names. 

names(finaldata)<-gsub("mean","-Mean",names(finaldata))
names(finaldata)<-gsub("std","-Std",names(finaldata))
names(finaldata)<-gsub("*\\.","",names(finaldata))
names(finaldata)<- gsub("^t", "time", names(finaldata))
names(finaldata)<- gsub("^f", "frequency", names(finaldata))
names(finaldata)<- gsub("X","-X", names(finaldata))
names(finaldata)<- gsub("Y","-Y", names(finaldata))
names(finaldata)<- gsub("Z","-Z", names(finaldata))
#-------------------------------------------------------------------------------
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity 

library(dplyr)
tidy_data<-finaldata%>%group_by(Activity,Subject)%>%summarise_each(funs(mean))
#-------------------------------------------------------------------------------
#create output data: tidy_data:
write.table(tidy_data,"tidy_data.txt",row.names = FALSE)