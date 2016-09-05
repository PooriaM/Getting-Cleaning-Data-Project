## Getting and Cleaning Data Project


### Description
Additional information about the variables, data and transformations used in the course project for the Johns Hopkins Getting and Cleaning Data course.

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

[The source data for this project can be found here.](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Attribute Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

## Variables:   
* `X_train`, `Y_train`, `X_test`, `Y_test`, `subject_train` and `subject_test` contain the data from the downloaded files.
* `x_data`, `y_data` and `subject_data` merge the previous datasets to further analysis.
* `features` contains the names for the `X_test` and `X_train` datasets, which are applied to the column names in full_data.
* `activity_labels` contains the names for test lales 1:6 as "WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING", "STANDING","LAYING"
* `full_data` is a dataframe created after merging all source information
* `finaldata` is a dataframe created after subsetting `full_data` to get the mean() and std() columns only.
* `tidy_data` is a cleaned up `finaldata` with the average of each variable for each activity and each subject

## About R script

### Section 1. Merge the training and the test sets to create one data set.

step 1: download zip file from website

step 2: unzip data

step 3: load following files and read data into tables and assign column names.

- features.txt
- activity_labels.txt
- subject_train.txt
- x_train.txt
- y_train.txt
- subject_test.txt
- x_test.txt
- y_test.txt

step 4: merge train and test data (dataframe: full_data)

## Section 2. Extract only the measurements on the mean and standard deviation for each measurement. 
Searches through column names (features) to find the ones that contain "mean()" or "std()".
Subsets found columns from full_data and generates dataframe final_data

## Section 3. Use descriptive activity names to name the activities in the data set
step 1: introduce activity names

step 2: replace Test.lablels 1 to 6 with activity names

## Section 4. Appropriately label the data set with descriptive activity names.
Use gsub function for pattern replacement to clean up the data labels.

## Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 
Use "dplyr" package to group data by activity and subject then summarize each column to the mean of whole column. the result dataframe is tidy_data

## Output 
Write dataframe tidy_data to tidy_data.txt.
