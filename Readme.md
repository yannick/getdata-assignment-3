Getting and Cleaning Data
=========================

Assessment 3
============

run_analysis.R takes the data from
https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
(original source: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones )

and creates a tidy dataset as defined in the assigment:

> You should create one R script called run_analysis.R that does the following. 
> 	- Merges the training and the test sets to create one data set.
> 	- Extracts only the measurements on the mean and standard deviation for each measurement. 
> 	- Uses descriptive activity names to name the activities in the data set
> 	- Appropriately labels the data set with descriptive variable names. 
> 	- From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
> 	

the order is not followed to make the code more reusable.

the result is a file called "summary_dataset.txt"