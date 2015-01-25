
#this function loads the data from either the test or training set, and
# - Uses descriptive activity names to name the activities in the data set as per step 3 of the assignment
# - sets Appropriately labels the data set with descriptive variable names.  as per step 4 of the assignment
load_data <- function(dataset_name) {
  
  activity_labels = read.table("activity_labels.txt")
  stopifnot(nrow(activity_labels) == 6) #6 activity labels
  names(activity_labels) <- c("id", "activity")
  
  features = read.table("features.txt") 
  stopifnot(nrow(features) == 561) #561 features

  
  #reading the data
  
  data_file = sprintf("%s/X_%s.txt", dataset_name, dataset_name) #creates e.g. test/X_test.txt
# paste0(dataset_name, "/X_", dataset_name, ".txt") #creates e.g. test/X_test.txt

  data_set_raw = read.table(data_file)
#  stopifnot(nrow(data_set_raw) == 7352) #7352 observations in training set
  #Appropriately labels the data set with descriptive variable names.
  names(data_set_raw) = as.character(features[,2])  

  # reading the subject file
  subject_file = sprintf("%s/subject_%s.txt", dataset_name, dataset_name) 
  subjects = read.table(subject_file)
  names(subjects) = "subject_id"

  #reading the activity mapping
  activity_mapping_file = sprintf("%s/y_%s.txt", dataset_name, dataset_name)
  row_to_activity_id = read.table(activity_mapping_file)
  stopifnot(nrow(row_to_activity_id) == nrow(data_set_raw)) #labels 
  names(row_to_activity_id) <- c("id")
  activity = factor(row_to_activity_id[,1])
  
  #Uses descriptive activity names to name the activities in the data set
  levels(activity) = as.character(activity_labels[,2]) #rename the levels from numbers to the names from activity_labels
  
  data_with_activity = cbind(data_set_raw, activity, subjects)
  return(data_with_activity)
}

training_set <- load_data("train")
test_set <- load_data("test")


combined_set <- rbind(training_set, test_set)


#now step 2:
#Extract only the measurements on the mean and standard deviation for each measurement. 

only_means_and_std <- combined_set[,grep("mean|std|activity|subject_id",names(combined_set))]

#use this...

#now step 5:
#From the data set in step 4, creates a second, 
#independent tidy data set with the average of each variable for each activity and each subject.
summary_dataset <- aggregate(only_means_and_std[,1:79],by=list("subject_id" = only_means_and_std$subject_id, "activity" = only_means_and_std$activity),FUN=mean, na.rm=TRUE)

#write the file
write.table(summary_dataset, "summary_dataset.txt", row.names=FALSE)



