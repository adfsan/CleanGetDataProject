run_analysis <- function(){
  # run_analysis.R
  # This R script loads the multiple tables available within the folders inside the UCI HAR Dataset folder and returns a tidy dataset as
  # specified by the course project assignment
  # This script uses:
  #
  # dplyr package
  #
  # load necessary packages
  library(dplyr)
  
  #############################################################################################
  ################################### STEP 1 OF THE PROJECT####################################
  #############################################################################################
  
  # stores each available table in a data frame
  # training set
  X_train <- as_data_frame(read.table("UCI HAR Dataset/train/X_train.txt"))
  y_train <- as_data_frame(read.table("UCI HAR Dataset/train/y_train.txt"))
  subject_train <- as_data_frame(read.table("UCI HAR Dataset/train/subject_train.txt"))
  # test set
  X_test <- as_data_frame(read.table("UCI HAR Dataset/test/X_test.txt"))
  y_test <- as_data_frame(read.table("UCI HAR Dataset/test/y_test.txt"))
  subject_test <- as_data_frame(read.table("UCI HAR Dataset/test/subject_test.txt"))
  
  # binding the data together
  training_set <- as_data_frame(bind_cols(subject_train,y_train, X_train))
  test_set <- as_data_frame(bind_cols(subject_test,y_test, X_test))
  dataset <- rbind(training_set,test_set)
  
  # naming the columns (before extraction)
  features <- read.table("UCI HAR Dataset/features.txt")
  features <- c("subject","activity",as.character(features[,2]))
  
  names(dataset) <- make.names(features,unique = TRUE) #prevents R from thinking there are duplicate feature names
  
  dataset <- arrange(dataset,subject,activity) #sorts dataset by ascending order of subject and activity
  
  # grouping the data by subject and activity
  
  data_group <- group_by(dataset,subject,activity)
  
  #############################################################################################
  ################################### STEP 2 OF THE PROJECT####################################
  #############################################################################################
  
  # summarise
  # apply the mean function to the variables that represent the mean and standard deviation for
  # each measurment
  
  dataset_tidy <- select(data_group, contains(".mean."),contains(".std."))
  dataset_tidy <- summarise_each(dataset_tidy,funs(mean))
  
  #############################################################################################
  ################################### STEP 3 OF THE PROJECT####################################
  #############################################################################################
  
  # replaces the numbers in the activity column with the activity names according to the 
  # activity_labels.txt file
  
  actv_lbl = read.table("UCI HAR Dataset/activity_labels.txt")
  actv_lbl <- as.character(actv_lbl[,2])
  dataset_tidy$activity <- actv_lbl[dataset_tidy$activity]
  
  #############################################################################################
  ################################### STEP 4 OF THE PROJECT####################################
  #############################################################################################
  
  # makes the variable names more readable by removing the excess of . (dots)
  
  features_new <- gsub("\\.\\.\\.","\\.",names(dataset_tidy))
  features_new <- gsub("\\.\\.","\\.",features_new)
  features_new[3:68] <- paste("avg", features_new[3:68])
  names(dataset_tidy) <- features_new
  
  #############################################################################################
  ################################### STEP 5 OF THE PROJECT####################################
  #############################################################################################
  
  # writes the new tidy dataset in a .txt file
  
  write.table(dataset_tidy,"dataset_tidy.txt",row.name=FALSE)
  return(dataset_tidy)
}
