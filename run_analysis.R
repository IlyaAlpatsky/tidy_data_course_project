# There are 3 functions one to download and unzip files  
# and 2 to generate 2 tidy datasets
#
#

#
# Main script  save data into text filesa
#
data_preparation_script <- function()
{
  # download data and extract all files from zip
  download_samsung_data()
  # prepare tidy data with mixed training and test data about all averages 
  all_mean_std_data <- run_analysis()
  # prepare grouped averages for data grouped by subject and by activity
  averages_data <- averages_by_subject_and_activity(all_mean_std_data)
  # write tidy data and grouped tidy data into csv files
  write_datasets(all_mean_std_data,averages_data)
  # Only write datasets 
 
}

#
# Load data from extracted files into datasets, prepare 2 tidy datasets and save them
#
run_analysis <- function() 
{
  # Read activity labels dictionary 
  act_labels<-read.csv("./UCI HAR Dataset/activity_labels.txt", header = F,sep = "")
  act_labels[,2] <- tolower(act_labels[,2])
  
  # Read features dictonary 
  feat_labels <- read.csv("./UCI HAR Dataset/features.txt", header = F,sep = "")
  
  # Read test subject from ./UCI HAR Dataset/test/subject_test.txt file
  subject_test <- read.csv("./UCI HAR Dataset/test/subject_test.txt",header = F,sep = "")
  
  # Read test labels from ./UCI HAR Dataset/test/y_test.txt
  test_labels <- read.csv("./UCI HAR Dataset/test/y_test.txt",header = F,sep = "")
  
  # Read test set from ./UCI HAR Dataset/test/X_test.txt
  test_set <- read.csv("./UCI HAR Dataset/test/X_test.txt",header = F,sep = "")
  
  # Read training subject from ./UCI HAR Dataset/train/subject_train.txt file
  subject_train <- read.csv("./UCI HAR Dataset/train/subject_train.txt",header = F,sep = "")

  # Read training labels from ./UCI HAR Dataset/train/y_train.txt
  train_labels <- read.csv("./UCI HAR Dataset/train/y_train.txt",header = F,sep = "")
  
  # Read training set from  ./UCI HAR Dataset/train/X_test.txt
  train_set <- read.csv("./UCI HAR Dataset/train/X_train.txt",header = F,sep = "")
  
  #bind test data into one dataframe
  test_binded <- cbind.data.frame(subject_test, test_labels,test_set)
  #bind train data into one dataframe
  train_binded <- cbind.data.frame(subject_train, train_labels,train_set)
  
  #bind test and train data together into one table
  all_data <- rbind(test_binded,train_binded)
  
  #prepare tidy data about any given window mean and standard deviation
  #extract only feature lebels to simlify columns labeling
  feat_labels <- as.character(feat_labels[,2])
  feat_labels <- gsub("-", "_", feat_labels)
  feat_labels<-gsub("([()])", "", feat_labels)
  
  #set interesting for tidy dataset features ids
  feat_lst <- c(1,2,3,4,5,6, 
               41,42,43,44,45,46, 
               81,82,83,84,85,86,
               121,122,123,124,125,126,
               161,162,163,164,165,166,
               201,202,
               214,215,
               227,228,
               240,241,
               253,254,
               266,267,268,
               269,270,271,
               345,346,347,
               348,349,350,
               424,425,426,
               427,428,429,
               503,504,
               516,517,
               529,530,
               542,543)
  
  #form tidy data.frame
  all_mean_std_tidy_data <- data.frame(all_data[,1], act_labels[all_data[,2],2],
                                       all_data[,2+feat_lst])
  #make column names of tidy data.frame informative                          
  colnames(all_mean_std_tidy_data)<-c("Subject_code","Activity_name",
                                        feat_labels[feat_lst])
                                          
  
  run_analysis <- all_mean_std_tidy_data
  
}

#
# Calculate average of all variables for each activity and each subject
#

averages_by_subject_and_activity <- function(tidy_activity_data_set)
{
  averages_by_subject_and_activity <- group_by(tidy_activity_data_set, Subject_code, Activity_name) %>% 
    summarise(mean_tBodyAcc_mean_X = mean(tBodyAcc_mean_X),
              mean_tBodyAcc_mean_Y = mean(tBodyAcc_mean_Y),
              mean_tBodyAcc_mean_Z = mean(tBodyAcc_mean_Z),
              mean_tBodyAcc_std_X = mean(tBodyAcc_std_X),
              mean_tBodyAcc_std_Y = mean(tBodyAcc_std_Y),
              mean_tBodyAcc_std_Z = mean(tBodyAcc_std_Z),
              mean_tGravityAcc_mean_X = mean(tGravityAcc_mean_X),
              mean_tGravityAcc_mean_Y = mean(tGravityAcc_mean_Y),
              mean_tGravityAcc_mean_Z = mean(tGravityAcc_mean_Z),
              mean_tGravityAcc_std_X = mean(tGravityAcc_std_X),
              mean_tGravityAcc_std_Y = mean(tGravityAcc_std_Y),
              mean_tGravityAcc_std_Z = mean(tGravityAcc_std_Z),
              mean_tBodyAccJerk_mean_X = mean(tBodyAccJerk_mean_X),
              mean_tBodyAccJerk_mean_Y = mean(tBodyAccJerk_mean_Y),
              mean_tBodyAccJerk_mean_Z = mean(tBodyAccJerk_mean_Z),
              mean_tBodyAccJerk_std_X = mean(tBodyAccJerk_std_X),
              mean_tBodyAccJerk_std_Y = mean(tBodyAccJerk_std_Y),
              mean_tBodyAccJerk_std_Z = mean(tBodyAccJerk_std_Z),
              mean_tBodyGyro_mean_X = mean(tBodyGyro_mean_X),
              mean_tBodyGyro_mean_Y = mean(tBodyGyro_mean_Y),
              mean_tBodyGyro_mean_Z = mean(tBodyGyro_mean_Z),
              mean_tBodyGyro_std_X = mean(tBodyGyro_std_X),
              mean_tBodyGyro_std_Y = mean(tBodyGyro_std_Y),
              mean_tBodyGyro_std_Z = mean(tBodyGyro_std_Z),
              mean_tBodyGyroJerk_mean_X = mean(tBodyGyroJerk_mean_X),
              mean_tBodyGyroJerk_mean_Y = mean(tBodyGyroJerk_mean_Y),
              mean_tBodyGyroJerk_mean_Z = mean(tBodyGyroJerk_mean_Z),
              mean_tBodyGyroJerk_std_X = mean(tBodyGyroJerk_std_X),
              mean_tBodyGyroJerk_std_Y = mean(tBodyGyroJerk_std_Y),
              mean_tBodyGyroJerk_std_Z = mean(tBodyGyroJerk_std_Z),
              mean_tBodyAccMag_mean = mean(tBodyAccMag_mean),
              mean_tBodyAccMag_std = mean(tBodyAccMag_std),
              mean_tGravityAccMag_mean = mean(tGravityAccMag_mean),
              mean_tGravityAccMag_std = mean(tGravityAccMag_std),
              mean_tBodyAccJerkMag_mean = mean(tBodyAccJerkMag_mean),
              mean_tBodyAccJerkMag_std = mean(tBodyAccJerkMag_std),
              mean_tBodyGyroMag_mean = mean(tBodyGyroMag_mean),
              mean_tBodyGyroMag_std = mean(tBodyGyroMag_std),
              mean_tBodyGyroJerkMag_mean = mean(tBodyGyroJerkMag_mean),
              mean_tBodyGyroJerkMag_std = mean(tBodyGyroJerkMag_std),
              mean_fBodyAcc_mean_X = mean(fBodyAcc_mean_X),
              mean_fBodyAcc_mean_Y = mean(fBodyAcc_mean_Y),
              mean_fBodyAcc_mean_Z = mean(fBodyAcc_mean_Z),
              mean_fBodyAcc_std_X = mean(fBodyAcc_std_X),
              mean_fBodyAcc_std_Y = mean(fBodyAcc_std_Y),
              mean_fBodyAcc_std_Z = mean(fBodyAcc_std_Z),
              mean_fBodyAccJerk_mean_X = mean(fBodyAccJerk_mean_X),
              mean_fBodyAccJerk_mean_Y = mean(fBodyAccJerk_mean_Y),
              mean_fBodyAccJerk_mean_Z = mean(fBodyAccJerk_mean_Z),
              mean_fBodyAccJerk_std_X = mean(fBodyAccJerk_std_X),
              mean_fBodyAccJerk_std_Y = mean(fBodyAccJerk_std_Y),
              mean_fBodyAccJerk_std_Z = mean(fBodyAccJerk_std_Z),
              mean_fBodyGyro_mean_X = mean(fBodyGyro_mean_X),
              mean_fBodyGyro_mean_Y = mean(fBodyGyro_mean_Y),
              mean_fBodyGyro_mean_Z = mean(fBodyGyro_mean_Z),
              mean_fBodyGyro_std_X = mean(fBodyGyro_std_X),
              mean_fBodyGyro_std_Y = mean(fBodyGyro_std_Y),
              mean_fBodyGyro_std_Z = mean(fBodyGyro_std_Z),
              mean_fBodyAccMag_mean = mean(fBodyAccMag_mean),
              mean_fBodyAccMag_std = mean(fBodyAccMag_std),
              mean_fBodyBodyAccJerkMag_mean = mean(fBodyBodyAccJerkMag_mean),
              mean_fBodyBodyAccJerkMag_std = mean(fBodyBodyAccJerkMag_std),
              mean_fBodyBodyGyroMag_mean = mean(fBodyBodyGyroMag_mean),
              mean_fBodyBodyGyroMag_std = mean(fBodyBodyGyroMag_std),
              mean_fBodyBodyGyroJerkMag_mean = mean(fBodyBodyGyroJerkMag_mean),
              mean_fBodyBodyGyroJerkMag_std = mean(fBodyBodyGyroJerkMag_std)
            )
}

#
# Download zip archive into working directory and extract it
#
  
download_samsung_data <- function() 
{
  destfile <- "./human_activity_from_samsung_dataset.zip"
  # if archive has not downloaded -- download it into working directory
  if (!file.exists(destfile))
  {
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip";
    download.file(fileURL, destfile, method = "curl");
    dateDownloaded <- date();
  }
  # extract archive
  unzip (destfile, exdir = "./");
}

#
# Write tidy data to text files
#
write_datasets <- function(mean_std_tidy_data , grouped_tidy_data)
{
  write.table(mean_std_tidy_data, row.names = F, "all_mean_std_tidy_data.txt")
  write.table(grouped_tidy_data, row.names = F, "grouped_mean_std_tidy_data.txt")
}
