library(dplyr)

## download file
setwd("C:/Users/RPan163353/Documents/RStudio/getting-and-cleaning-data")
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, "getdata_projectfiles_UCI HAR Dataset.zip")
unzip("getdata_projectfiles_UCI HAR Dataset.zip", exdir = "UCI HAR Dataset")
files <- list.files("UCI HAR Dataset", full.names = TRUE)

## set working directory
setwd("C:/Users/RPan163353/Documents/RStudio/getting-and-cleaning-data/UCI HAR Dataset/UCI HAR Dataset")

## read files
activity_labels <- read.table("./activity_labels.txt",header = FALSE)
features <- read.table("./features.txt",header = FALSE)

x_test <- read.table("./test/X_test.txt",header = FALSE)
y_test <- read.table("./test/y_test.txt",header = FALSE)
subjects_test <- read.table("./test/subject_test.txt",header=FALSE)
body_acc_x_test <- read.table("./test/Inertial Signals/body_acc_x_test.txt",header = FALSE)
body_acc_y_test <- read.table("./test/Inertial Signals/body_acc_y_test.txt",header = FALSE)
body_acc_z_test <- read.table("./test/Inertial Signals/body_acc_z_test.txt",header = FALSE)
body_gyro_x_test <- read.table("./test/Inertial Signals/body_gyro_x_test.txt",header = FALSE)
body_gyro_y_test <- read.table("./test/Inertial Signals/body_gyro_y_test.txt",header = FALSE)
body_gyro_z_test <- read.table("./test/Inertial Signals/body_gyro_z_test.txt",header = FALSE)
total_acc_x_test <- read.table("./test/Inertial Signals/total_acc_x_test.txt",header = FALSE)
total_acc_y_test <- read.table("./test/Inertial Signals/total_acc_y_test.txt",header = FALSE)
total_acc_z_test <- read.table("./test/Inertial Signals/total_acc_z_test.txt",header = FALSE)

x_train <- read.table("./train/X_train.txt",header = FALSE)
y_train <- read.table("./train/y_train.txt",header = FALSE)
subjects_train <- read.table("./train/subject_train.txt",header=FALSE)
body_acc_x_train <- read.table("./train/Inertial Signals/body_acc_x_train.txt",header = FALSE)
body_acc_y_train <- read.table("./train/Inertial Signals/body_acc_y_train.txt",header = FALSE)
body_acc_z_train <- read.table("./train/Inertial Signals/body_acc_z_train.txt",header = FALSE)
body_gyro_x_train <- read.table("./train/Inertial Signals/body_gyro_x_train.txt",header = FALSE)
body_gyro_y_train <- read.table("./train/Inertial Signals/body_gyro_y_train.txt",header = FALSE)
body_gyro_z_train <- read.table("./train/Inertial Signals/body_gyro_z_train.txt",header = FALSE)
total_acc_x_train <- read.table("./train/Inertial Signals/total_acc_x_train.txt",header = FALSE)
total_acc_y_train<- read.table("./train/Inertial Signals/total_acc_y_train.txt",header = FALSE)
total_acc_z_train <- read.table("./train/Inertial Signals/total_acc_z_train.txt",header = FALSE)

## rename columns
names(x_train) <- features$V2
names(x_test) <- features$V2
names(y_train) <- "activity"
names(y_test) <- "activity"
names(subjects_train) <- "subjects"
names(subjects_test) <- "subjects"

##reconstruct the train and test data sets
dat_test <- cbind(x_test, y_test, subjects_test)
dat_train <- cbind(x_train, y_train, subjects_train)

## 1. Merges the training and the test sets to create one data set.
dat <- rbind(dat_test, dat_train)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
mean_std_measurement <- grep("[Mm]ean|std", names(dat), value = TRUE)
dat_mean_std_measurement <- dat[,mean_std_measurement]

## 3. Uses descriptive activity names to name the activities in the data set
for (i in 1:length(dat$activity)) {
    r = dat[i,]$activity
    dat[i,]$activity = as.character(activity_labels[r,]$V2)
}

## 4. Appropriately labels the data set with descriptive variable names.
names(dat) <- gsub("tBodyAcc-","body acceleration signal in time domain from the accelerometer", names(dat))
names(dat) <- gsub("tGravityAcc-","gravity acceleration signal in time domain from the accelerometer", names(dat))
names(dat) <- gsub("tBodyAccJerk-","body acceleration jerk signal in time domian  from the accelerometer", names(dat))
names(dat) <- gsub("tBodyGyro-","body acceleration signal in time domain from the gyroscope", names(dat))
names(dat) <- gsub("tBodyGyroJerk-","body acceleration jerk signal in time domain from the gyroscope", names(dat))
names(dat) <- gsub("tBodyAccMag-","body acceleration signal in time domain applied to Fast Fourier Transform from the accelerometer", names(dat))
names(dat) <- gsub("tGravityAccMag-","gravity acceleration signal in time domain applied to Fast Fourier Transform from the accelerometer", names(dat))
names(dat) <- gsub("tBodyAccJerkMag-","body acceleration jerk signal in time domain applied to Fast Fourier Transform from the accelerometer", names(dat))
names(dat) <- gsub("tBodyGyroMag-","body acceleration signal in time domain applied to Fast Fourier Transform from the gyroscope", names(dat))
names(dat) <- gsub("tBodyGyroJerkMag-","body acceleration jerk signal in time domain applied to Fast Fourier Transform from the gyroscope", names(dat))
names(dat) <- gsub("fBodyAcc-","body acceleration signal in frequency domain from the accelerometer", names(dat))
names(dat) <- gsub("fBodyAccJerk-","body acceleration jerk signal in frequency domain from the accelerometer", names(dat))
names(dat) <- gsub("fBodyGyro-","body acceleration signal in frequency domain from the gyroscope", names(dat))
names(dat) <- gsub("fBodyAccMag-","body acceleration signal in frequency domain applied to Fast Fourier Transform from the accelerometer", names(dat))
names(dat) <- gsub("fBodyAccJerkMag-","body acceleration jerk signal in frequency domain applied to Fast Fourier Transform from the accelerometer", names(dat))
names(dat) <- gsub("fBodyGyroMag-","body acceleration signal in frequency domain applied to Fast Fourier Transform from the gyroscope", names(dat))
names(dat) <- gsub("fBodyGyroJerkMag-","body acceleration jerk signal in frequency domain applied to Fast Fourier Transform from the gyroscope", names(dat))
names(dat) <- gsub("mean", "Mean value", names(dat))
names(dat) <- gsub("std", "Standard deviation", names(dat))
names(dat) <- gsub("mad", "Median absolute deviation ", names(dat))
names(dat) <- gsub("max", "Largest value in array", names(dat))
names(dat) <- gsub("min", "Smallest value in array", names(dat))
names(dat) <- gsub("sma", "Signal magnitude area", names(dat))
names(dat) <- gsub("energy", "Energy measure. Sum of the squares divided by the number of values", names(dat))
names(dat) <- gsub("iqr", "Interquartile range ", names(dat))
names(dat) <- gsub("entropy", "Signal entropy", names(dat))
names(dat) <- gsub("arCoeff", "Autorregresion coefficients with Burg order equal to 4", names(dat))
names(dat) <- gsub("correlation", "correlation coefficient between two signals", names(dat))
names(dat) <- gsub("maxInds", "correlation coefficient between two signals", names(dat))
names(dat) <- gsub("meanFreq", "Weighted average of the frequency components to obtain a mean frequency", names(dat))
names(dat) <- gsub("skewness", "skewness of the frequency domain signal ", names(dat))
names(dat) <- gsub("kurtosis", "kurtosis of the frequency domain signal ", names(dat))
names(dat) <- gsub("bandsEnergy", "Energy of a frequency interval within the 64 bins of the FFT of each window", names(dat))
names(dat) <- gsub("angle", "Angle between to vectors", names(dat))

##5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
v_activity <- dat$activity
v_subjects <- dat$subjects
c <- character()
mean_row <- numeric()
for (i in 1:length(v_activity)) {
  c[i] = paste(v_activity[i], v_subjects[i])
  mean_row[i] = rowMeans(dat[i,1:561])
}
mean_subject_group <-  tapply(mean_row, c, mean)

activity_new = c()
subjects_new = c()
for (i in 1:length(unique(c))) {
  result <- data.frame(strsplit(unique(c)[i], " "))
  activity_new[i] = as.character(result[1,1])
  subjects_new[i] = as.character(result[2,1])
}
tidydat <- cbind("mean" = data.frame(mean_subject_group)$mean_subject_group, "activity" = activity_new, "subjects" = subjects_new)
write.table(tidydat, "TidyDat.txt", row.name=FALSE)
