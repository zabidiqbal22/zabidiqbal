## Github repo: 

#setting up working directory 

setwd("M:/econ7data/Coursera/Cleaning Data/UCI HAR Dataset/")

## 1. Merging the training and the test sets to create one data set.

# Read data from files

train_data <- read.table("./train/X_train.txt")
train_label <- read.table("./train/y_train.txt")
train_subject <- read.table("./train/subject_train.txt")
test_data <- read.table("./test/X_test.txt")
test_label <- read.table("./test/y_test.txt") 
test_subject <- read.table("./test/subject_test.txt")


# Create the separate data, subject, and label text file by merging each components
library(plyr)
Data_all<- rbind(train_data, test_data)
Label_all <- rbind(train_label, test_label)
Subject_all <- rbind(train_subject, test_subject)


##2. Extract only the measurements on the mean and standard deviation for each measurement. 

  
features<-read.table('./features.txt',header=FALSE); #imports features.txt
dim(features)  # 561*2
mean_std <- grep("mean\\(\\)|std\\(\\)", features[, 2]) # extracting mean and sd
Data_all <- Data_all[, mean_std]

names(Data_all) <- gsub("\\(\\)", "", features[mean_std, 2]) 
names(Data_all) <- gsub("mean", "Mean", names(Data_all)) 
names(Data_all) <- gsub("std", "Std", names(Data_all))
names(Data_all) <- gsub("-", "", names(Data_all))


##3. Use descriptive activity names to name the activities in the data set

activity_labels<-read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt

# Assign column names to the test data imported in step 1
colnames(test_subject) <- "subjectId";
colnames(test_data)   <- features[,2]; 
colnames(test_label)       <- "activityId";

# Assigin column names to the data imported in step 1
colnames(activity_labels)  <- c('activityId','activityType');
colnames(train_subject)  <- "subjectId";
colnames(train_data)        <- features[,2]; 
colnames(train_label)        <- "activityId";

# Merging label and subject file

Label_all <- rbind(train_label, test_label)
Subject_all <- rbind(train_subject, test_subject)

# final data set
Data_final <- cbind(Data_all, Subject_all, Label_all);
Data_final <- merge(Data_final,activity_labels,by='activityId',all.x=TRUE)
colNames  <- colnames(Data_final); 





## 4. Appropriately label the data set with descriptive activity names. 


for (i in 1:length(colNames)) 
{
  colNames[i] <- gsub("\\()","",colNames[i])
  colNames[i] <- gsub("-std$","StdDev",colNames[i])
  colNames[i] <- gsub("-mean","Mean",colNames[i])
  colNames[i] <- gsub("^(t)","time",colNames[i])
  colNames[i] <- gsub("^(f)","freq",colNames[i])
  colNames[i] <- gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] <- gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] <- gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] <- gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] <- gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] <- gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] <- gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames(Data_final) <- colNames



write.table(Data_final, "Data_final.txt",row.names=FALSE,sep='\t') #  1st dataset




##5) Creates a second, independent tidy finalData set with the average of each variable for each activity and each subject. 

library(plyr)
library(dplyr)
tidy_data <-aggregate(. ~ subjectId+ activityId+activityType, Data_final[-2], FUN = mean) 
summary(tidy_data)

# Export tidy Data set 

write.table(tidy_data, "tidy_data.txt",row.names=FALSE,sep='\t') # 2nd dataset


#source("run_analysis.R")
