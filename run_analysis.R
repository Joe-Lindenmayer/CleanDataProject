library(plyr)
library(dplyr)

#download the dataset and unzip it
if(!file.exists("./getcleandata")){dir.create("./getcleandata")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="./getcleandata/projectdataset.zip")

unzip(zipfile = "./getcleandata/projectdataset.zip", exdir = "./getcleandata")

#Read tales for training datasets and test datasets
x_train <- read.table("./getcleandata/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./getcleandata/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./getcleandata/UCI HAR Dataset/train/subject_train.txt")

x_test <- read.table("./getcleandata/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./getcleandata/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./getcleandata/UCI HAR Dataset/test/subject_test.txt")


features <- read.table("./getcleandata/UCI HAR Dataset/features.txt")
activityLabels <- read.table("./getcleandata/UCI HAR Dataset/activity_labels.txt")


#Merge train data, Merge test data, then Merge the Merged Data
allX <- rbind(x_train, x_test)
allY <- rbind(y_train, y_test)
Subjects <- rbind(subject_train, subject_test)

# Extracts only the measurements on the mean and standard deviation for each measurement.
meanandstd <- features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
allX <- allX[,meanandstd[,1]]

# Use descriptive activity names to name the activities 
colnames(allY) <- "activity"
allY$activitylabel <- factor(allY$activity, labels = as.character(activityLabels[,2]))
activity_label <- allY[,-1]

#Labels appropriately
colnames(allX) <- features[meanandstd[,1],2]

#forming the tidy dataset
colnames(Subjects) <- "subject"
tidy <- cbind(allX, activity_label,Subjects)
tidy_mean <- tidy %>% group_by(activity_label, subject) %>% summarize_each(funs(mean))
write.table(tidy_mean, file = "tidy.txt", row.names=FALSE, col.names=TRUE)



