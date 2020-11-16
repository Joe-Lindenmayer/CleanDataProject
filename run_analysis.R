library(plyr)

#download the dataset and unzip it
if(!file.exists("./getcleandata")){dir.create("./getcleandata")}
fileurl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileurl,destfile="./getcleandata/projectdataset.zip")

unzip(zipfile = "./getcleandata/projectdataset.zip", exdir = "./getcleandata")

#Read tables for training datasets and test datasets
x_train <- read.table("./getcleandata/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./getcleandata/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./getcleandata/UCI HAR Dataset/train/subject_train.txt")

features <- read.table("./getcleandata/UCI HAR Dataset/features.txt")
activityLabels = read.table("./getcleandata/UCI HAR Dataset/activity_labels.txt")

#Need labels for the tables
colnames(x_train) <- features[,2]
colnames(y_train) <- "activityID"
colnames(subject_train) <- "subjectID"
colnames(x_test) <- features[,2]
colnames(y_test) <- "activityID"
colnames(subject_test) <- "subjectID"
colnames(activityLabels) <- c("activityID", "activityType")

#Merge train data, Merge test data, then Merge the Merged Data
alltrain <- cbind(y_train, subject_train, x_train)
alltest <- cbind(y_test, subject_test, x_test)
fulldataset <- rbind(alltrain, alltest)

# Extracts only the measurements on the mean and standard deviation for each measurement.
# There are several mean and std columns, we need to search for each of them the . metacharacters allow any characters after "mean" to be collected
colNames <- colnames(fulldataset)
meanandstd <- (grepl("activityID",colNames)|grepl("subjectID", colNames)|grepl("mean..",colNames)|grepl("std...",colNames))
meanandstdsubset <- fulldataset[ , meanandstd == TRUE]

# Uses descriptive activity names to name the activities in the data set
setWithActivityNames <- merge(meanandstdsubset, activityLabels, by= "activityID", all.x = TRUE)
#activity types now has its own column at the end of the dataset

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Will use metacharacters (.) (anything) and (~) (dependent on)
# in the second line, we fixed the order so that the SubjectIDs stay together
# column names are still ugly looking, going to edit labels for readability 

tidy <- aggregate(. ~subjectID + activityID, setWithActivityNames, mean)
tidy <- tidy[order(tidy$subjectID, tidy$activityID), ]
names(tidy) <- gsub("^t", "time", names(tidy))
names(tidy) <- gsub("^f", "frequency", names(tidy))
names(tidy) <- gsub("Acc", "Accelerometer", names(tidy))
names(tidy) <- gsub("Gyro", "Gyroscope", names(tidy))
names(tidy) <- gsub("Mag", "Magnitude", names(tidy))
names(tidy) <- gsub("BodyBody", "Body", names(tidy))

write.table(tidy, "tidy.txt", row.names=FALSE)


