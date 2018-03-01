# Coursera JHU Data Science Getting and Cleaning Data Project

# Requirements:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, creates a second, independent tidy data set 
# with the average of each variable for each activity and each subject.

# download the file
if (!file.exists("har.zip")) {
    url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(url, dest = "har.zip", mode = "wb") 
    unzip("har.zip", exdir = "./")
}

# set directories
trainDir <- paste(getwd(),"/UCI HAR Dataset/train/",sep = "")
testDir <- paste(getwd(),"/UCI HAR Dataset/test/",sep = "")
dataDir <- paste(getwd(),"/UCI HAR Dataset/",sep = "") 

# read activity label data
activity_labels <- read.table(paste(dataDir,"activity_labels.txt",sep = 
                                        ''), col.names = c("activityId","activityName"))

# cleaning activity labels
activity_labels[,2] <- tolower(activity_labels[,2])
activity_labels[,2] <- gsub("_", " ", activity_labels[,2])
activity_labels[,2] <- factor(activity_labels[,2])

# read feature data
features <- read.table(paste(dataDir,"features.txt",sep = ''))

# cleaning feature data:
# collect mean and std variables
meanstdvars <- features[grepl("mean()",features$V2) | grepl("std()",features$V2),2]

# remove meanFreq
varslist <- meanstdvars[!grepl("meanFreq()", meanstdvars)]

# remove () and -
remove <- c("\\(\\)", "-")
varslist <- gsub(paste(remove, collapse = "|"), "", varslist)

# camel case Mean and Std
varslist <- gsub("mean", "Mean", varslist)
varslist <- gsub("std", "Std", varslist)

# change t and f to time and freq
varslist <- lapply(varslist,function(x) {
    if (substr(x, 1, 1) == "t") {
        x <- paste("Time",substr(x,2,nchar(x)), sep = "")
    } else if (substr(x, 1, 1) == "f") {
        x <- paste("Freq",substr(x, 2, nchar(x)), sep = "")
    }
    x
})
# convert to list and make a factor
varslist <- as.factor(unlist(varslist))

# read in test data
X_test <- read.table(paste(testDir,"X_test.txt",sep = ''))
y_test <- read.table(paste(testDir,"y_test.txt",sep = ''))
subject_test <- read.table(paste(testDir,"subject_test.txt",sep = ''))

# read in train data
X_train <- read.table(paste(trainDir,"X_train.txt",sep = ''))
y_train <- read.table(paste(trainDir,"y_train.txt",sep = ''))
subject_train <- read.table(paste(trainDir,"subject_train.txt",sep = ''))

# rbind test and train data into single df
testtrain <- rbind(X_test, X_train)
# select only desired columns in meanstd_list
testtrain <- testtrain[, varslist]
# rename columns
names(testtrain) <- varslist

# rbind subject test and train
allsubject <- rbind(subject_test, subject_train)
# rename column
names(allsubject) <- "subjectId"

# rbind y_test and y_train
allactivity <- rbind(y_test, y_train)

# assign labels from features
allactivity$V1 <- factor(allactivity$V1, levels = activity_labels[,1], labels = activity_labels[,2])
# rename column
names(allactivity) <- "activityName"

# bind all three together
alldata <- cbind(allsubject, allactivity, testtrain)

# create a tidy data set with avg of each variable for each activity and each subject
library(dplyr)
myTidy <- alldata %>%
    group_by(subjectId, activityName) %>%
    summarise_each(funs(mean))

# write tidy data set to a new file
write.table(myTidy, file = "tidydata.txt", row.names = FALSE)