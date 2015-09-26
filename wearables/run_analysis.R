# John Hopkins - Getting and Cleaning Data
# Week 3 Project Solution

library(dplyr)

# Import train data
train.x <- read.table('./train/X_train.txt')
train.y <- read.table('./train/y_train.txt')
train.subject <- read.table('./train/subject_train.txt')

# Import test data
test.x <- read.table('./test/X_test.txt')
test.y <- read.table('./test/y_test.txt')
test.subject <- read.table('./test/subject_test.txt')

# Import metadata
activityLabels <- read.table('activity_labels.txt', stringsAsFactors = FALSE)
features <- read.table('features.txt', stringsAsFactors = FALSE)

# 
# 1. Merge the training and the test sets to create one data set
#

# Merge train and test signals
data.x <- rbind(train.x, test.x)

#
# 2. Extract only the measurements on the mean and standard deviation for each measurement
# 

# Set correct collumn names 
colnames(data.x) <-features[,2]

# Find Mean and Standard Derivation
meanIdx <- grep('mean()',colnames(data.x),fixed=TRUE)
stdIdx <- grep('std()',colnames(data.x),fixed=TRUE)

# Create data frame containing only the mean and the SD
data.all <- data.x[,c(meanIdx,stdIdx)]

#
# 3. Use descriptive activity names to name the activities in the data set
#

# Merge train and test activity label ids and place labels next to measurments
labelIDs <- as.integer(rbind(train.y, test.y)[,1])
activity <- vector(mode="character",length=length(labelIDs))
# Replace labels IDs with Activity Names

for (i in 1:length(labelIDs)) {
    activity[i] <- activityLabels[labelIDs[i],2]
}
data.all <- cbind(activity, data.all)

#
# 4. Appropriately labels the data set with descriptive variable names. 
#

# Merge train and test subjects and place them next to measurments
subject <- rbind(train.subject, test.subject)
colnames(subject) <- "subject"
data.all <- cbind(subject, data.all)

#
# 5. From the data set in step 4, create a second, independent tidy data set
# with the average of each variable for each activity and each subject
#

# Group by both subject and activity
by_subject_activity <- group_by(data.all, subject, activity)
# Calculate means for each column/group
data.tidy <- summarize_each(by_subject_activity,funs(mean))

write.table(data.tidy, file = "tidyData.txt", row.name = FALSE)

