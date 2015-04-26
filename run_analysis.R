# Steps to create the tidy data.
# ===============================

# 1. Merges the training and the test sets to create one data set.
# ----------------------------------------------------------------------------
# Reads training data
training.set <- read.table('./UCI HAR Dataset/train/X_train.txt')
training.labels <- read.table('./UCI HAR Dataset/train/y_train.txt', 
                              col.names=c('Activity'))
training.subject <- read.table('./UCI HAR Dataset/train/subject_train.txt',
                               col.names=c('Subject'))

# Creates the training dataset.
training <- cbind(training.set, training.labels, training.subject)

# Reads test data
test.set <- read.table('./UCI HAR Dataset/train/X_train.txt')
test.labels <- read.table('./UCI HAR Dataset/train/y_train.txt', col.names=c('Activity'))
test.subject <- read.table('./UCI HAR Dataset/train/subject_train.txt', col.names=c('Subject'))

# Creates the test dataset.
test <- cbind(test.set, test.labels, test.subject)
dataset <- rbind(test, training)


# 2. Extracts only the measurements on the mean and standard deviation for each 
#    measurement.
# ----------------------------------------------------------------------------

#    Creates a vector with TRUE values on the poisitions where the value
#    contains -mean() or -std()
features <- read.table('./UCI HAR Dataset/features.txt')
mean.or.std <- grepl('-mean\\(\\)|-std\\(\\)', features[,2])

#    Select only the measurements for the mean and standard deviation also 
#    activity and subject columns.
selector <- c(mean.or.std, TRUE, TRUE)
dataset <- dataset[, selector]


# 3. Uses descriptive activity names to name the activities in the data set.
# ----------------------------------------------------------------------------
activity.labels <- read.table('./UCI HAR Dataset/activity_labels.txt',
                              col.names=c('Activity', 'Labels'))

# Parse activity column in the dataset as factor
dataset$Activity <- as.factor(dataset$Activity)

# Set levels to the activity column, the values are going to be represented as
# strings:
# 1 =>            WALKING
# 2 =>   WALKING_UPSTAIRS
# 3 => WALKING_DOWNSTAIRS
# 4 =>            SITTING
# 5 =>           STANDING
# 6 =>             LAYING
levels(dataset$Activity) <- as.character(activity.labels[,2])

# 4. Appropriately labels the data set with descriptive variable names.
# ----------------------------------------------------------------------------
mean.or.std.colnames <- as.character(features[, 2][mean.or.std])
colnames(dataset) <- c(mean.or.std.colnames, 'Activity', 'Subject')


# 5. From the data set in step 4, creates a second, independent tidy data set
#    with the average of each variable for each activity and each subject.
# ----------------------------------------------------------------------------

# Calculate mean for all columns grouping by Activity and Subject
library(dplyr)
result <- dataset %>% group_by(Activity, Subject) %>% summarise_each(funs(mean))

# Writing the second tidy dataset
write.table(result, 'dataset.txt', row.name=FALSE)

print("dataset.txt with the tidt dataset file created successfully")
