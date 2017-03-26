library(dplyr)

zipFile <- "getdata_dataset.zip"
dirName <- "UCI HAR Dataset"

# Check if data dir is present
if (!file.exists(dirName)) {
  # Check if zip file is available
  if (!file.exists(zipFile)){
    # Download zip file from the URL
    URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
    download.file(URL, zipFile, method="curl")
  } 
  # unzip the zip file
  unzip(zipFile)
}

# Load features
featuresFile <- "UCI HAR Dataset/features.txt"
features <- read.table(featuresFile)
features[,2] <- as.character(features[,2])

# Load activity labels
activitiesFile <- "UCI HAR Dataset/activity_labels.txt"
activityLabels <- read.table(activitiesFile)
activityLabels[,2] <- as.character(activityLabels[,2])

# Extract only measurements on mean and standard deviation
requiredMeasurements <- grep("mean\\(\\)|std\\(\\)", features[,2])
requiredMeasurements.names <- features[requiredMeasurements,2]

# Change -mean into Mean
requiredMeasurements.names = gsub('-mean()', 'Mean', requiredMeasurements.names)

# Change -sub to Sub
requiredMeasurements.names = gsub('-std()', 'Std', requiredMeasurements.names)

# Remove unwanted hyphens(-) and brackets()
requiredMeasurements.names <- gsub('[-()]', '', requiredMeasurements.names)


# Load the training datasets
trainX <- read.table("UCI HAR Dataset/train/X_train.txt")[requiredMeasurements]
trainY <- read.table("UCI HAR Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")

# Merge all the training datasets into 1
train <- cbind(trainSubjects, trainY, trainX)

# Load test datasets
testX <- read.table("UCI HAR Dataset/test/X_test.txt")[requiredMeasurements]
testY <- read.table("UCI HAR Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")

#Merge all the test datsets into 1
test <- cbind(testSubjects, testY, testX)

# merge both (training and test) datasets
allData <- rbind(train, test)

# Add labels
colnames(allData) <- c("subject", "activity", requiredMeasurements.names)

# convert the activities and subjects into factors
allData$activity <- factor(allData$activity, levels = activityLabels[,1], labels = activityLabels[,2])
allData$subject <- as.factor(allData$subject)

# compute mean and order the dataset by activity name
allDataMean <- allData %>% 
  group_by(activity, subject) %>% 
  summarize_each(funs(mean)) %>%
  arrange(desc(activity))

# write it to a file 'tidy.txt'
write.table(allDataMean, file = "./tidy.txt", sep = ",", row.names = FALSE, col.names = TRUE)