#---I have donwloaded and extracted the data set to my hard drive and set the 
#---the working drive to ./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset

#---To start, read all file
featureName <- read.table("features.txt")
activLabels <- read.table("activity_labels.txt")

subjectTrain <- read.table("./train/subject_train.txt", header=FALSE)
activTrain <- read.table("./train/y_train.txt",header=FALSE)
featuresTrain <- read.table("./train/X_train.txt", header=FALSE)

subjectTest <- read.table("./test/subject_test.txt", header=FALSE)
activTest <- read.table("./test/y_test.txt", header=FALSE)
featuresTest <- read.table("./test/X_test.txt", header=FALSE)

#--- Part 1. 
#--- combind train and test data in to one
subject <- rbind(subjectTrain, subjectTest)
active <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#--- get the columns
colnames(features) <- t(featureName[2])

#--- Merge data
colnames(active) <- "Activity"
colnames(subject) <- "Subject"
Mergedata <- cbind(features, active, subject)

#--- Part2
#--- Getting the mean and STD data
columnswithMeanSTD <- grep(".*Mean.*|.*Std.*", names(Mergedata), ignore.case=TRUE)

#--- add activity and subject columns
extractcolumns <- c(columnswithMeanSTD, 562, 563)
extractedData <- Mergedata[,extractcolumns]

#--- Part 3
#--- Change the data type to accepte the name
extractedData$Activity <- as.character(extractedData$Activity)
for(i in 1:6){
extractedData$Activity[extractedData$Activity == i] <- as.character(activLabels[i,2])
}

#--- Change the data type to factor
extractedData$Activity <- as.factor(extractedData$Activity)

#--- Part 4
#--- change the name
names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#--- Part 5
#--- set subject as factor
extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)

#--- creat tidyData
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)

