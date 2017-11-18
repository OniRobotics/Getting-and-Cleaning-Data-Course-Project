library("reshape2")
library("dmm")

## Set working directory to where the data is stored
setwd("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset")

## Loading the variable names
actlabels <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/activity_labels.txt", col.names = c("Label","Activity"))
features <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/features.txt")

## Combine the test folder data set with the names and subjects
testset <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/test/X_test.txt", col.names = features$V2)
testlabels <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/test/y_test.txt", col.names = "Activity")
testsubjects <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/test/subject_test.txt", col.names = "Subject")
test <- data.frame(testsubjects,testlabels,testset)

## Combine the train folder data set with the labels and subjects
trainset <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/train/X_train.txt", col.names = features$V2)
trainlabels <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/train/y_train.txt", col.names = "Activity")
trainsubjects <- read.table("D:/Backup/Documents/Data Science/Week 12/UCI HAR Dataset/train/subject_train.txt", col.names = "Subject")
train <- data.frame(trainsubjects,trainlabels,trainset)

## Stack the test and train data sets together
data <- rbind.data.frame(test,train)

## Extract mean and std
value1 <- grep("[Mm]ean", names(data))
value2 <- grep("[Ss]td", names(data))
value <- sort(c(1,2,value1,value2))
results <- data[value]

## Replace the generic labels with the activity names
actlabels[,2] <- as.character(actlabels[,2])
for (i in 1:length(actlabels[,1])) {
  results$Activity <- gsub(actlabels[i,1],actlabels[i,2],results$Activity)
}

## Post results
write.table(results,"STDandMean.txt", row.names = FALSE)

## Take the mean of each variable from results
results$Subject <- as.factor(as.character(results$Subject))
mresults <- melt(results,id = c("Subject","Activity"))
tidy <- dcast(mresults, Subject + Activity ~ variable,mean)
tidy$Subject <- unfactor(tidy$Subject)
tidyset <- tidy[order(tidy$Subject),]  # sort by subject

## Post Results
write.table(tidyset,"Tidy.txt", row.names = FALSE)