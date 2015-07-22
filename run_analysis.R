#Step1.
#Set directory and read raw data
#Merge train and test dataset to a new dataset

#read data from the unziped file
setwd("C:/Users/l/GC1")
traindata <- read.table("./data/train/X_train.txt")
dim(traindata) #7352 561
trainlabel <- read.table("./data/train/y_train.txt")
table(trainlabel)
trainsubject <- read.table("./data/train/subject_train.txt")
testdata <- read.table("./data/test/X_test.txt")
dim(testdata) #2947 561
testlabel <- read.table("./data/test/y_test.txt")
table(testlabel)
testsubject <- read.table("./data/test/subject_test.txt")


#merge dataset
joindata <- rbind(traindata, testdata)
dim(joindata)
joinsubject <- rbind(trainsubject, testsubject)
dim(joinsubject)
joinlabel <- rbind(trainlabel, testlabel)
dim(joinlabel)


#Step2
#Extract only the measurements on the mean and standard deviation for each mesurement

features <- read.table("./data/features.txt")
dim(features) #561 2
meansdindex <- grep("mean\\(\\)|std\\(\\)", features[,2])
length(meansdindex)
joindata <- joindata[,meansdindex]
names(joindata) <- gsub("\\(\\)", "", features[meansdindex, 2])
names(joindata) <- gsub("-", "", names(joindata))

#Step3
#Uses descriptive activity names to name the activities in the data set

activity <- read.table("./data/activity_labels.txt")
activity[,2] <- tolower(gsub("_", "", activity[,2]))
activitylabel <- activity[joinlabel[,1], 2]
joinlabel[,1] <- activitylabel
names(joinlabel) <- "activity"

#Step4
#Appropriately labels the data set with descriptive variable names.
names(joinsubject) <- "subject"
tidydata <- cbind(joinsubject, joinlabel, joindata)
dim(tidydata)
write.table(tidydata, "tidydata.txt")


#Step5
#creates a second, independent tidy data set with the average of each variable for each activity and each subject.

subjectlength <- length(table(joinsubject)) # 30
activitylength <- dim(activity)[1] # 6
columnlength <- dim(tidydata)[2]
result <- matrix(NA, nrow=subjectlength*activitylength, ncol=columnlength) 
result <- as.data.frame(result)
colnames(result) <- colnames(tidydata)
row <- 1
for(i in 1:subjectlength) {
        for(j in 1:activitylength) {
                result[row, 1] <- sort(unique(joinsubject)[, 1])[i]
                result[row, 2] <- activity[j, 2]
                bool1 <- i == tidydata$subject
                bool2 <- activity[j, 2] == tidydata$activity
                result[row, 3:columnlength] <- colMeans(tidydata[bool1&bool2, 3:columnlength])
                row <- row + 1
        }
}
head(result)
write.table(result, "data_with_means.txt") # write out the 2nd dataset
