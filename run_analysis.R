##------------------------------------------------------------------------------
## Course Project
## You should create one R script called run_analysis.R that does the following:
##------------------------------------------------------------------------------
##
## Merges the training and the test sets to create one data set ##
##
##'train/X_train.txt': Training set.
##'test/X_test.txt': Test set.
## 1) Read in the datasets
train <- "UCI HAR Dataset/train/X_train.txt"
test <- "UCI HAR Dataset/test/X_test.txt"
train_set <- read.csv(train, sep="", header=FALSE)
test_set <- read.csv(test, sep="", header=FALSE)
## 2) Stack them
fulldata <- rbind(train_set, test_set)
##
## Extracts only the measurements on the mean and standard deviation for each measurement. ##
##
## 1) Understand what columns in the dataset correspond to.
## This is given in the file features.txt.
## Read in this file
feat <- read.csv("UCI HAR Dataset/features.txt", sep="", header = FALSE)
## feat contains two columns: 
## the first has the column names of the dataset, the second the explanation.
## Select rows corresponding to mean and std dev. Keep values from the first column only.
mean_std <- feat[grepl("mean()|std()", feat[,2]),1]
## Now create a vector containing the names of the columns you want to extract from fulldata
mean_std_col_names <- paste0("V", mean_std)
## Perform the extraction and save result into new data frame
newdata <- fulldata[,mean_std_col_names]
##
## Uses descriptive activity names to name the activities in the data set. ##
##
## 1) The activity number for each row in the dataset is stored in the y_train/set.txt files 
## Read in the information and stack it
ytrain <- "UCI HAR Dataset/train/Y_train.txt"
ytest <- "UCI HAR Dataset/test/Y_test.txt"
ytrain_set <- read.csv(ytrain, sep="", header=FALSE)
ytest_set <- read.csv(ytest, sep="", header=FALSE)
fullydata <- rbind(ytrain_set, ytest_set)
## 2) Match numbers with labels 
## The key is given in the activity_labels.txt file
rowlabels <- read.csv("UCI HAR Dataset/activity_labels.txt", sep="", header = FALSE)
## Now change the values in fullydata accordingly
for (i in 1:nrow(rowlabels)) {
  act <- rowlabels[i,1]
  where <- fullydata == act
  fullydata[where,] <- as.character(rowlabels[i,2])
}
## 3) Add fullydata to the dataset newdata
newdata <- cbind(fullydata,newdata)
##
## Appropriately labels the data set with descriptive variable names. ##
##
## 1) Recover the names of the columns in the dataframe 
mean_std_labels <- feat[grepl("mean()|std()", feat[,2]),2]
## 2) Change column names
colnames(newdata) <- c("Activity", as.character(mean_std_labels))
##
## From the data set in step 4, creates a second, independent tidy data set ##
## with the average of each variable for each activity and each subject.    ##
##
## 1) Add a column with the subject who performed each experiment
## Read in the information on subjects and stack it
strain <- "UCI HAR Dataset/train/subject_train.txt"
stest <- "UCI HAR Dataset/test/subject_test.txt"
strain_set <- read.csv(strain, sep="", header=FALSE)
stest_set <- read.csv(stest, sep="", header=FALSE)
fullsdata <- rbind(strain_set, stest_set)
## Add column with name
newdata <- cbind(fullsdata,newdata)
colnames(newdata)[1] <- "Subject"
## 2) Group the data frame newdata by Subject and Activity
## (The operations below require the dplyr package)
library(dplyr)
by_sub_act <- group_by(newdata, Subject, Activity)
newdata2 <- summarise_each(by_sub_act, funs(mean))
## Now write the new dataset into a file
write.table(newdata2, file="GCD_courseproject.txt", row.names=FALSE)