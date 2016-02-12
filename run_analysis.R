# replace with the location of
# your unzipped data set
folderLocation <- "C:/Users/CR0731/Desktop/Learning/University/ATT Data Scientist/Data Scientist/Coursera Getting Data/data/UCI HAR Dataset/"
setwd(folderLocation)

# read in training data
readtrainData <- read.table("./train/X_train.txt", sep="")
readtrainLabel <- read.table("./train/Y_train.txt")
readtrainSubjects <- read.table("./train/subject_train.txt")

# convert to dplyr data frames
library(dplyr)
xtrain <- tbl_df(readtrainData)
xtrainLbl <- tbl_df(readtrainLabel)
xtrainSubs <- tbl_df(readtrainSubjects)
xtraincombined <- tbl_df(cbind(xtrainSubs, xtrainLbl, xtrain))

# read in testing data
readtestData <- read.table("./test/X_test.txt")
readtestLabels <- read.table("./test/Y_test.txt")
readtestSubjects <- read.table("./test/subject_test.txt")

# convert to dplyr data frames
xtestdata <- tbl_df(readtestData)
xtestlbls <- tbl_df(readtestLabels)
xtestsubs <- tbl_df(readtestSubjects)
xtestcombined <- cbind(xtestsubs, xtestlbls, xtestdata)

# append training data to test data
# and convert to dplyr data frame
total <- tbl_df(rbind(xtestcombined,xtraincombined))

# read in column names data
names <- read.table("./features.txt", sep=" ")
testnames <- c("Subjects","Activity",as.character(names[,2]))
colnames(total) <- testnames

# find locations of all column names
# containing mean() and std()
locationMean <- grep("mean\\(\\)", testnames)
locationSTD <- grep("std\\(\\)", testnames)
bothStats <- sort(c(locationMean, locationSTD))

# subset table on columns containing both statistics
# and keep the first two columns "Subjects" and "Activity"
subsetTotal <- tbl_df(total[,c(1,2, bothStats)])

# sort the data on subject and then activity
subsetTotal <- arrange(subsetTotal, Subjects, Activity)

# Convert Activities 1-6 levels to
# Descriptive Activity Names
activities <- as.matrix((subsetTotal[,2]))
ax <- as.character(activities)
library(plyr)
nums <- as.character(1:6)
activitynames <- c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING")
newActivities <- mapvalues(ax, from = nums, to = activitynames)
subsetTotal[,"Activity"] <- newActivities
detach(package:plyr)
library(dplyr)

# create better descriptive column names
# read in column names
playset <- colnames(subsetTotal)

# find all "()" and delete them
firstPass <- gsub("[\\(\\)]","",playset)

# split the words on "-", except first two columns
# creates list of stripped words
secondPass <- strsplit(firstPass,"-")[3:length(firstPass)]

# for each element in the list, capitalize std and mean
# put the measured signal trait first
# if XYZ coordinate, place that last
betterNames <- ""
for (i in 1:length(secondPass)) {
  element <- secondPass[[i]]
  statVar <- toupper(element[2])
  signal <- element[1]
  if (!is.na(element[3])) {
    coord <- tolower(element[3])
    betterNames[i] <- paste(signal,statVar, coord, sep="")
  }
  else {
    betterNames[i] <- paste(signal, statVar, sep="")
  }
}
bestNames <- c("Subject", "Activity", betterNames)
colnames(subsetTotal) <- bestNames

# make sure Subject and Activity are factors
subsetTotal$Activity <- as.factor(subsetTotal$Activity)
subsetTotal$Subject <- as.factor(subsetTotal$Subject)
tidySamsung <- tbl_df(subsetTotal)

# find the average of the signal measurements
# by Activity and Subject
step1 <- group_by(tidySamsung,Subject, Activity)

# summarise_each will summarise all non-groupby
# columns by the function indicated
step2 <- summarise_each(step1,funs(mean))

# create new names for these averaged columns by
# placing "MEAN" in front of their original names
toBeRenamed <- colnames(step2)[c(-1,-2)]
betternewNames = ""
for (i in 1:length(toBeRenamed)){
  betternewNames[i] = paste("MEAN",toBeRenamed[i],sep="")
}
colnames(step2) <- c("Subject", "Activity", betternewNames)
meanedSamsung <- tbl_df(step2)

# uncomment to create txt file of the final tidy data
## write.table(meanedSamsung,file="tidydata.txt",row.names = FALSE)
