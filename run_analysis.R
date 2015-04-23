## Required libraries
library(dplyr)
library(tidyr)

## Get the source data and extract it
if (!file.exists("UCI HAR Dataset"))
{
        download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "data.zip")
        unzip("data.zip")
}

## Merge the data from Test and Training in each corresponding table
subject_train <- read.table(".\\UCI HAR Dataset\\train\\subject_train.txt", header=FALSE)
y_train <- read.table(".\\UCI HAR Dataset\\train\\y_train.txt", header=FALSE)
x_train <- read.table(".\\UCI HAR Dataset\\train\\X_train.txt", header=FALSE)

subject_test <- read.table(".\\UCI HAR Dataset\\test\\subject_test.txt", header=FALSE)
y_test <- read.table(".\\UCI HAR Dataset\\test\\y_test.txt", header=FALSE)
x_test <- read.table(".\\UCI HAR Dataset\\test\\X_test.txt", header=FALSE)

dataset_subject <- bind_rows(subject_train, subject_test)
dataset_y <- bind_rows(y_train, y_test)
dataset_x <- bind_rows(x_train, x_test)

## Extract Mean and deviation for each measurement

features <- read.table(".\\UCI HAR Dataset\\features.txt", header=FALSE)

names(features) <- c("id","name")
names(dataset_subject) <- "subject"

names(dataset_x) <- as.character(features$name)

dataset_x <- dataset_x[,grepl("mean()", colnames(dataset_x), fixed=TRUE) | grepl("std()", colnames(dataset_x), fixed=TRUE)]

## Use descriptive activity Names for the data set

activities <- read.table(".\\UCI HAR Dataset\\activity_labels.txt", header=FALSE)

names(activities) <- c("id","name")
names(dataset_y) <- "activity"

dataset_y$activity <- activities[dataset_y$activity,]$name

## Label the data set with descriptive variable names.
# Every single dataset has already been labeled. Let´s bind them together into a single one labeled.

dataset <- bind_cols(dataset_subject, dataset_y, dataset_x)

## Create an independent tidy data set
dataset <- tbl_df(dataset)
names(dataset) <- gsub(pattern = "\\(\\)", replacement = "", names(dataset))

tidy_ds <- dataset %>%
        group_by(subject, activity) %>%
        # Sort by subject and then activity
        arrange(subject, activity) %>%
        # Compute the mean for every variable
        summarise_each(funs(mean)) %>%
        # Convert all the variable into a value of signal
        gather(signal, value, -subject, -activity) %>%
        # From signal extract the name of the signal, the type of measurement (mean, std) and the axis
        separate(signal, into=c("signal", "variable", "axis"), sep="\\-", extra="drop") %>%
        # Turn the value into 2 values: mean and std
        spread(variable, value)

# Save the data
write.table(tidy_ds, file="analysis_output.txt", row.name=FALSE)
