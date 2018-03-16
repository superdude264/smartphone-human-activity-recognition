#' ---
#' title: "WorkBook"
#' output:
#'   html_document:
#'     df_print: paged
#' ---
#' ### Options
## ----GlobalOptions, message=FALSE, warning=FALSE-------------------------
options(knitr.duplicate.label = 'allow')

#' 
#' ### Libraries
## ----message=FALSE, warning=FALSE----------------------------------------
library(tidyverse)

#' 
#' ### Data constants
## ----message=FALSE, warning=FALSE----------------------------------------
FILE_URL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
DATA_DIR <- "./data/"
DATA_FILE = paste0(DATA_DIR, "uci_har_dataset.zip")

#' 
#' ### Download & store data
## ----message=FALSE, warning=FALSE----------------------------------------
if(!file.exists(DATA_DIR)){dir.create(DATA_DIR)}
# download.file(FILE_URL, destfile = DATA_FILE, method = "curl")
DOWNLOAD_DATE <- date()

#' 
#' ### Extract data
## ----message=FALSE, warning=FALSE----------------------------------------
unzip(DATA_FILE, exdir = DATA_DIR)
setwd(paste0(DATA_DIR, "UCI HAR Dataset"))

activities <- read.table("activity_labels.txt")
features <- read.table("features.txt")

# Test
test_subjects <- read.table("./test/subject_test.txt")
test_x <- read.table("./test/X_test.txt")
test_y <- read.table("./test/y_test.txt")

# Train
train_subjects <- read.table("./train/subject_train.txt")
train_x <- read.table("./train/X_train.txt")
train_y <- read.table("./train/y_train.txt")

# Merge
subjects <- rbind(test_subjects, train_subjects)
x <- rbind(test_x, train_x)
y <- rbind(test_y, train_y)

# Clean-up
remove(test_subjects, test_x, test_y, train_subjects, train_x, train_y)

setwd("../..")

#' 
#' ### Factorize & merge
## ------------------------------------------------------------------------
# 'Factorize' 
activities <- activities$V2
features <- features$V2
y <- factor(y$V1, labels = activities)

# Set column names for X
colnames(x) <- features

# Merge columns
har <- cbind(activity = y, subject = subjects$V1, x)

# Clean-up
remove(x, y, activities, features, subjects)

#' 
#' ### Filter out unwanted metrics
## ------------------------------------------------------------------------
mean_names <- grep("-mean\\(\\)*", names(har), perl = T, value = T)
std_names <- grep("-std\\(\\)*", names(har), perl = T, value = T)
# angle_mean_names <- grep("Mean", names(har), perl = T, value = T)

har_means <- har[mean_names]
har_stds <- har[std_names]
# har_angle_means <- har[angle_mean_names]

har <- cbind(activity = har$activity, subject = har$subject, har_means, har_stds)
har <- as_tibble(har)

remove(mean_names, std_names, har_means, har_stds)

#' 
#' ### Rename variables
#' Original naming convention is as follows:
#' 
#' `<domain><acceleration_type><measurement_device>[Jerk][Mag]-<summarization_function>()-<axis>`
#' 
#' Where the following values describe the data value:
#' 
#' * domain: 't' | 'f' -- time or frequency domain
#' * acceleration_type: 'Body' | 'Gravity' -- body or gravity acceleration
#' * measurement_device: 'Acc' | 'Gyro' -- accelerometer or gyroscope signal
#' * 'Jerk' -- jerk (acceleration derived in time)
#' * 'Mag' --  magnitude (Euclidean norm)
#' * summarization_function: 'mean' | 'std' -- mean or standard deviation used to summarize signal
#' * axis: 'X' | 'Y' | 'Z' -- axis of signal
#' 
#' From the original dataset:
#' 
#' > The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 
#' >
#' > Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 
#' >
#' > Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 
#' >
#' > These signals were used to estimate variables of the feature vector for each pattern:  
#' '-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
#' >
#' > * tBodyAcc-XYZ
#' > * tGravityAcc-XYZ
#' > * tBodyAccJerk-XYZ
#' > * tBodyGyro-XYZ
#' > * tBodyGyroJerk-XYZ
#' > * tBodyAccMag
#' > * tGravityAccMag
#' > * tBodyAccJerkMag
#' > * tBodyGyroMag
#' > * tBodyGyroJerkMag
#' > * fBodyAcc-XYZ
#' > * fBodyAccJerk-XYZ
#' > * fBodyGyro-XYZ
#' > * fBodyAccMag
#' > * fBodyAccJerkMag
#' > * fBodyGyroMag
#' > * fBodyGyroJerkMag
#' >
#' > The set of variables that were estimated from these signals are: 
#' >
#' > * mean(): Mean value
#' > * std(): Standard deviation
#' 
## ------------------------------------------------------------------------
tag_columns = har %>% select(activity, subject)
measurements = har %>% select(-one_of("activity"), -one_of("subject"))

# Fix names that have "Body" twice
colnames(measurements) <- gsub("BodyBody", "Body", names(measurements))

# Remove '()' from summarization function name
colnames(measurements) <- gsub("\\(\\)", "", names(measurements))

# Replace dash with period
colnames(measurements) <- gsub("-", ".", names(measurements))

# Set to 'period case' (i.e. r.does.names.like.this)
# See: https://stackoverflow.com/questions/22528625/how-to-convert-camelcase-to-not-camel-case-in-r
colnames(measurements) <- tolower(gsub("([[:lower:]])([[:upper:]])", "\\1.\\L\\2", names(measurements), perl = TRUE))

har_tidy <- cbind(tag_columns, measurements)

remove(har, tag_columns, measurements)

#' 
#' ### Generate a summary dataset
## ------------------------------------------------------------------------
har_summary <- har_tidy %>%
   group_by(activity, subject) %>%
   summarise_all(funs(mean))

#' 
#' ### Write out the datasets
## ----message=FALSE, warning=FALSE----------------------------------------
TIDY_DATA = paste0(DATA_DIR, "tidy/")
if (!file.exists(TIDY_DATA)) { dir.create(TIDY_DATA) }

setwd(TIDY_DATA)

write.table(har_tidy, "har_tidy.txt", row.names = F)
write.csv(har_summary, "har_summary.txt", row.names = F)

# Write out access info
cat(paste("Source", FILE_URL, sep = "\t"), file = "ACCESS_INFO.txt", sep = "\n")
cat(paste("Date", DOWNLOAD_DATE, sep = "\t"), file = "ACCESS_INFO.txt", append = TRUE)

setwd("../..")

#' 
#' 
#' 
#' 
