destFile = "uci-har-dataset.zip" 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
download.file(fileUrl, destfile = destFile, method = "curl") 
importFeaturesColumnNames <- function(filename){ 
       df <- read.table(filename, header = FALSE, stringsAsFactors = FALSE, 
            col.names = c("number", "name")) 
  return(df$name) } 
importFeatures <- function(filename, colnames){
  df <- read.table(filename, header = FALSE, sep = "",  
   stringsAsFactors = FALSE, col.names = colnames) 
  df <- df[, grep("[.](mean|std)[.]", colnames(df))]
       names(df) <- gsub("[.]", "", names(df)) 
       names(df) <- gsub("mean", "Mean", names(df)) 
       names(df) <- gsub("std", "Std", names(df)) 
        
       return(df) }

importActivityLabels <- function(filename){ 
  df <- read.table("activity_labels.txt", header = FALSE,  
              stringsAsFactors = FALSE,  
              col.names = c("level", "label")) 
  return(df)}
importActivities <- function(filename, factors){ 
  df <- read.table(filename, header = FALSE, sep = "",  
          stringsAsFactors = FALSE, col.names = c("activity")) 
  df$activity <- factor(df$activity,  
        levels = factors$level, labels = factors$label) 
  return(df) } 
importSubjects <- function(filename){ 
  df <- read.table(filename, header = FALSE, sep = "",  
     stringsAsFactors = FALSE, col.names = c("subject")) 
   return(df) } 

unzip("uci-har-dataset.zip")
setwd("./UCI HAR Dataset")

features <- importFeaturesColumnNames("features.txt") 
features_train <- importFeatures("train/X_train.txt", features) 
features_test <- importFeatures("test/X_test.txt", features) 
activities <- importActivityLabels("activity_labels.txt") 
activities_train <- importActivities("train/y_train.txt", activities) 
activities_test <- importActivities("test/y_test.txt", activities) 
subjects_train <- importSubjects("train/subject_train.txt") 
subjects_test <- importSubjects("test/subject_test.txt") 

train <- cbind(features_train, activities_train, subjects_train) 
test <- cbind(features_test, activities_test, subjects_test) 
tidy <- rbind(train, test)
tidy_means <- with(tidy, aggregate(tidy[,1:66],  
      by=list(activity=activity, subject=subject), FUN=mean)) 
write.table(tidy_means, file="tidy_means.txt", row.names=FALSE)