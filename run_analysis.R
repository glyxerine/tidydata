getFeatures <- function() {
        ## Load features
        features <- read.csv("./UCI HAR Dataset/features.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)
        features$V2 <- sapply(features$V2, tolower)
        
        ## Clean up the features
        features$V2 <- sapply(features$V2, function(x) gsub("-x","xaxis",x))
        features$V2 <- sapply(features$V2, function(x) gsub("-y","yaxis",x))
        features$V2 <- sapply(features$V2, function(x) gsub("-z","zaxis",x))
        features$V2 <- sapply(features$V2, function(x) gsub("-","",x))
        
        features$V2
}

getActivities <- function() {
        ## Load features
        act <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE, stringsAsFactors = FALSE)
        
        ## Set the column names
        names(act) <- c("activitycode", "activity")
        act
}

processData <- function(subjectFile, datasetFile, labelFile) {
        ## Load activity label
        actlabel <- getActivities()
        
        ## Load features
        features <- getFeatures()
        
        ## Load training subject
        subject <- read.csv(subjectFile, sep = "", header = FALSE)
        names(subject) <- "subject"
        
        ## Load training set
        maindata <- read.csv(datasetFile, sep = "", header = FALSE)
        
        ## Load training label
        datalabel <- read.csv(labelFile, sep = "", header = FALSE)
        names(datalabel) <- "activitycode"
        
        ## Merge training label and activity label
        result <- merge(x = datalabel, y = actlabel, by = "activitycode", all.x = TRUE)
        result <- data.frame(subject, result$activity, stringsAsFactors = FALSE)
        names(result) <- c("subject","activity")
        
        ## Set features as the name of training set
        names(maindata) <- features
        
        ## Drop columns that are not mean value (mean()) or standard deviation (std())
        for (col in names(maindata)) {
                if (length(grep("mean\\(\\)|std\\(\\)", col)) > 0) {
                        result[[col]] <- maindata[[col]]
                }
        }
        
        ## Further clean up of header by removing ()
        names(result) <- sapply(names(result), function(x) gsub("\\(\\)","",x))
        result
}

run <- function() {
        ## Get clean training data
        traindata <- processData("./UCI HAR Dataset/train/subject_train.txt","./UCI HAR Dataset/train/X_train.txt","./UCI HAR Dataset/train/y_train.txt")
        
        ## Get clean test data
        testdata <- processData("./UCI HAR Dataset/test/subject_test.txt","./UCI HAR Dataset/test/X_test.txt","./UCI HAR Dataset/test/y_test.txt")
        
        ## Combine training and test data, and output to a text file
        resultdata <- rbind(traindata, testdata)
        #write.table(resultdata, file = "combinedata.csv", row.names = FALSE)
        
        ## Summarise each mean and standard deivation variable using dplyr package then write to text file
        library(dplyr)
        summariseddata <- resultdata %>% group_by(subject, activity) %>% summarise_each(funs(mean))
        write.table(summariseddata, file = "summariseddata.csv", row.names = FALSE)
}