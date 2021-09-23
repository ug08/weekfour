library (data.table); library(dplyr)
##combining test data sets to create one final "test" data set [1]
    subjecttest <- fread("subject_test.txt")
    xtest <- fread("X_test.txt")
    ytest <- fread("y_test.txt")
    
    xtest$ytest <- ytest ; xtest$subjecttest <- subjecttest
    test <- xtest
    
##combining training data sets to create one final "training" data set 
    
    subjecttrain <- fread("subject_train.txt")
    xtrain <- fread("X_train.txt")
    ytrain <- fread("y_train.txt")
    
    xtrain$ytrain <- ytrain ; xtrain$subjecttrain <- subjecttrain
    train <- xtrain
    
##converting "test" and "train" into data frames to ease merging    
    test <- as.data.frame(test)
    train <- as.data.frame(train)

## now, combining "test" and "train" to create a "final" data frame 
    
    final<- bind_rows(test, train)
## deleting duplicate subject ID columns 
    final$subjecttrain <- NULL
    final$ytrain <- NULL

## renaming columns with descriptive variable names 
    names(final)[names(final) == "subjecttest"] <- "subjectid"
    names(final)[names(final) == "ytest"] <- "activityid"

## reading the text file (features.txt) containing the descriptive column names 
    text <- read.delim(file.choose("features.txt"))
    
## cleaning "features.txt" a little for ease of use
    text <-rbind(c("1 tBodyAcc-mean()-X"), text)
    text$sno <- c(1:561)
    names(text) <- c("x", "sno")
    
## filtering out only columns containing info about mean or standard dev (std) [2]
    means <- grep("[Mm]ean",text$x)
    std <- grep("std", text$x)
## storing column numbers with mean and std in a variable called meanstd    
    meanstd<- c(means, std) 
    
## Extracting only the measurements on the mean and standard deviation for each measurement. 
## Making sure to not skip subjectid and activityid, hence including columns 562, 363 as well 
   
    
     final <-final[,c( 562, 563,meanstd)]
## Using descriptive variable names to name the activities in the data set
## creating a vector meanstdnames containing the columns with mean/ std info
    
      meanstdnames <- grep("[Mm]ean|std",text$x, value=TRUE) 
## cleaning text data in meanstd
     meanstdnames <-tolower(meanstdnames)
     meanstdnames <- gsub("-","",meanstdnames)
     meanstdnames <- gsub( "\\(", "", meanstdnames)
     meanstdnames <- gsub( "\\)", "", meanstdnames)
     meanstdnames <- gsub( ",", "", meanstdnames)
     meanstdnames <- sub("t", "time", meanstdnames)
     meanstdnames <- sub("f", "freq", meanstdnames)
     
     meanstdnames <-strsplit(meanstdnames, split=" ")
     secondelement<- function(x) {x[2]}
     meanstdnames <- sapply(meanstdnames, secondelement)

## changing column names of "final" to descriptive variable names [4]
     names(final)[3:88] <- meanstdnames

##Using descriptive activity names to name the activities in the data set [3]
    
     final$activityid <- factor(final$activityid, levels=c(1:6), labels=c("walking",
                                "walking_upstairs", "walking_downstairs","sitting", "standing", "laying"))
     
    final$subjectid <- factor(final$subjectid) 
    final <- final[complete.cases(final),]
     
##creating a second, independent tidy data set with the average of each variable for each activity and each subject [5]    
    final <- final %>% group_by (subjectid, activityid) %>% 
      summarize (
      across(c(timebodyaccmeanx:anglezgravitimeymean), ~mean(.x,na.rm=TRUE))
    )
    final