##run_analysis.R  written by Nirmal Kumar


##The R code in run_analysis.R are shown below
## downloaded all the files of https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip in home ##folder and copied each file of subdir under the same folder
## so basically all files are under single pwd folder

##used the necessary libariies required to work with data.table data frame

library(data.table)
library(dplyr)

## read each of the files in a separate data table data frame

 dtactlab <- read.table("activity_labels.txt")
 dtfeat <- read.table("features.txt")
 dtxtrain <- read.table("X_train.txt")
 dtsubtrain <-  read.table("subject_train.txt")
 dtytrain <- read.table("y_train.txt")
 dtxtest <- read.table("X_test.txt")
 dtytest <- read.table("y_test.txt")
 dtsubtest <- read.table("subject_test.txt")






  ## 1. Merges the training and the test sets to create one data set.
    
        ##merge two data sets means merge dtxtrain and dtxtest, merge dtsubtest and dtsubtest and merge dtytrain and dtytest
       
	 features <- rbind(dtxtrain,dtxtest)   
        subject<- rbind(dtsubtrain,dtsubtest)
        activity <- rbind(dtytrain,dtytest)

	##re name these new data frames columns name dtfeat has all the variables in second column to used dtfeat[2] and  ##assigned it to colnames of features data frame table
	colnames(features) <- t(dtfeat[2])
	colnames(subject) <- "subject"
	colnames(activity) <- "Activity"

       ## all three above data frames have equal rows so we will have to column bind all above three data frames namely  ##features, subject, activity
   
	     fulldata <- cbind(features,subject,activity)

        ##  dim(fulldata) is now 10299*563 with proper headings



  ## 2.  Extracts only the measurements on the mean and standard deviation for each measurement. 

	  ##gg <-fulldata[,grepl("mean()", colnames(fulldata)) | grepl("std()", colnames(fulldata))] returns 79 columns
	  ## colmeanstd <- grep(".*Mean.*|.*Std.*", names(fulldata), ignore.case=TRUE) returns 86 columns
	  ## as a best practice when it is writen mean and std , it is better to pull maximum data and hence i would go with           ##columnsWithMeanSTD and also add subject & activity	
          ## it will fetch all columsn which has word mean irrespective of case upper/lower
  
          colmeanstd <- grep(".*Mean.*|.*Std.*", names(fulldata), ignore.case=TRUE)     ## this returns all positions so add ##last 2 numerically called subject and Activity
	  colmeanstd  <-  c(colmeanstd, 562, 563)
	  outputdata <- fulldata[, colmeanstd  ]    ##dim(outputdata) is 10299*88	  		  
   

  ## 3. Uses descriptive activity names to name the activities in the data set
  
	 ##currently Activity is numeric and we need to update with dtactlab data frame which has character , so change the data ##type
	 	outputdata$Activity <- as.character(outputdata$Activity)  ## now 1 as become "1"

		outputdata$Activity[outputdata$Activity == 1] <- "Walking"
		outputdata$Activity[outputdata$Activity == 2] <- "Walking Upstairs"
		outputdata$Activity[outputdata$Activity == 3] <- "Walking Downstairs"
		outputdata$Activity[outputdata$Activity== 4] <- "Sitting"
		outputdata$Activity[outputdata$Activity == 5] <- "Standing"
		outputdata$Activity[outputdata$Activity == 6] <- "Laying"
	        outputdata$Activity <- as.factor(outputdata$Activity) 


  ## 4.  Appropriately labels the data set with descriptive variable names. 
	   	names(outputdata) <-gsub("Acc", "Accelerometer", names(outputdata))
		names(outputdata)<-gsub("Gyro", "Gyroscope", names(outputdata))
		names(outputdata)<-gsub("BodyBody", "Body", names(outputdata))
		names(outputdata)<-gsub("Mag", "Magnitude", names(outputdata))
		names(outputdata)<-gsub("^t", "Time", names(outputdata))
		names(outputdata)<-gsub("^f", "Frequency", names(outputdata))
		names(outputdata)<-gsub("-mean()", "Mean", names(outputdata), ignore.case = TRUE)
		names(outputdata)<-gsub("-std()", "STD", names(outputdata), ignore.case = TRUE)
		names(outputdata)<-gsub("-freq()", "Frequency", names(outputdata), ignore.case = TRUE)
		
		names(outputdata)

  ## 5.  From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each ##activity and each subject.
	outputdata$subject <- as.factor(outputdata$subject)		
	outputdata <- data.table(outputdata)
	tidydata <-aggregate(outputdata, by=list(outputdata$subject,outputdata$Activity),  FUN=mean, na.rm=TRUE)
	tidydata <- tidydata[order(tidydata$subject,tidydata$Activity),]
	write.table(tidydata, file = "Tidy.txt", row.names = FALSE)

