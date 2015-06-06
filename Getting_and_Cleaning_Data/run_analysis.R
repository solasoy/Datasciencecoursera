run_analysis <- function() {
  
  setwd("C://Users//solasoy//Dropbox//R//Coursera")
  
  # Load Dataset ------------------------------
  
  dr = "C://Users//solasoy//Dropbox//R//Coursera"
  writeflag = 0
  
  paste(dr,"//test//","X_test.txt",sep="")
  x1 <- read.table(paste(dr,"//test//","X_test.txt",sep=""))
  x2 <- read.table(paste(dr,"//train//","X_train.txt",sep=""))
  
  
  sub_train <- read.table(paste(dr,"//train//","subject_train.txt",sep=""))
  sub_test <- read.table(paste(dr,"//test//","subject_test.txt",sep=""))
  
  sub_train <- as.numeric(sub_train[,1])
  train_id <- unique(sub_train)
  sub_test <- as.numeric(sub_test[,1])
  test_id  <- unique(sub_test)
  
  ytrain <- read.table(paste(dr,"//train//","y_train.txt",sep=""))
  ytest <- read.table(paste(dr,"//test//","y_test.txt",sep=""))
  
  ytrain <- as.numeric(ytrain[,1])
  ytest <- as.numeric(ytest[,1])
  
  ## (1) Merge training and test set into one data set -----------------------------
  subject <- c(sub_train,sub_test)
  activity <- c(ytrain,ytest)
  
  
  # Combined training and test data with 2 extra columns 
  # containing subject and activity factor variables
  data <- data.frame(cbind(subject,activity,rbind(x2,x1))) 
  
  
  ## (2) Extract variables with mean and std-dev -------------------
  
  variables <- read.table(paste(dr,"//features.txt",sep=""))
  variables <- variables[,2]
  h <- numeric() 
  
  for (i in 1:length(variables)) {
    if (!is.na(grep("-mean()",variables[i])==1 || grep("-std()",variables[i])==1)) {
      if (isTRUE(grep("-meanFreq()",variables[i])==1)) {
        next
      }
      else {
        h <- c(h,i)
      }
      
    }     
  }
  data_reduced <- numeric() 
  
  for (i in 1:length(h)) {
    data_reduced <- data.frame(cbind(data_reduced,data[,2+h[i]]))
  }
  
  ## (3) Replace the variable "activity" with descriptive names ----------------
  
  activity[activity == 1] = 'Walking'
  activity[activity == 2] = 'Walking_Upstairs'
  activity[activity == 3] = 'Walking_Downstairs'
  activity[activity == 4] = 'Sitting'
  activity[activity == 5] = 'Standing'
  activity[activity == 6] = 'Laying'
  data$activity <- activity
  
  
  ## (4) Label the data set with descriptive variable names ---------------
  variables <- variables[h] # Reduced list of variables
  colnames(data_reduced) <- variables
  data <- cbind(data[,1:2],data_reduced)
  
  
  ## (5) From the data set in step 4, create a second, independent --------------------
  ## tidy data set with the average of each variable for each activity and each subject.
  
  col_a <- colnames(data[,1:2]) # subject and activity
  col_b <- colnames(data[,3:dim(data)[2]]) # other variables
  tidy_data <- aggregate(data[col_b], by = data[col_a], FUN=mean)
  
  if (writeflag==1){
    write.table(tidy_data,paste(dr,'//OSoyemi_TidyDataSet.txt',sep=''),row.name=FALSE)
  }
  
  
  tidy_data
   
  
}


