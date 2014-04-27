# computer.name <- Sys.info()["nodename"]
# 
# 
# # working directory
# 
# if (computer.name == "MALUCH-T420")    
#   work.dir <- "d://uczelnia/coursera/Getting and Cleaning Data/GaCD_peer/"
# 
# if (computer.name == "YOUR-COMPUTER-NAME")    
#   work.dir <- "your/directory"
# 
# setwd(work.dir)



# directory with original data files
# data.dir <- paste(work.dir, "/UCI HAR Dataset/", sep="")
data.dir <- getwd()

# checking if all nedded files are there (only in the root dir of the data set)
needed.files.and.dirs <- c("activity_labels.txt", "features.txt", "features_info.txt", 
                           "README.txt", "test", "train")
if (sum(na.omit(list.files(data.dir)%in%needed.files.and.dirs)) == 6){
  print("Performing analysis")

  
  # getting activity names and their links to the class labels
  activity.names <- read.table(paste(data.dir, "/activity_labels.txt", sep=""))
  
  # getting feature labels vector (length 561)
  feature.labels <- as.vector(read.table(paste(data.dir, "/features.txt", sep=""))[,2])
  
  
  ################
  # TRAINING SET #
  ################
  
  # setting training set directory
  train.dir <- paste(data.dir, "/train", sep="")
  
  # getiing X train
  X.train.path <- paste(train.dir, "/X_train.txt", sep="")
  # reading file content (dim 7352  561)
  if(file.exists(X.train.path)){
    train.set <- read.table(X.train.path)
  }else{
    simpleError("!!! missing file !!!" )
    simpleError(X.train.path)
  }
  
  # getiing training class labels (linked with activity names)
  y.train.path <- paste(train.dir, "/y_train.txt", sep="")
  # reading file content (length 7352)
  if(file.exists(y.train.path)){
    train.class <- as.vector(read.table(y.train.path)[,1])
  }else{
    simpleError("!!! missing file !!!" )
    simpleError(y.train.path)
  }
  
  # getting subject train (length 7352)
  subject.train.path <- paste(train.dir, "/subject_train.txt", sep="")
  if(file.exists(subject.train.path)){
    train.subject <- as.vector(read.table(subject.train.path)[,1])
  }else{
    simpleError("!!! missing file !!!")
    simpleError(subject.train.path)
  }
  
  ###########################
  # READING DATA FROM FILES #
  ###########################
  
  ###############
  # TESTING SET #
  ###############
  
  # setting testing set directory
  test.dir <- paste(data.dir, "/test", sep="")
  
  # getiing X test
  X.test.path <- paste(test.dir, "/X_test.txt", sep="")
  # reading file content (dim 2947  561)
  if(file.exists(X.test.path)){
    test.set <- read.table(X.test.path)
  }else{
    simpleError("!!! missing file !!!" )
    simpleError(X.test.path)
  }
  
  # getiing testing class labels (linked with activity names)
  y.test.path <- paste(test.dir, "/y_test.txt", sep="")
  # reading file content (length 2947)
  if(file.exists(y.test.path)){
    test.class <- as.vector(read.table(y.test.path)[,1])
  }else{
    simpleError("!!! missing file !!!" )
    simpleError(y.test.path)
  }
  
  # getting subject test (length 2947)
  subject.test.path <- paste(test.dir, "/subject_test.txt", sep="")
  if(file.exists(subject.test.path)){
    test.subject <- as.vector(read.table(subject.test.path)[,1])
  }else{
    simpleError("!!! missing file !!!")
    simpleError(subject.test.path)
  }

  #################################################################
  # 1. Merges the training and the test sets to create one data set.
  #################################################################
  
  # updating features names
  feature.labels.merged <- c("subject.id", "class", feature.labels)
  
  #   merging test set data into one data.frame
  test.set.merged <- cbind(test.subject, test.class, test.set)
  names(test.set.merged) <- feature.labels.merged
  
  #   merging train set data into one data.frame
  train.set.merged <- cbind(train.subject, train.class, train.set)
  names(train.set.merged) <- feature.labels.merged

#   merging two data sets into one data.frame
  data.set <- rbind(test.set.merged, train.set.merged)
  
  

  #################################################################
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
  #################################################################

  library(stringr)
  # finding "mean()" and "std()" substrings in features labels
  mean.vect <- str_detect(feature.labels.merged, fixed("mean()"))
  std.vect <- str_detect(feature.labels.merged, fixed("std()"))
  vect <- (mean.vect | std.vect)
  # subject id and class are still needed
  vect[1:2] <- TRUE
  
  #getting only columns of subject id, class and mean and standard deviation for each measurement
  data.set <- data.set[,which(vect)]

  #################################################################
  # 3. Uses descriptive activity names to name the activities in the data set
  # 4. Appropriately labels the data set with descriptive activity names. 
  #################################################################
  
  # the activity_labels.txt file was read in line 25 of this file
  data.set$class <- factor(activity.names$V2[data.set$class])
  
  
  
  #################################################################
  # 5. Creates a second, independent tidy data set with the average of each variable 
  # for each activity and each subject. 
  #################################################################

  # using reshape2 package to reshape data.set and than creating requested tidy data set 
  # of mean of each variable for each activity and each subject. 
  library(reshape2)
  melt.set <- melt(data.set, id=c("subject.id", "class"))
  tidy.set <- dcast(melt.set, subject.id + class ~ variable, mean)


  
  # saving tidy data set into .txt format file
  write.table(tidy.set, paste(data.dir, "/tidy_set.txt"))

  

}else{
  simpleError("Something is wrong with the files or dir names - NOT performing analysis")
}























