mean.std.extract <- function(data.names, keys=c("mean","std")){
  #extract column headers containing either 'mean' or 'std'
  
  #define the search string with a pasted regular expression
  search.string <- "SubjectID|Exercise"
  for(string in keys){
    search.string <- paste(search.string, string, sep = "|")
  }
  
  i <- 1; returned.names <- rep(NA, length(data.names))
  while(i <= length(data.names)){
    if(grepl(search.string, data.names[[i]])){
      returned.names[[i]] <- data.names[[i]]; i <- i+1
    } else {
      i <- i+1
    }
  }
  return(returned.names[!is.na(returned.names)])
}

activity.labeler <- function(activity.column){
  #function for relabeling activities with more descriptive titles
  relabeled.column <- rep(NA, length(activity.column)); i<-1
  activities <- c("WALKING", "WALKING_UP", "WALKING_DOWN",
                  "SITTING", "STANDING", "LAYING")
  while(i <= nrow(activity.column)){
    relabeled.column[[i]] <- activities[activity.column[i,1]]
    i<-i+1
  }
  return(relabeled.column)
}

####################################################################
#See code book for column name key
column.names <- c("SubjectID", "Exercise", "MEAN.X.TBA", "MEAN.Y.TBA", "MEAN.Z.TBA",
                  "STD.X.TBA", "STD.Y.TBA", "STD.Z.TBA", "MEAN.X.TAG", "MEAN.Y.TAG",
                  "MEAN.Z.TAG", "STD.X.TAG", "STD.Y.TAG", "STD.Z.TAG", "MEAN.X.TBA-J",
                  "MEAN.Y.TBA-J", "MEAN.Z.TBA-J", "STD.X.TBA-J", "STD.Y.TBA-J", "STD.Z.TBA-J",
                  "MEAN.X.TBG", "MEAN.Y.TBG", "MEAN.Z.TBG", "STD.X.TBG", "STD.Y.TBG",
                  "STD.Z.TBG", "MEAN.X.TBG-J", "MEAN.Y.TBG-J", "MEAN.Z.TBG-J",
                  "STD.X.TBG-J", "STD.Y.TBG-J", "STD.Z.TBG-J", "MEAN.TBA-MAG", "STD.TBA-MAG",
                  "MEAN.TGA-MAG", "STD.TGA-MAG", "MEAN.TBA-JMAG", "STD.TBA-JMAG", "MEAN.TBG-MAG",
                  "STD.TBG-MAG", "MEAN.TBG-JMAG", "STD.TBG-JMAG",  "MEAN.X.FBA", "MEAN.Y.FBA", 
                  "MEAN.Z.FBA", "STD.X.FBA", "STD.Y.FBA", "STD.Z.FBA", "MEAN-F.X.FBA",
                  "MEAN-F.Y.FBA", "MEAN-F.Z.FBA", "MEAN.X.FBA-J", "MEAN.Y.FBA-J", "MEAN.Z.FBA-J",
                  "STD.X.FBA-J", "STD.Y.FBA-J", "STD.Z.FBA-J", "MEAN-F.X.FBA-J", "MEAN-F.Y.FBA-J",
                  "MEAN-F.Z.FBA-J", "MEAN.X.FBG", "MEAN.Y.FBG", "MEAN.Z.FBG", "STD.X.FBG",
                  "STD.Y.FBG", "STD.Z.FBG", "MEAN-F.X.FBG", "MEAN-F.Y.FBG", "MEAN-F.Z.FBG", 
                  "MEAN.FBA-MAG", "STD.FBA-MAG", "MEAN-F.FBA-MAG", "MEAN.FBA-JMAG", 
                  "STD.FBA-JMAG", "MEAN-F.FBA-JMAG", "MEAN.FBG-MAG", "STD.FBG-MAG", "MEAN-F.FBG-MAG", 
                  "MEAN.FBG-JMAG", "STD.FBG-JMAG", "MEAN-F.FBG-JMAG")

######################################################################

trainPath = './train'
testPath = './test'
### Merge training and test sets to create one data set
loadData <- function(){
  #Function for loading and merging the test and train data
  featureNames <- read.table('features.txt')
  testSubjectID <- read.table('./test/subject_test.txt')
  testLabel <- read.table('./test/y_test.txt')

  testData <- read.table('./test/X_test.txt')

  trainSubjectID <- read.table('./train/subject_train.txt')
  trainLabels <- read.table('./train/y_train.txt')

  trainData <- read.table('./train/X_train.txt')
  colnames(trainData) <- featureNames[,2]
  colnames(testData) <- featureNames[,2]
  testData$SubjectID <- testSubjectID
  trainData$SubjectID <- trainSubjectID
  
  trainData$Exercise <- trainLabels; trainData$SubjectID <- trainSubjectID
  testData$Exercise <- testLabel; testData$SubjectID <- testSubjectID
  
  mainData <- rbind(trainData, testData);
  mainData <- mainData[order(mainData$SubjectID), c(563, 562, 1:561)]
  
  ## Extracts only the measurements on the mean and standard deviation for each measurement
  mainData <- mainData[,mean.std.extract(names(mainData))]
  ## Uses Descriptive activity names to name the activities in the data set
  mainData[,2] <- activity.labeler(mainData[,2])
  ## Appropriately labels the data set with descriptive variable names
  colnames(mainData) <- column.names
  return(mainData)
  }

dataSplitter <- function(data){
  # Return only the mean of every measurement 
  data <- data[[mean.std.extract(names(data), keys = "MEAN")]]
  
  
}





## Creates a second, independent tidy data set with the average of each variable for each activity and each subject
