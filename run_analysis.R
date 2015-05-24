run_analysis <- function (FolderPath){
      setwd(FolderPath)
      Headers <- read.table("features.txt", header=FALSE) # loading the feature dataset to clean it
      Headers$V2 <- gsub('\\(', "", Headers$V2) # replacing the \\( with nothing 
      Headers$V2 <- gsub('\\)', "", Headers$V2) # replacing the \\) with nothing 
      Headers$V2 <- gsub('-', "_", Headers$V2) # replacing the - with _  
      Headers$V2 <- gsub('BodyBody', "Body", Headers$V2) # replacing the repeated word of Body by a single one
      ActivityLabels <- read.table("activity_labels.txt" , header=FALSE) # loads activity labels
      
      ReadData1 <- read.table("train/X_train.txt", header=FALSE) # loading the train dataset
      colnames(ReadData1) <- Headers[,2] # applies the column name from the rows of the data set Headers
      ReadData2 <- read.table("train/y_train.txt", header=FALSE) # loading the train dataset
      ReadData2 <- sapply(ReadData2$V1, function(x) ActivityLabels$V2[[x]]) ## replacing activity with names
      colnames(ReadData2) <- c("Activity") # naming the column Activity
      ReadData3 <- read.table("train/subject_train.txt", header=FALSE)
      colnames(ReadData3) <- c("Subject") # naming the column Subject
      DataFrame1 <- cbind(ReadData3,ReadData2,ReadData1) # combining the 
      names(DataFrame1)[2]<-"Activity"
      
      ReadData4 <- read.table("test/X_test.txt", header=FALSE)
      colnames(ReadData4) <- Headers[,2]
      ReadData5 <- read.table("test/y_test.txt", header=FALSE)
      ReadData5<- sapply(ReadData5$V1, function(x) ActivityLabels$V2[[x]]) ## replacing activity with names
      colnames(ReadData5) <- c("Activity")
      ReadData6 <- read.table("test/subject_test.txt", header=FALSE)
      colnames(ReadData6) <- c("Subject")
      DataFrame2 <- cbind(ReadData6,ReadData5,ReadData4)
      names(DataFrame2)[2]<-"Activity"
      DataFrame <- rbind(DataFrame1,DataFrame2) # merge the two dataframe 1- 2 to provide a unique file with headers
      
      
      DataFrameClean <- DataFrame[,c("Subject","Activity",colnames(DataFrame)[grep("std()|mean()",colnames(DataFrame))])] # filtering the data.frame to keep only values of mean/stand
      dt <- DataFrameClean %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
      write.table(dt, "Run_Analysis_SmartPhone.txt", row.names=FALSE)
}
