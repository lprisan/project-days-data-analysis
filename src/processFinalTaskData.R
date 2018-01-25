library(dplyr)

processFinalTaskData <- function(data){
  
  newdata <- data
  
  # Add additional columns with the different switches per task and student
  # names of the columns containing the data
  students <- c("tasks.student.A", "tasks.student.B", "tasks.student.C", 
                "tasks.student.D", "tasks.student.E")
  
  for(s in students){
    colname <- paste(s,"reading",sep=".")
    newdata[,colname] <- as.numeric(grepl(pattern = "Reading", x = newdata[,s], fixed = TRUE))
    colname <- paste(s,"ideas",sep=".")
    newdata[,colname] <- as.numeric(grepl(pattern = "ideas", x = newdata[,s], fixed = TRUE))
    colname <- paste(s,"artifacts",sep=".")
    newdata[,colname] <- as.numeric(grepl(pattern = "artifacts", x = newdata[,s], fixed = TRUE))
    colname <- paste(s,"support",sep=".")
    newdata[,colname] <- as.numeric(grepl(pattern = "support", x = newdata[,s], fixed = TRUE))
    colname <- paste(s,"revising",sep=".")
    newdata[,colname] <- as.numeric(grepl(pattern = "Revising", x = newdata[,s], fixed = TRUE))
    
    # Eliminate original columns
    newdata <- select(newdata, -ends_with(s))
  }
  
  
  newdata
  
}