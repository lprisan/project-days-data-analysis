library(reshape2)

processObservationData <- function(data, 
                                   nrlevels=6, 
                                   date=as.POSIXct(strptime("10-01-2018", "%d-%m-%Y")),
                                   project="Isle", 
                                   activitycol=T,
                                   observercol=T,
                                   namecols=c("timestamp","group","activity",
                                              "StudentA","StudentB","StudentC",
                                              "StudentD","StudentE","comment")){
  
  obs_data <- data
  names(obs_data) <- namecols
  
  #Clean up empty columns
  # for(i in ncol(obs_data):1){
  #   if(sum(complete.cases(obs_data[i]))==0 || sum(obs_data[i]!="")==0 || sum(obs_data[i]!="")==1) obs_data <- obs_data[,-i]
  # }
  
  # Cleanup invalid data, before 10am like "2017-10-11 09:44:49 GMT"
  #obs_data <- obs_data[obs_data$timestamp>as.POSIXct("2017-10-11 10:00:00 GMT", origin = "1970-01-01", tz = "GMT"),]
  
  # Student view of the observations
  if(observercol){
    if(!activitycol){
      student_obs <- melt(obs_data, id=c(1:2,ncol(obs_data)), measure=3:(ncol(obs_data)-2))
      names(student_obs)[[4]]<-"student"
    }else{
      student_obs <- melt(obs_data, id=c(1:3,ncol(obs_data)), measure=4:(ncol(obs_data)-2))
      names(student_obs)[[5]]<-"student"
    }
  }else{
    if(!activitycol){
      student_obs <- melt(obs_data, id=1:2, measure=3:(ncol(obs_data)-1))
      names(student_obs)[[3]]<-"student"
    }else{
      student_obs <- melt(obs_data, id=1:3, measure=4:(ncol(obs_data)-1))
      names(student_obs)[[4]]<-"student"
    }
  }
  student_obs$disengaged <- as.numeric(grepl(pattern = "disengaged", x = student_obs$value, fixed = TRUE))
  student_obs$looking <- as.numeric(grepl(pattern = "Looking", x = student_obs$value, fixed = TRUE))
  student_obs$talking <- as.numeric(grepl(pattern = "Talking", x = student_obs$value, fixed = TRUE))
  student_obs$technology <- as.numeric(grepl(pattern = "technology", x = student_obs$value, fixed = TRUE))
  student_obs$resources <- as.numeric(grepl(pattern = "resources", x = student_obs$value, fixed = TRUE))
  student_obs$external <- as.numeric(grepl(pattern = "external", x = student_obs$value, fixed = TRUE))
  student_obs$student.id <- paste(student_obs$group,student_obs$student)
  
  if(observercol){
    if(!activitycol){
      student_obs <- student_obs[,c(1:4,6:ncol(student_obs))]
    }else{
      student_obs <- student_obs[,c(1:5,7:ncol(student_obs))]
    }
  }else{
    if(!activitycol){
      student_obs <- student_obs[,c(1:3,5:ncol(student_obs))]
    }else{
      student_obs <- student_obs[,c(1:4,6:ncol(student_obs))]
    }
  }
  
  
  
  
  student_obs$project <- project
  student_obs$date <- date
  
  student_obs$global.id <- paste(student_obs$project,student_obs$date,student_obs$student.id)
  
  student_obs  
}

