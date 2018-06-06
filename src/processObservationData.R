library(reshape2)
library(gsheet)

processObservationData <- function(data,
                                   date=as.POSIXct(strptime("10-01-2018", "%d-%m-%Y")),
                                   project="Isle", 
                                   activitycol=F,
                                   observercol=F,
                                   namecols=c("timestamp","group",
                                              "StudentA","StudentB","StudentC",
                                              "StudentD")){
  
  #data <- data[ , !names(data) %in% "comment"]
  obs_data <- data
  names(obs_data) <- namecols
  

  #Clean up empty columns
  for(i in ncol(obs_data):1){
  if(sum(complete.cases(obs_data[i]))==0 || sum(obs_data[i]!="")==0 || sum(obs_data[i]!="")==1)
    obs_data <- obs_data[,-i]
  }

  #Cleanup invalid data, before 10am like "2017-10-11 09:44:49 GMT"
  #obs_data <- obs_data[obs_data$timestamp>as.POSIXct("2017-10-11 10:00:00 GMT", origin = "1970-01-01", tz = "GMT"),]

  #Student view of the observations
  if(observercol){
    if(!activitycol){
     student_obs <- melt(obs_data, id=c(1:2,ncol(obs_data)), measure=3:(ncol(obs_data)-2), na.rm=T)
     names(student_obs)[[4]]<-"student"
    }else{
      #print(obs_data)
     student_obs <- melt(obs_data, id=c(1:3,ncol(obs_data)), measure=4:(ncol(obs_data)-2), na.rm=T)
     names(student_obs)[[5]]<-"student"
     head(student_obs)
    }
   }else{
   if(!activitycol){
     student_obs <- melt(obs_data, id=1:2, measure=3:(ncol(obs_data)-1), na.rm=T)
     names(student_obs)[[3]]<-"student"
   }else{
     student_obs <- melt(obs_data, id=1:3, measure=4:(ncol(obs_data)-1), na.rm=T)
     names(student_obs)[[4]]<-"student"
   }
  }
  
  new_names <- names(student_obs)
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
 # summary(student_obs)
 
 if(!activitycol){
   student_obs$activity <- "Standard"
 }
 
 if(!observercol){
   student_obs$observer <- "1-A"
 }
 
 student_obs$project <- project
 student_obs$date <- date

 student_obs$global.id <- paste(student_obs$project,student_obs$date,student_obs$student.id)

 student_obs
}

processAllObservationData <- function(fileURLs = c("https://docs.google.com/spreadsheets/d/11BPHcSlqwozx3ffOQhQ5i5O8TRVa8xKoaw2uiny8k5A/edit",
                                                     "https://docs.google.com/spreadsheets/d/1kqSu52ZJo0y3cnhAe9fEsv9t7kUdRQ-2cKqSsziQk_M/edit",
                                                     "https://docs.google.com/spreadsheets/d/1FSqm57ygxaIFa5V5oUCx7ht21ayuxMm7MO2IeczZ7ww/edit",
                                                     "https://docs.google.com/spreadsheets/d/13Od5UuY5LLh2E5EPVD17IP6dO3x_xTpxyVYg9BQmIdE/edit",
                                                     "https://docs.google.com/spreadsheets/d/1hkkeSRYKKtlpxYAA4jEUddhkv-X5oj6gXzGPKobA5Wc/edit",
                                                     "https://docs.google.com/spreadsheets/d/1FSqm57ygxaIFa5V5oUCx7ht21ayuxMm7MO2IeczZ7ww/edit",
                                                     "https://docs.google.com/spreadsheets/d/1zcq0lGaavcxjdeEOgc6uNtMGAEy_NqXg4ufmnFaUjJo/edit",
                                                     "https://docs.google.com/spreadsheets/d/1EoAfPj2hFcmlHo2ndfunLV4krkgJ2a69KlCi5NoYp0k/edit")){
  

              
  
  complete_dataset <- data.frame(timestamp=as.Date(character()), group=character(), activity=character(), 
                                    student = character(), disengaged = as.integer(),
                                    looking = as.integer(), talking = as.integer(), technology = as.integer(),
                                    resources = as.integer(), external = as.integer(),
                                    student.id = character(), project = character(),
                                    date = as.Date(character()),global.id = character(), observer = character())
  
  for (i in 1:length(fileURLs)){
    raw_data <- as.data.frame(gsheet2tbl(fileURLs[i]))
    
    raw_data_head <- colnames(raw_data)
    
    
    date_string <- raw_data[1,1]
    sheet_date <- as.Date(date_string[1], format = "%d/%m/%Y")
    
    activity <- F
    observer <- F
    student_count <- 0
    
    #Detects whether there are several activities listed on the Observersheet
    #Problem with encoding of .csv file on Mac: "In which" should be what works on windows 
    if(grepl(pattern = "In.which", x = raw_data_head[3], fixed = TRUE)){activity <- T}
    if(grepl(pattern = "observer", x = raw_data_head[length(raw_data_head)], fixed = TRUE)){observer <- T}
    
    #Counts the number of students per group on the the Observersheet
    for (name in raw_data_head){
      if(grepl(pattern = "Student", x = name, fixed = TRUE)){student_count <- student_count + 1}
    }
    
    name_cols <- c("timestamp","group")
    
    if(activity){name_cols <- c(name_cols,"activity")}
    
    if(student_count>0){name_cols <- c(name_cols,"Student A")}
    if(student_count>1){name_cols <- c(name_cols,"Student B")}
    if(student_count>2){name_cols <- c(name_cols,"Student C")}
    if(student_count>3){name_cols <- c(name_cols,"Student D")}
    if(student_count>4){name_cols <- c(name_cols,"Student E")}
    if(student_count>5){name_cols <- c(name_cols,"Student F")}
    if(student_count>6){name_cols <- c(name_cols,"Student G")}
    if(student_count>7){name_cols <- c(name_cols,"Student H")}
    if(student_count>8){name_cols <- c(name_cols,"Student I")}
    if(student_count>9){name_cols <- c(name_cols,"Student J")}
    
    name_cols <- c(name_cols,"ann")
    
    if(observer){name_cols <- c(name_cols,"observer")}
    

    
    processed_data <- processObservationData(raw_data, date = sheet_date, namecols = name_cols,
                                             activitycol = activity, observercol = observer)
    
    complete_dataset <- rbind(complete_dataset, processed_data)
  }
  
  complete_dataset
}

