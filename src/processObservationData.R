library(reshape2)
library(gsheet)
library(pracma)
library(magrittr)
library(dplyr)
library(stringr)

processObservationData <- function(data,
                                   date = as.POSIXct(strptime("10-01-2018", "%d-%m-%Y")),
                                   activitycol=F,
                                   observercol=F,
                                   project = c("Isle"),
                                   namecols=c("timestamp","group",
                                              "StudentA","StudentB","StudentC",
                                              "StudentD")){
  
  #data <- data[ , !names(data) %in% "comment"]
  obs_data <- data
  names(obs_data) <- namecols
  
  #Clean up empty columns
  # for(i in ncol(obs_data):1){
  # if(sum(complete.cases(obs_data[i]))==0 || sum(obs_data[i]!="")==0 || sum(obs_data[i]!="")==1)
  #   obs_data <- obs_data[,-i]
  # }

  #Cleanup invalid data, before 10am like "2017-10-11 09:44:49 GMT"
  #obs_data <- obs_data[obs_data$timestamp>as.POSIXct("2017-10-11 10:00:00 GMT", origin = "1970-01-01", tz = "GMT"),]

  #Student view of the observations
  if(observercol){
    if(!activitycol){
      student_obs <- melt(obs_data, id=c(1:2,(ncol(obs_data)-1):ncol(obs_data)), measure=4:(ncol(obs_data)-2), na.rm=T)
     names(student_obs)[[5]]<-"student"
     names(student_obs)[4]<-"observer"
    }else{
      #print(obs_data)
     student_obs <- melt(obs_data, id=c(1:3,(ncol(obs_data)-1):ncol(obs_data)), measure=4:(ncol(obs_data)-2), na.rm=T)
     names(student_obs)[[6]]<-"student"
     names(student_obs)[5]<-"observer"
    }
   }else{
   if(!activitycol){
     student_obs <- melt(obs_data, id=c(1:2,ncol(obs_data)), measure=3:(ncol(obs_data)-1), na.rm=T)
     names(student_obs)[[4]]<-"student"
     #names(student_obs)[[3]]<-"comments"
   }else{
     student_obs <- melt(obs_data, id=c(1:3,ncol(obs_data)), measure=4:(ncol(obs_data)-1), na.rm=T)
     names(student_obs)[[5]]<-"student"
   }
   }
  student_obs$group <- str_replace(student_obs$group, "Team", "Group")
  
 student_obs$disengaged <- as.numeric(grepl(pattern = "disengaged", x = student_obs$value, fixed = TRUE))
 student_obs$looking <- as.numeric(grepl(pattern = "Looking", x = student_obs$value, fixed = TRUE))
 student_obs$talking <- as.numeric(grepl(pattern = "Talking", x = student_obs$value, fixed = TRUE))
 student_obs$technology <- as.numeric(grepl(pattern = "technology", x = student_obs$value, fixed = TRUE))
 student_obs$resources <- as.numeric(grepl(pattern = "resources", x = student_obs$value, fixed = TRUE))
 student_obs$external <- as.numeric(grepl(pattern = "external", x = student_obs$value, fixed = TRUE))
 student_obs$student.id <- paste(student_obs$group,student_obs$student)
 
 if(observercol){
   if(!activitycol){
     student_obs <- student_obs[,c(1:5,7:ncol(student_obs))]
   }else{
     student_obs <- student_obs[,c(1:6,8:ncol(student_obs))]
   }
 }else{
   if(!activitycol){
     student_obs <- student_obs[,c(1:4,6:ncol(student_obs))]
   }else{
     student_obs <- student_obs[,c(1:5,7:ncol(student_obs))]
   }
 }
 # summary(student_obs)
 if(!activitycol){
   student_obs$activity <- " "
   student_obs$activity <- NA
 }
 #student_obs$activity <- factor(student_obs$activity)
 
 if(!observercol){
   student_obs$observer <- " "
   student_obs$observer <- NA
 }
 #student_obs$observer <- factor(student_obs$observer)
 
 student_obs$project <- project
 student_obs$date <- date
 student_obs$date <- as.POSIXct(student_obs$date, format = "%d/%m/%Y")
 
 student_obs$global.id <- paste(student_obs$date,student_obs$student.id)
 
 student_obs
}

processAllObservationData <- function(fileURLs = c("https://docs.google.com/spreadsheets/d/11BPHcSlqwozx3ffOQhQ5i5O8TRVa8xKoaw2uiny8k5A/edit",
                                                     "https://docs.google.com/spreadsheets/d/1kqSu52ZJo0y3cnhAe9fEsv9t7kUdRQ-2cKqSsziQk_M/edit",
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
                                    date = as.Date(character()),global.id = character(), observer = character(),
                                    comments = character())
  
  for (i in 1:length(fileURLs)){
    raw_data <- as.data.frame(gsheet2tbl(fileURLs[i]), check.names = FALSE, fileEncoding="UTF-8-BOM")
    
    raw_data_head <- colnames(raw_data)
    
    
    date_string <- raw_data[1,1]
    sheet_date <- strsplit(date_string, " ")[[1]]
    sheet_date <- sheet_date[1]
    
    project_name <- NA
    project_name <- dplyr::filter(get_project_names(), dates == sheet_date)[1,2]
    
    activity <- F
    observer <- F
    student_count <- 0
    
    #Detects whether there are several activities listed on the Observersheet
    #Problem with encoding of .csv file on Mac: "In which" should be what works on windows 
    if(grepl(pattern = "which", x = raw_data_head[3], fixed = TRUE)){activity <- T}
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
    
    name_cols <- c(name_cols,"comments")
    
    if(observer){name_cols <- c(name_cols,"observer")}
    
    processed_data <- processObservationData(raw_data, date = sheet_date, namecols = name_cols,
                                             activitycol = activity, observercol = observer, project = project_name)
    
    complete_dataset <- rbind.data.frame(complete_dataset, processed_data)
  }
  
  complete_dataset
}

aggregate_on_observations <- function(dataframe){
  return_data <- dataframe %>%
    group_by(timestamp, group) %>%
    summarize(disengaged = sum(disengaged, na.rm = TRUE), looking = sum(looking, na.rm = TRUE),
              talking = sum(talking, na.rm = TRUE), technology = sum(technology, na.rm = TRUE),
              resources = sum(resources, na.rm = TRUE),
              external = sum(external, na.rm = TRUE), activity = first_element(activity),
              observer = first_element(observer), project = first_element(project),
              date = first_element(date), comments = first_element(comments))
  
  
   return_data
}


get_project_names <- function(){
  dates <- c("11/10/2017", "22/11/2017", "06/12/2017", "13/12/2017", "10/01/2018", "18/10/2017", "08/11/2017")
  names <- c("Linnarum", "Linnarum", "Ile Mysterieuse", "Ile Mysterieuse", "Ile Mysterieuse", "Keha", "Keha")
  
  #dates <- as.POSIXct(dates, format = "%d-%m-%Y")
  
  df <- data.frame(dates, names)
  
  df
}

