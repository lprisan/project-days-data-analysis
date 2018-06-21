library(reshape2)
library(gsheet)
library(readr)
library(pracma)
library(dplyr)
library(stringr)

#Based on processObservationData.R


load_intermediate_questionnaire <- function(url){
  raw_data <- as.data.frame(gsheet2tbl(url), check.names = FALSE, fileEncoding="UTF-8")
  
  if(ncol(raw_data)>11){
    raw_data <- raw_data[, 1:11]
  }
  
  if(grepl(pattern = "(quantitative)", x = names(raw_data)[6], fixed = TRUE)){
    raw_data <- raw_data[, c(1:4,6:ncol(raw_data))]
  }
  
  raw_data <- raw_data[, apply(raw_data, 2, function(x) !all(is.na(x)))]
  
  kadriorg <- FALSE
  
  if(grepl(pattern = "Kadriorg's", x = names(raw_data)[ncol(raw_data)], fixed = FALSE)){
    kadriorg <- TRUE
  }
  
  if(ncol(raw_data) == 11){
    names(raw_data) <- c("timestamp", "group", "student", "activity", "contribution",
                       "difficulty", "prepared", "satisfaction", "collaboration", "challenges", "skills")
    raw_data$kadriorg <- NA
  }
  else if(kadriorg){
    names(raw_data) <- c("timestamp", "group", "student", "activity", "contribution",
                         "difficulty", "prepared", "satisfaction", "kadriorg")
    raw_data$collaboration <- NA
    raw_data$challenges <- NA
    raw_data$skills <- NA
  }
  else{
    names(raw_data) <- c("timestamp", "group", "student", "activity", "contribution",
                         "difficulty", "prepared", "satisfaction", "collaboration")
    raw_data$kadriorg <- NA
    raw_data$challenges <- NA
    raw_data$skills <- NA
  }
  
  raw_data$contribution <- parse_number(raw_data$contribution)
  raw_data$difficulty <- parse_number(raw_data$difficulty)
  raw_data$prepared <- parse_number(raw_data$prepared)
  raw_data$satisfaction <- parse_number(raw_data$satisfaction)
  raw_data$collaboration <- parse_number(raw_data$collaboration)
  raw_data$kadriorg <- parse_number(raw_data$kadriorg)
  
  date_string <- raw_data[1,1]
  sheet_date <- as.Date(date_string[1], format = "%d/%m/%Y")
  
  raw_data$date <- sheet_date
  
  raw_data
}


load_all_intermediate_questionnaires <- function(
  urls = c("https://docs.google.com/spreadsheets/d/1TnEb1MEy1PQeYvtFMbEm5s33xqQCX7IpM05hoBELg10/edit#gid=710315571",
          "https://docs.google.com/spreadsheets/d/1NbsjX7vQqgfCKmTJQ-9Zs3zj2dU0CrnMBavXy9qZfqc/edit#gid=76827960",
          "https://docs.google.com/spreadsheets/d/13GZK27me6PYfo7AitvIvWVR_RmhhVP6ypGgc_VaqH2s/edit",
          "https://docs.google.com/spreadsheets/d/1o-JBf9MeReo8ob8LT80OfE3_ZQwFG5Xi9AJtRkxSRUc/edit#gid=1834453828",
          "https://docs.google.com/spreadsheets/d/11cDBX25XCJq89UdmWydXEfMvdHcA4xG-jhbSBgxEnQw/edit#gid=1341825476",
          "https://docs.google.com/spreadsheets/d/1PzrANF_LSSHUqz9exq6QmcEOt3wQ20o7LZ5-PAMl9kI/edit#gid=431307842")){
  
  complete_dataset <- data.frame(timestamp=as.Date(character()), group=character(), activity=character(), 
                                 student = character(), contribution = as.integer(),
                                 difficulty = as.integer(), prepared = as.integer(), satisfaction = as.integer(),
                                 collaboration = as.integer(), kadriorg = as.integer(),
                                 challenges = character(), skills = character(), date = as.Date(character()))
  
  for(url in urls){
    new_dataset <- load_intermediate_questionnaire(url)
    
    complete_dataset <- rbind.data.frame(complete_dataset, new_dataset)
  }
  
  complete_dataset$global.id <- paste(complete_dataset$date, complete_dataset$group, "Student", complete_dataset$student)
  
  complete_dataset$id <- NA
  for(i in 1:nrow(complete_dataset)){
    complete_dataset[i, "id"] <- i
  }
  
  complete_dataset
}


load_final_questionnaire <- function(url){
  raw_data <- as.data.frame(gsheet2tbl(url), check.names = FALSE, fileEncoding="UTF-8-BOM")
  
  if(ncol(raw_data) == 15){
    raw_data <- raw_data[, c(1:4,10:ncol(raw_data))]
  }
  
  names(raw_data) <- c("timestamp", "group", "student", "strategy", "roles", "satisfaction", "improvements",
                       "ease", "tech.problems", "comments")
  
  raw_data$satisfaction <- as.integer(raw_data$satisfaction)
  raw_data$difficulty <- as.integer(raw_data$difficulty)
  
  date_string <- raw_data[1,1]
  sheet_date <- as.Date(date_string[1], format = "%d/%m/%Y")
  
  raw_data$date <- sheet_date
  
  raw_data
}

load_all_final_questionnaires <- function(
  urls = c("https://docs.google.com/spreadsheets/d/1v2E-FQMl4t2qJAAc3Vg8Qo6-7-W_Ajq5ja89QBPk4Fs/edit#gid=1280693886",
            "https://docs.google.com/spreadsheets/d/1JjXYqMGQmTKbAW9q_u7Rl9LWgglIOiVGVKU6iBTijl0/edit#gid=1575889659",
            "https://docs.google.com/spreadsheets/d/1CRUVSsgMcNdJ_kWf3T9aQfMvh3rSQe94fzZMoepOMvw/edit#gid=241912863",
            "https://docs.google.com/spreadsheets/d/1AyTGEA_Sb69Sf-m0HaiAczhr9jjjiFo57yJQVtHetIo/edit#gid=814282282",
            "https://docs.google.com/spreadsheets/d/1R4EKJ1KYiuMqjT41p72XP4Lbsv_5JrDGfWRhQ_CSRNk/edit#gid=52226403")){
  
  complete_dataset <- data.frame(timestamp=as.Date(character()), group=character(),
                                 student = character(), strategy = character(), roles = character(),
                                 satisfaction = as.integer(), improvements = character(), ease = as.integer(),
                                 tech.problems = character(), comments = as.integer(),
                                 date = as.Date(character()))
  
  for(url in urls){
    new_dataset <- load_final_questionnaire(url)
    
    print(names(complete_dataset))
    print(names(new_dataset))
    
    complete_dataset <- rbind.data.frame(complete_dataset, new_dataset)
  }
  
  complete_dataset$global.id <- paste(complete_dataset$date, complete_dataset$group, "Student", complete_dataset$student)
  
  complete_dataset
}


merge_with_data <- function(data, questionnaire){
  students <- as.vector(unique(questionnaire$global.id))
  data$questionnaire <- NA
  
  #data$timestamp <- as.POSIXct(data$timestamp, format = "%d/%m/%Y %I:%M:%S")
  
  
  for(student in students){
    ### grepl doesnt work
    answers <- filter(questionnaire, grepl(student, global.id, fixed = TRUE))
    answers <- answers[order(answers$timestamp), c("timestamp", "date", "id", "global.id")]
    answers <- filter(answers, grepl(student, answers$global.id, fixed = TRUE))
    
    #answers$timestamp <- as.POSIXct(answers$timestamp, format = "%d/%m/%Y %H:%M:%S")
    
    last_time <- as.POSIXct(paste(answers[1,2], "00:00:01", sep = " "), format = "%Y-%m-%d %H:%M:%S")
    
    for(i in 1:nrow(answers)){
        # dplyr::filter(dplyr::filter(dplyr::filter(data, grepl(student,global.id, fixed = T)),
        #     as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M:%S") <= as.POSIXct(answers[1,i],format = "%d/%m/%Y %H:%M:%S")),
        #     as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M:%S") > last_time)$questionnaire <- i

        # if(nrow(layer) > 0){
        #   layer$quesionnaire <- i
        # }
      for(j in 1:nrow(data)){
        date <- as.POSIXct(data[j,c("timestamp")], format = "%d/%m/%Y %H:%M:%S")
        compare <- as.POSIXct(answers[i,1], format = "%d/%m/%Y %H:%M:%S")
        print(paste(date, compare, student))
        if(!is.na(compare)){
          if(date <= compare && date>last_time && strcmp(student, data$global.id[j])){
            data$id[j] <- answers$id[i]
          }
        }
        last_time <- compare
      }
    }
  }
  
  return(data)
}


