library(reshape2)
library(gsheet)
library(readr)
library(pracma)
library(dplyr)
library(stringr)
library(tidyverse)

#source("processObservationData.R")

#Based on processObservationData.R


load_intermediate_questionnaire <- function(url, date_type){
  raw_data <- as.data.frame(gsheet2tbl(url), check.names = FALSE, fileEncoding="UTF-8")
  
  #Checks whether there is already a data analysis in the gdoc (right of data) and removes it
  if(ncol(raw_data)>11){
    raw_data <- raw_data[, 1:11]
  }
  
  #Checks whether the contribution is listed twice and removes it
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
  
  #some sheets have european (d/m/y) date format, some american (m/d/y)
  if(strcmp(date_type, "european")){
    sheet_date <- as.Date(date_string[1], format = "%d/%m/%Y")
    raw_data$timestamp <- format(as.POSIXct(raw_data$timestamp, format = "%d/%m/%Y %H:%M:%S"), "%d/%m/%Y %H:%M:%S")
  }
  else{
    sheet_date <- as.Date(date_string[1], format = "%m/%d/%Y")
    raw_data$timestamp <- format(as.POSIXct(raw_data$timestamp, format = "%m/%d/%Y %H:%M:%S"), "%d/%m/%Y %H:%M:%S")
  }
  
  raw_data$date <- sheet_date
  
  project <- dplyr::filter(get_project_names(), dates == strftime(sheet_date, format = "%d/%m/%Y"))[1,2]
  raw_data$project <- project
  
  raw_data
}


load_all_intermediate_questionnaires <- function(
  urls = c("https://docs.google.com/spreadsheets/d/1TnEb1MEy1PQeYvtFMbEm5s33xqQCX7IpM05hoBELg10/edit#gid=710315571",
          "https://docs.google.com/spreadsheets/d/1NbsjX7vQqgfCKmTJQ-9Zs3zj2dU0CrnMBavXy9qZfqc/edit#gid=76827960",
          "https://docs.google.com/spreadsheets/d/13GZK27me6PYfo7AitvIvWVR_RmhhVP6ypGgc_VaqH2s/edit",
          "https://docs.google.com/spreadsheets/d/1o-JBf9MeReo8ob8LT80OfE3_ZQwFG5Xi9AJtRkxSRUc/edit#gid=1834453828",
          "https://docs.google.com/spreadsheets/d/11cDBX25XCJq89UdmWydXEfMvdHcA4xG-jhbSBgxEnQw/edit#gid=1341825476",
          "https://docs.google.com/spreadsheets/d/1PzrANF_LSSHUqz9exq6QmcEOt3wQ20o7LZ5-PAMl9kI/edit#gid=431307842"),
  date_format = c("european", "american", "european", "european", "european", "european")){
  
  complete_dataset <- data.frame(timestamp=as.Date(character()), group=character(), activity=character(), 
                                 student = character(), contribution = as.integer(),
                                 difficulty = as.integer(), prepared = as.integer(), satisfaction = as.integer(),
                                 collaboration = as.integer(), kadriorg = as.integer(),
                                 challenges = character(), skills = character(), date = as.Date(character()),
                                 project = as.character())
  
  for(i in 1:length(urls)){
    new_dataset <- load_intermediate_questionnaire(urls[i], date_format[i])
    
    complete_dataset <- rbind.data.frame(complete_dataset, new_dataset)
  }
  
  complete_dataset$global.id <- paste(complete_dataset$date, complete_dataset$group, "Student", complete_dataset$student)
  
  complete_dataset$id <- NA
  for(i in 1:nrow(complete_dataset)){
    complete_dataset[i, "id"] <- i
  }
  
  complete_dataset
}


load_final_questionnaire <- function(url, date_type){
  raw_data <- as.data.frame(gsheet2tbl(url), check.names = FALSE, fileEncoding="UTF-8-BOM")
  
  if(ncol(raw_data) == 15){
    raw_data <- raw_data[, c(1:4,10:ncol(raw_data))]
  }
  
  names(raw_data) <- c("timestamp", "group", "student", "strategy", "roles", "satisfaction", "improvements",
                       "ease", "tech.problems", "comments")
  
  raw_data$satisfaction <- as.integer(raw_data$satisfaction)
  raw_data$ease <- as.integer(raw_data$ease)
  
  date_string <- raw_data[1,1]
  
  date_string <- strsplit(date_string, " ")[[1]]
  date_string <- date_string[1]

  if(strcmp(date_type, "european")){
    sheet_date <- as.Date(date_string[1], format = "%d/%m/%Y")
    raw_data$timestamp <- format(as.POSIXct(raw_data$timestamp, format = "%d/%m/%Y %H:%M:%S"), "%d/%m/%Y %H:%M:%S")
  }
  else{
    sheet_date <- as.Date(date_string[1], format = "%m/%d/%Y")
    raw_data$timestamp <- format(as.POSIXct(raw_data$timestamp, format = "%m/%d/%Y %H:%M:%S"), "%d/%m/%Y %H:%M:%S")
  }
  
  raw_data$date <- sheet_date
  
  project <- dplyr::filter(get_project_names(), dates == strftime(sheet_date, format = "%d/%m/%Y"))[1,2]
  raw_data$project <- project
  
  raw_data
}

load_all_final_questionnaires <- function(
  urls = c("https://docs.google.com/spreadsheets/d/1v2E-FQMl4t2qJAAc3Vg8Qo6-7-W_Ajq5ja89QBPk4Fs/edit#gid=1280693886",
            "https://docs.google.com/spreadsheets/d/1JjXYqMGQmTKbAW9q_u7Rl9LWgglIOiVGVKU6iBTijl0/edit#gid=1575889659",
            "https://docs.google.com/spreadsheets/d/1CRUVSsgMcNdJ_kWf3T9aQfMvh3rSQe94fzZMoepOMvw/edit#gid=241912863",
            "https://docs.google.com/spreadsheets/d/1AyTGEA_Sb69Sf-m0HaiAczhr9jjjiFo57yJQVtHetIo/edit#gid=814282282",
            "https://docs.google.com/spreadsheets/d/1R4EKJ1KYiuMqjT41p72XP4Lbsv_5JrDGfWRhQ_CSRNk/edit#gid=52226403"),
  date_format = c("american", "european", "european", "european", "european")){
  
  complete_dataset <- data.frame(timestamp=as.Date(character()), group=character(),
                                 student = character(), strategy = character(), roles = character(),
                                 satisfaction = as.integer(), improvements = character(), ease = as.integer(),
                                 tech.problems = character(), comments = as.integer(),
                                 date = as.Date(character()), project = as.character())
  
  for(i in 1:length(urls)){
    new_dataset <- load_final_questionnaire(urls[i], date_format[i])
    
    #print(names(complete_dataset))
    #print(names(new_dataset))
    
    complete_dataset <- rbind.data.frame(complete_dataset, new_dataset)
  }
  
  complete_dataset$global.id <- paste(complete_dataset$date, complete_dataset$group, "Student", complete_dataset$student)
  
  complete_dataset$id <- NA
  for(i in 1:nrow(complete_dataset)){
    complete_dataset[i, "id"] <- i
  }
  
  complete_dataset
}





match_with_data <- function(data, iq, fq){
   students <- unique(fq$global.id)
   data$int.questionnaire <- NA
   data$fin.questionnaire <- NA
   data$timestamp <- as.POSIXct(data$timestamp, format = "%d/%m/%Y %H:%M:%S")
   
   ret <- dplyr::filter(data, disengaged == -1)
   
   for(n in 1:length(students)){
     i_answers <- iq %>% filter(global.id == students[[n]])
     #i_answers$timestamp <- as.POSIXct(i_answers$timestamp, format = "%d/%m/%Y %H:%M:%S")
     #i_answers <- i_answers[order(i_answers$timestamp),]
     #i_answers <- i_answers %>% dplyr::filter(global.id == student)
     #i_answers <- dplyr::filter(i_answers, grepl(student, i_answers$global.id, fixed = TRUE))
     
     f_answers <- fq %>% filter(global.id == students[[n]])
     #f_answers$timestamp <- as.POSIXct(f_answers$timestamp, format = "%d/%m/%Y %H:%M:%S")
     #f_answers <- answers[order(-f_answers$timestamp), c("timestamp", "date", "id", "global.id")]
     #f_answers <- dplyr::filter(f_answers, grepl(student, f_answers$global.id, fixed = TRUE))
     
     filtered_data <- data %>% filter(global.id == students[[n]])
     filtered_data$timestamp <- as.POSIXct(filtered_data$timestamp, format = "%d/%m/%Y %H:%M:%S")
     
     if(nrow(i_answers) > 0 && nrow(filtered_data) > 0){
      for(i in nrow(i_answers):1){
        for(j in 1:nrow(filtered_data)){
          #print(filtered_data[j, "timestamp"])
          #print(i_answers[i, "timestamp"])
          if(!is.na(filtered_data[j, "timestamp"]) &&
             as.POSIXct(filtered_data[j, "timestamp"], format = "%Y-%m-%d %H:%M:%S")
             <= as.POSIXct(i_answers[i, "timestamp"], format = "%d/%m/%Y %H:%M:%S")){
            filtered_data[j, "int.questionnaire"] <- i_answers[i, "id"]
          }
        }
      }
       filtered_data$fin.questionnaire <- f_answers$id[1]
     }
     ret <- rbind.data.frame(ret, filtered_data)
   }
   
   
   ret
}

merge_and_aggregate <- function(data, iq, fq, sparseAE = F){
  #### The dataset given to this function must contain exactly three MCA dimension variables and exactly
  #### three AE unit variables
  
  data <- dplyr::filter(data, !is.na(data$int.questionnaire))
  
  ## Aggregates data by intermediate and final questionnaires
  if(sparseAE){
    data <- data %>%
      dplyr::group_by(int.questionnaire, fin.questionnaire) %>%
      dplyr::summarize(global.id = first_element(global.id),
        disengaged = mean(disengaged, na.rm = TRUE), looking = mean(looking, na.rm = TRUE),
              talking = mean(talking, na.rm = TRUE), technology = mean(technology, na.rm = TRUE),
              resources = mean(resources, na.rm = TRUE),
              external = mean(external, na.rm = TRUE), activity = first_element(activity),
              observer = first_element(observer), project = first_element(project),
              date = first_element(date), comments = first_element(comments),
              MCAdim1 = mean(MCAdim1, na.rm = TRUE), MCAdim2 = mean(MCAdim2, na.rm = TRUE),
              MCAdim3 = mean(MCAdim3, na.rm = TRUE), AEdim1 = mean(AEdim1, na.rm = TRUE),
              AEdim2 = mean(AEdim2, na.rm = TRUE), AEdim3 = mean(AEdim3, na.rm = TRUE),
              SpAEdim1 = mean(AEdim1, na.rm = TRUE),
              SpAEdim2 = mean(AEdim2, na.rm = TRUE), SpAEdim3 = mean(AEdim3, na.rm = TRUE),n = n())
  }
  else{
    data <- data %>%
      dplyr::group_by(int.questionnaire, fin.questionnaire) %>%
      dplyr::summarize(global.id = first_element(global.id),
                       disengaged = mean(disengaged, na.rm = TRUE), looking = mean(looking, na.rm = TRUE),
                       talking = mean(talking, na.rm = TRUE), technology = mean(technology, na.rm = TRUE),
                       resources = mean(resources, na.rm = TRUE),
                       external = mean(external, na.rm = TRUE), activity = first_element(activity),
                       observer = first_element(observer), project = first_element(project),
                       date = first_element(date), comments = first_element(comments),
                       MCAdim1 = mean(MCAdim1, na.rm = TRUE), MCAdim2 = mean(MCAdim2, na.rm = TRUE),
                       MCAdim3 = mean(MCAdim3, na.rm = TRUE), AEdim1 = mean(AEdim1, na.rm = TRUE),
                       AEdim2 = mean(AEdim2, na.rm = TRUE), AEdim3 = mean(AEdim3, na.rm = TRUE), n = n())
  }
  
  
  data$contribution <- NA
  data$difficulty <- NA
  data$prepared <- NA
  data$i_satisfaction <- NA
  data$collaboration <- NA
  data$kariorg <- NA
  data$challanges <- NA
  data$skills <- NA
  
  data$strategy <- NA
  data$roles <- NA
  data$f_satisfaction <- NA
  data$improvements <- NA
  data$ease <- NA
  data$tech.problems <- NA
  data$comments <- NA
  
  ## Fetches the values from the matching intermediate and final questionnaires
  for(i in 1:nrow(data)){
    intermediate <- dplyr::filter(iq, id == data[i, "int.questionnaire"][[1]])
    final <- dplyr::filter(fq, id == data[i, "fin.questionnaire"][[1]])
    
    data$contribution[i] <- intermediate[1, "contribution"]
    data$difficulty[i] <- intermediate[1, "difficulty"]
    data$prepared[i] <- intermediate[1, "prepared"]
    data$i_satisfaction[i] <- intermediate[1, "satisfaction"]
    data$collaboration[i] <- intermediate[1, "collaboration"]
    data$kariorg[i] <- intermediate[1, "kadriorg"]
    data$challanges[i] <- intermediate[1, "challenges"]
    data$skills[i] <- intermediate[1, "skills"]
    
    data$strategy[i] <- final[1, "strategy"]
    data$roles[i] <- final[1, "roles"]
    data$f_satisfaction[i] <- final[1, "satisfaction"]
    data$improvements[i] <- final[1, "improvements"]
    data$ease[i] <- final[1, "ease"]
    data$tech.problems[i] <- final[1, "tech.problems"]
    data$comments[i] <- final[1, "comments"]
  }
  
  data <- data[, 3:ncol(data)]
  
  data
}


getTimeofday <- function(date){
  timeofday <- as.POSIXct(strptime(paste(hour(date),
                                              minute(date),
                                              second(date), sep=":"),
                                        "%H:%M:%S"))
  timeofday
}

first_element <- function(data){
  ret <- data[1]
  ret
}

