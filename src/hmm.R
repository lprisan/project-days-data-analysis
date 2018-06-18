library(depmixS4)
library(quantmod)
library(ggplot2)
library(dplyr)
library(stringr)


create_hmm_states_student <- function(data, globalid, max_states = 3){
  #Selects data of the student passed on to the function
  student_data <- subset(new_data, global.id == globalid)

  datetime <- as.character(student_data[,1])
  datetime <- as.POSIXlt(datetime, format = "%d/%m/%Y %H:%M:%S")
  
  TSData <- data.frame(student_data[,c(5:10)],row.names=datetime)
  
  #remove a column if a student has constant value for it (messes up depmix function)
  degenerate_columns <- c()
  
  for(i in 1:length(TSData[1,])){
    if(length(unique(TSData[,i])) == 1){
      degenerate_columns <- c(degenerate_columns, i)
    }
  }
  
  if(length(degenerate_columns)>0){
    TSData <- TSData[, -(degenerate_columns)]
  }
  
  responses <- get_response_list(names(TSData))

  family <- list()
  
  for(i in 1:length(responses)){
    family[[i]] <- multinomial()
  }
  
  #creates HMMs with different amount of states and compares their likelyhood to return best
  HMM <- depmix(response = responses, data = TSData, nstates=1, family = family, verbose = F)
  HMMfit <- fit(HMM, verbose = FALSE, emc=em.control(rand=T))
  
  loglik1 <- as.data.frame(logLik(HMMfit, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
  
  measures <- data.frame(1, loglik1, AIC(HMMfit), BIC(HMMfit))
  names(measures) <- c("nstates", "logLik", "AIC", "BIC")
  
  for(i in 2:max_states){
    HMM2 <- depmix(response = responses, data = TSData, nstates=i, family = family)
    HMMfit2 <- fit(HMM2, verbose = FALSE, emc=em.control(rand=T))
    
    loglik2 <- as.data.frame(logLik(HMMfit2, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
    
    measures2 <- data.frame(1, loglik2, AIC(HMMfit2), BIC(HMMfit2))
    names(measures2) <- c("nstates", "logLik", "AIC", "BIC")
    measures <- rbind(measures, measures2)
    
    if(loglik2 > loglik1){
      HMM <- HMM2
      HMMfit <- HMMfit2
      loglik1 <- loglik2
    }
  }
  
  HMMfit
}

remove_unique_students <- function(data){
  globalids <- unique(data$global.id)
  
  for(id in globalids){
    if(nrow(subset(data, global.id == id)) == 1){data <- subset(data, global.id != id)}
  }
  
  data
}


create_hmm_states_global <- function(data, max_states = 3, sorted = F, visualise = T){
  previous_id <- " "
  counter <- 0
  indicator <- c()
  
  data <- remove_unique_students(data)
  if(!sorted){
    data <- data[order(as.character(data$global.id)), ]
  }
  
  #creates an indicator for different timeseries, which is required to train depmix on several timeseries
  for(id in data$global.id){
    if(id == previous_id){
      counter <- counter +1
    }
    else{
      previous_id <- id
      indicator <- c(indicator, counter)
      counter <- 1
    }
  }
  
  indicator <- c(indicator, counter)
  indicator <- indicator[2:length(indicator)]
  
  hmm_data <- data[, 5:10]
  
  family <- list(multinomial(),multinomial(),multinomial(),
                 multinomial(),multinomial(),multinomial())
  responses <- c(disengaged ~ 1,looking ~ 1,talking ~ 1,technology ~ 1,resources ~ 1,external ~ 1)
  
  HMM <- depmix(response = responses, data = hmm_data, nstates=1, family = family, ntimes = indicator)
  HMMfit <- fit(HMM, verbose = FALSE, emc=em.control(random.start = FALSE))
  
  loglik1 <- as.data.frame(logLik(HMMfit, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
  
  measures <- data.frame(1, loglik1, AIC(HMMfit), BIC(HMMfit))
  names(measures) <- c("nstates", "logLik", "AIC", "BIC")
  
  for(i in 2:max_states){
    HMM2 <- depmix(response = responses, data = data, nstates=i, family = family, ntimes = indicator)
    HMMfit2 <- fit(HMM2, verbose = FALSE, emc=em.control(random.start = TRUE))
    
    loglik2 <- as.data.frame(logLik(HMMfit2, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
    
    measures2 <- data.frame(i, loglik2, AIC(HMMfit2), BIC(HMMfit2))
    names(measures2) <- c("nstates", "logLik", "AIC", "BIC")
    measures <- rbind(measures, measures2)
    
    if(loglik2 > loglik1){
      HMM <- HMM2
      HMMfit <- HMMfit2
      loglik1 <- loglik2
    }
  }
  
  if(visualise){
      par(mfrow=c(2,1))
      plot(measures$nstates, measures$AIC, main="Minimise (Cirlces = AIC, Diamonds = BIC)")
      #legend(1, 1, legend=c("AIC", "BIC"), pch = c(19, 23), cex=0.8)
      points(measures$nstates, measures$BIC, pch = 23)
      plot(measures$nstates, measures$logLik, main="Maximise", pch = 22)
  }
  
  HMMfit
}

## Just auxillary function to keep knitting time of .Rmd file at reasonalbe levels
plot_criteria <- function(measures){
  par(mfrow=c(2,1))
  plot(measures$nstates, measures$AIC, main="Minimise (Cirlces = AIC, Diamonds = BIC)")
  points(measures$nstates, measures$BIC, pch = 23)
  plot(measures$nstates, measures$logLik, main="Maximise", pch = 22)
  legend(1, 1, legend=c("AIC", "BIC"), pch = c(19, 23))
}


get_response_list <- function(input){
  responses <- list()
  i <- 1
  
  if("disengaged" %in% input){
    responses[[i]] <- disengaged ~ 1
    i <- i + 1
  }
  if("looking" %in% input){
    responses[[i]] <- looking ~ 1
    i <- i + 1
  }
  if("talking" %in% input){
    responses[[i]] <- talking ~ 1
    i <- i + 1
  }
  if("technology" %in% input){
    responses[[i]] <- technology ~ 1
    i <- i + 1
  }
  if("resources" %in% input){
    responses[[i]] <- resources ~ 1
    i <- i + 1
  }
  if("external" %in% input){
    responses[[i]] <- external ~ 1
    i <- i + 1
  }
  
  responses
}


insert_hmm <- function(data, initial_state = 1){
  data <- remove_unique_students(data)
  data <- data[order(as.character(data$global.id)), ]
  
  #set.seed(initial_state)
  
  HMMfit <- create_hmm_states_global(data, sorted = TRUE, visualise = FALSE)
  post <- depmixS4::posterior(HMMfit)
  
  data$HMMPredS <- post$state
  data$HMMS1 <- post$S1
  data$HMMS2 <- post$S2
  data$HMMS3 <- post$S3
  
  data
}


plot_development_by_student <- function(data, globalid){
  student_data <- subset(data, global.id == globalid)
  
  datetime <- as.character(student_data[,1])
  datetime <- as.POSIXlt(datetime, format = "%d/%m/%Y %H:%M:%S")
  
  student_data$timestamp <- datetime
  
  p <- ggplot() + geom_smooth(data = student_data, aes(x = timestamp, y = MCAdim1),
                              colour="darkblue", size=1) +
        geom_smooth(data = student_data, aes(x = timestamp, y = HMMPredS), fill="red", colour="red", size=1)
  
  print(p)
}

state_analysis <- function(dataframe){
  students <- unique(dataframe$global.id)
  state_changes <- 0
  ts_length <- 0
  

  for(student in students){
    #Problem here. The filter function doesn't work as desired and only returns empty data frames
    student_data <- dataframe %>% dplyr::filter(str_detect(global.id, as.character(student)))
    #student_data <- dplyr::filter(dataframe, !grepl(student, as.character(global.id)))
    student_data <- student_data$HMMPredS
    
    last_state <- student_data[1]
    ts_length <- ts_length + length(student_data)
    
    for(i in 2:length(student_data)){
      if(student_data[i]!=last_state){
        last_state <- student_data[i]
        state_changes <- state_changes + 1
      }
    }
  }
  
  return(c(state_changes/length(students), ts_length/length(students)))
}



