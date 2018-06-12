library(depmixS4)
library(quantmod)
library(ggplot2)


create_hmm_states_student <- function(data, globalid, max_iterations = 3){
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
  
  for(i in 2:max_iterations){
    HMM2 <- depmix(response = responses, data = TSData, nstates=i, family = family)
    HMMfit2 <- fit(HMM2, verbose = FALSE, emc=em.control(rand=T))
    
    loglik2 <- as.data.frame(logLik(HMMfit2, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
    
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


create_hmm_states_global <- function(data, max_iterations = 3, sorted = F){
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
  
  print(indicator)

  HMM <- depmix(response = responses, data = hmm_data, nstates=1, family = family, ntimes = indicator)
  HMMfit <- fit(HMM, verbose = FALSE, emc=em.control(rand=T))
  
  loglik1 <- as.data.frame(logLik(HMMfit, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
  
  for(i in 2:max_iterations){
    HMM2 <- depmix(response = responses, data = data, nstates=i, family = family, ntimes = indicator)
    HMMfit2 <- fit(HMM2, verbose = FALSE, emc=em.control(rand=T))
    
    loglik2 <- as.data.frame(logLik(HMMfit2, method=c("fb","lystig","classification"),na.allow=TRUE))[1,1]
    
    if(loglik2 > loglik1){
      HMM <- HMM2
      HMMfit <- HMMfit2
      loglik1 <- loglik2
    }
  }
  
  HMMfit
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

### Will create better plotting function tomorrow
compare_with_mca <- function(HMMfit, MCA){
  datetime <- as.character(MCA$timestamp)
  datetime <- as.POSIXlt(datetime, format = "%d/%m/%Y %H:%M:%S")
  MCA$timestamp <- datetime
  
  HMMfit <- posterior(HMMfit)
  HMMfit$timestamp <- datetime
  
  p <- ggplot() + geom_smooth(data = MCA, aes(x = timestamp, y = MCAdim1),
                              colour="darkblue", size=1) +
                geom_smooth(data = HMMfit, aes(x = timestamp, y = S1), fill="red", colour="red", size=1)
  
  print(p)
}


insert_hmm<- function(data){
  data <- remove_unique_students(data)
  data <- data[order(as.character(data$global.id)), ]
  
  HMMfit <- create_hmm_states_global(data, sorted = T)
  post <- posterior(HMMfit)
  
  data$HMMS1 <- post$S1
  data$HMMS2 <- post$S2
  data$HMMS3 <- post$S3
  
  data
}



