library(irr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(gsheet)
library(reshape2)
library(dplyr)
library(lubridate)

processObservationData <- function(data, 
                                   nrlevels=6, 
                                   date=as.POSIXct(strptime("10-01-2018", "%d-%m-%Y")),
                                   project="Isle", 
                                   activitycol=T,
                                   observercol=T){
  
  obs_data <- data
  
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
  # summary(student_obs)
  
  student_obs$project <- project
  student_obs$date <- date
  
  student_obs$global.id <- paste(student_obs$project,student_obs$date,student_obs$student.id)
  
  student_obs  
}

getTimeofday <- function(data){
  data$time <- as.POSIXct(strptime(data$timestamp, "%d/%m/%Y %H:%M:%S"))
  data$timeofday <- as.POSIXct(strptime(paste(hour(data$time),
                                              minute(data$time),
                                              second(data$time), sep=":"),
                                        "%H:%M:%S"))
  data$timeofday
}



url_obs <- "https://docs.google.com/spreadsheets/d/1hkkeSRYKKtlpxYAA4jEUddhkv-X5oj6gXzGPKobA5Wc/edit"

data <- as.data.frame(gsheet2tbl(url_obs))
names(data) <- c("timestamp","group","activity",
                 "StudentA","StudentB","StudentC",
                 "StudentD","StudentE","comment","observer")

df <- processObservationData(data)
df$timeofday <- getTimeofday(df)

#Catch: always put the bigger data on data2!
matchObs <- function(data1, data2, col=3){
  data <- data1
  names(data)[col] <- paste(names(data)[col],".A",sep="")
  nameb <- paste(names(data2)[col],".B",sep="")
  data[,nameb] <- NA
  data[,"timeofday.b"] <- NA
  data[,"difftime"] <- NA
  for(i in 1:nrow(data)){
    d2 <- data2[data2$student.id==data[i,"student.id"],]
    diffs <- abs(d2[,1]-data[i,1])
    idx <- which.min(diffs)
    data[i,nameb] <- data2[row.names(d2[idx,]),col]
    data[i,"timeofday.b"] <- as.POSIXct(data2[row.names(d2[idx,]),"timeofday"])
    data[i,"difftime"] <- min(diffs)
  }
  
  data
}

# IRR calculations
results <- data.frame()
for(i in 6:11){ #For each of the variables disengaged, looking, etc
  print(names(df)[i])
  results[nrow(results)+1,"var"] <- names(df)[i]
  ob1a <- df %>% filter(observer=="1-A") %>% select(timeofday,student.id,i)
  ob1b <- df %>% filter(observer=="1-B") %>% select(timeofday,student.id,i)
  gr1 <- matchObs(ob1b,ob1a)
  results[nrow(results),"group"] <- "Group1"
  # Cohen's Kappa
  k1 <- kappa2(gr1[,c(3,4)], "unweighted")
  print(kappa2(gr1[,c(3,4)], "unweighted"))
  results[nrow(results),"kappa"] <- k1$value
  results[nrow(results),"kappa.p"] <- k1$p.value
  # Krippendorf's alpha
  a1 <- kripp.alpha(t(as.matrix(gr1[,c(3,4)])), method="nominal")
  print(kripp.alpha(t(as.matrix(gr1[,c(3,4)])), method="nominal"))
  results[nrow(results),"alpha"] <- a1$value

  results[nrow(results)+1,"var"] <- names(df)[i]
  ob2a <- df %>% filter(observer=="2-A") %>% select(timeofday,student.id,i)
  ob2b <- df %>% filter(observer=="2-B") %>% select(timeofday,student.id,i)
  gr2 <- matchObs(ob2a,ob2b)
  results[nrow(results),"group"] <- "Group2"
  # Cohen's Kappa
  k2 <- kappa2(gr2[,c(3,4)], "unweighted")
  print(kappa2(gr2[,c(3,4)], "unweighted"))
  results[nrow(results),"kappa"] <- k2$value
  results[nrow(results),"kappa.p"] <- k2$p.value
  # Krippendorf's alpha
  a2 <- kripp.alpha(t(as.matrix(gr2[,c(3,4)])), method="nominal")
  print(kripp.alpha(t(as.matrix(gr2[,c(3,4)])), method="nominal"))
  results[nrow(results),"alpha"] <- a2$value
  
  #irrdata <- rbind(gr1,gr2)
}

# All results
ggplot(results, aes(x="ALL", y=kappa))+geom_boxplot()+theme_minimal()
ggplot(results, aes(x="ALL", y=alpha))+geom_boxplot()+theme_minimal()

# By kind of observation
ggplot(results, aes(x=var, y=kappa))+geom_boxplot()+theme_minimal()
ggplot(results, aes(x=var, y=alpha))+geom_boxplot()+theme_minimal()


# By group (set of observers)
ggplot(results, aes(x=group, y=kappa))+geom_boxplot()+theme_minimal()
ggplot(results, aes(x=group, y=alpha))+geom_boxplot()+theme_minimal()





