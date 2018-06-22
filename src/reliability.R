library(irr)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(gsheet)
library(reshape2)
library(dplyr)
library(lubridate)



getTimeofday <- function(data){
  data$time <- as.POSIXct(strptime(data$timestamp, "%d/%m/%Y %H:%M:%S"))
  data$timeofday <- as.POSIXct(strptime(paste(hour(data$time),
                                              minute(data$time),
                                              second(data$time), sep=":"),
                                        "%H:%M:%S"))
  data$timeofday
}



#url_obs <- "https://docs.google.com/spreadsheets/d/1hkkeSRYKKtlpxYAA4jEUddhkv-X5oj6gXzGPKobA5Wc/edit"

# data <- as.data.frame(gsheet2tbl(url_obs))
# names(data) <- c("timestamp","group","activity",
#                  "StudentA","StudentB","StudentC",
#                  "StudentD","StudentE","comment","observer")

#df <- processObservationData(data)
#df$timeofday <- getTimeofday(df)

##Catch: always put the bigger data on data2!
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
test_discrete_vars_for_observers <- function(df, selected_columns = c("disengaged", "looking",
                                                  "talking","technology", "resources", "external")){
df$timeofday <- getTimeofday(df)
results <- data.frame()

for(i in selected_columns){ #For each of the variables disengaged, looking, etc
  #print(i)
  results[nrow(results)+1,"var"] <- i
  ob1a <- df %>% dplyr::filter(observer=="1-A") %>% dplyr::select(timeofday,student.id,i)
  ob1b <- df %>% dplyr::filter(observer=="1-B") %>% dplyr::select(timeofday,student.id,i)
  gr1 <- matchObs(ob1b,ob1a)
  results[nrow(results),"group"] <- "Group1"
  # Cohen's Kappa
  k1 <- kappa2(gr1[,c(3,4)], "unweighted")
  #print(kappa2(gr1[,c(3,4)], "unweighted"))
  results[nrow(results),"kappa"] <- k1$value
  results[nrow(results),"kappa.p"] <- k1$p.value
  # Krippendorf's alpha
  a1 <- kripp.alpha(t(as.matrix(gr1[,c(3,4)])), method="nominal")
  #print(kripp.alpha(t(as.matrix(gr1[,c(3,4)])), method="nominal"))
  results[nrow(results),"alpha"] <- a1$value

  results[nrow(results)+1,"var"] <- i
  ob2a <- df %>% dplyr::filter(observer=="2-A") %>% dplyr::select(timeofday,student.id,i)
  ob2b <- df %>% dplyr::filter(observer=="2-B") %>% dplyr::select(timeofday,student.id,i)
  gr2 <- matchObs(ob2a,ob2b)
  results[nrow(results),"group"] <- "Group2"
  # Cohen's Kappa
  k2 <- kappa2(gr2[,c(3,4)], "unweighted")
  #print(kappa2(gr2[,c(3,4)], "unweighted"))
  results[nrow(results),"kappa"] <- k2$value
  results[nrow(results),"kappa.p"] <- k2$p.value
  # Krippendorf's alpha
  a2 <- kripp.alpha(t(as.matrix(gr2[,c(3,4)])), method="nominal")
  #print(kripp.alpha(t(as.matrix(gr2[,c(3,4)])), method="nominal"))
  results[nrow(results),"alpha"] <- a2$value
  
  #irrdata <- rbind(gr1,gr2)
}
results
}

normalise <- function(data, columns){
  for(column in columns){
    data[, column] <- (data[, column] - mean(data[, column])) / sd(data[, column])
  }
  
  data
}

test_cont_vars_for_observers <- function(data, selected_columns = c("AEdim1", "AEdim2", "AEdim3")){
  data$timeofday <- getTimeofday(data)
  
  results <- data.frame()
  for(i in selected_columns){ #For each of the variables disengaged, looking, etc
    #print(i)
    results[nrow(results)+1,"var"] <- i
    ob1a <- data %>% dplyr::filter(observer=="1-A") %>% dplyr::select(timeofday,student.id,i)
    ob1b <- data %>% dplyr::filter(observer=="1-B") %>% dplyr::select(timeofday,student.id,i)
    gr1 <- matchObs(ob1b,ob1a)
    results[nrow(results),"group"] <- "Group1"
    # Krippendorf's alpha
    a1 <- kripp.alpha(t(as.matrix(gr1[,c(3,4)])), method="interval")
    #print(kripp.alpha(t(as.matrix(gr1[,c(3,4)])), method="interval"))
    results[nrow(results),"alpha"] <- a1$value
    
    results[nrow(results)+1,"var"] <- names(df)[i]
    ob2a <- data %>% dplyr::filter(observer=="2-A") %>% dplyr::select(timeofday,student.id,i)
    ob2b <- data %>% dplyr::filter(observer=="2-B") %>% dplyr::select(timeofday,student.id,i)
    gr2 <- matchObs(ob2a,ob2b)
    results[nrow(results),"group"] <- "Group2"
    # Krippendorf's alpha
    a2 <- kripp.alpha(t(as.matrix(gr2[,c(3,4)])), method="interval")
    #print(kripp.alpha(t(as.matrix(gr2[,c(3,4)])), method="interval"))
    results[nrow(results),"alpha"] <- a2$value
  }
  
  results
}

# # All results
# ggplot(results, aes(x="ALL", y=kappa))+geom_boxplot()+theme_minimal()
# ggplot(results, aes(x="ALL", y=alpha))+geom_boxplot()+theme_minimal()
# 
# # By kind of observation
# ggplot(results, aes(x=var, y=kappa))+geom_boxplot()+theme_minimal()
# ggplot(results, aes(x=var, y=alpha))+geom_boxplot()+theme_minimal()
# 
# 
# # By group (set of observers)
# ggplot(results, aes(x=group, y=kappa))+geom_boxplot()+theme_minimal()
# ggplot(results, aes(x=group, y=alpha))+geom_boxplot()+theme_minimal()





