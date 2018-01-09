library("FactoMineR")
library("factoextra")
library(ggplot2)
library(gsheet)
library(reshape)
library(reshape2)
library(tidyr)

# Get the data from google spreadsheets

obs_urls <- data.frame(date=c("11-10-2017","22-11-2017","18-10-2017","08-11-2017","06-12-2017","13-12-2017"),
                       url=as.character(c("https://docs.google.com/spreadsheets/d/11BPHcSlqwozx3ffOQhQ5i5O8TRVa8xKoaw2uiny8k5A/edit",
                                          "https://docs.google.com/spreadsheets/d/1kqSu52ZJo0y3cnhAe9fEsv9t7kUdRQ-2cKqSsziQk_M/edit",
                                          "https://docs.google.com/spreadsheets/d/18SL1SG3dJFt18HqJ-jsn1S2zbS0pt4BP2wfU9ILJZWs/edit",
                                          "https://docs.google.com/spreadsheets/d/1EoAfPj2hFcmlHo2ndfunLV4krkgJ2a69KlCi5NoYp0k/edit",
                                          "https://docs.google.com/spreadsheets/d/1FSqm57ygxaIFa5V5oUCx7ht21ayuxMm7MO2IeczZ7ww/edit",
                                          "https://docs.google.com/spreadsheets/d/13Od5UuY5LLh2E5EPVD17IP6dO3x_xTpxyVYg9BQmIdE/edit")),
                       project=as.factor(c("Linnaruum","Linnaruum","Meeled","Meeled","Isle","Isle")),
                       nrlevels=c(5,6,6,6,6,6),
                       activitycol=c(F,F,F,F,T,T),
                       stringsAsFactors = F)

obs_urls$date <- as.POSIXct(strptime(obs_urls$date, "%d-%m-%Y"))


processObservationData <- function(url, nrlevels=6, date, project, activitycol=F){
  
  obs_data <- read.csv(text=gsheet2text(url, format='csv'), stringsAsFactors=FALSE)
  
  if(!activitycol){
    names(obs_data)[1:2] <- c("timestamp", "group")
    names(obs_data)[3:(ncol(obs_data)-1)] <- paste("Student",toupper(letters[1:(ncol(obs_data)-3)]),sep="")
    names(obs_data)[ncol(obs_data)] <- "comment"
  }else{
    names(obs_data)[1:3] <- c("timestamp", "group", "activity")
    names(obs_data)[4:(ncol(obs_data)-1)] <- paste("Student",toupper(letters[1:(ncol(obs_data)-4)]),sep="")
    names(obs_data)[ncol(obs_data)] <- "comment"
  }
  #Clean up empty columns
  for(i in ncol(obs_data):1){
    if(sum(complete.cases(obs_data[i]))==0 || sum(obs_data[i]!="")==0 || sum(obs_data[i]!="")==1) obs_data <- obs_data[,-i]
  }
  
  # Cleanup invalid data, before 10am like "2017-10-11 09:44:49 GMT"
  #obs_data <- obs_data[obs_data$timestamp>as.POSIXct("2017-10-11 10:00:00 GMT", origin = "1970-01-01", tz = "GMT"),]
  
  # Student view of the observations
  if(!activitycol){
    student_obs <- melt(obs_data, id=1:2, measure=3:(ncol(obs_data)-1))
    names(student_obs)[[3]]<-"student"
  }else{
    student_obs <- melt(obs_data, id=1:3, measure=4:(ncol(obs_data)-1))
    names(student_obs)[[4]]<-"student"
  }
  student_obs$disengaged <- as.numeric(grepl(pattern = "disengaged", x = student_obs$value, fixed = TRUE))
  student_obs$looking <- as.numeric(grepl(pattern = "Looking", x = student_obs$value, fixed = TRUE))
  student_obs$talking <- as.numeric(grepl(pattern = "Talking", x = student_obs$value, fixed = TRUE))
  student_obs$technology <- as.numeric(grepl(pattern = "technology", x = student_obs$value, fixed = TRUE))
  student_obs$resources <- as.numeric(grepl(pattern = "resources", x = student_obs$value, fixed = TRUE))
  student_obs$external <- as.numeric(grepl(pattern = "external", x = student_obs$value, fixed = TRUE))
  student_obs$student.id <- paste(student_obs$group,student_obs$student)

  if(!activitycol){
    student_obs <- student_obs[,c(1:3,5:ncol(student_obs))]
  }else{
    student_obs <- student_obs[,c(1:4,6:ncol(student_obs))]
  }
  
  # summary(student_obs)
  
  student_obs$project <- project
  student_obs$date <- date

  student_obs$global.id <- paste(student_obs$project,student_obs$date,student_obs$student.id)
  
  student_obs  
}

# df <- data.frame()
# for(i in 1:nrow(obs_urls)){
#   tmp <- processObservationData(obs_urls[i,"url"],
#                                 obs_urls[i,"nrlevels"],
#                                 obs_urls[i,"date"],
#                                 obs_urls[i,"project"],
#                                 obs_urls[i,"activitycol"])
#   
#   if(sum(grepl(pattern = "activity", x = names(tmp), fixed=T))==0){
#     tmp$activity <- NA
#   }
#   
#   if(nrow(df)==0){
#     df <- tmp  
#   }
#   else{
#     df <- rbind(df,tmp)
#   }
# }
# save(df,file = "obsdata.Rdata")
#Start here when offline
df <- get(load("obsdata.Rdata"))
data <- df[,4:9]
for(i in 1:ncol(data)){
  data[,i] <- as.factor(data[,i])
}
res.mca <- MCA(data, graph = T)
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))
eig.val <- get_eigenvalue(res.mca)
# Too many points!
# fviz_mca_biplot(res.mca, 
#                 repel = TRUE, # Avoid text overlapping (slow if many point)
#                 ggtheme = theme_minimal())
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
fviz_mca_var(res.mca, 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())
# Color by cos2 values: quality on the factor map - not so important right now
# fviz_mca_var(res.mca, col.var = "cos2",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              repel = TRUE, # Avoid text overlapping
#              ggtheme = theme_minimal())
# fviz_mca_var(res.mca, col.var = "contrib",
#              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
#              repel = TRUE, # avoid text overlapping (slow)
#              ggtheme = theme_minimal()
# )
# Dimensions from the MCA
df$engagement <-res.mca$ind$coord[,1]
df$offtask <- res.mca$ind$coord[,2]

# Plotting engagement and offtask of selected students or groups
# d <- df %>% filter(global.id == unique(global.id)[[1]]) 
# qplot(1:23, engagement, data=d, geom="line")
# qplot(1:23, offtask, data=d, geom="line")
# 
# d <- df %>% filter(global.id == unique(global.id)[[50]]) 
# qplot(1:35, engagement, data=d, geom="line")
# qplot(1:35, offtask, data=d, geom="line")

bygroup <- aggregate(cbind(engagement, offtask) ~ timestamp+group+project+date, data=df, mean)
bygroup$global.id <- paste(bygroup$project,bygroup$date,bygroup$group)

# d <- bygroup %>% filter(global.id == unique(global.id)[[10]])
# ggplot(d, aes(1:41, engagement))+theme_minimal()+
#   geom_line(alpha=0.5)+geom_smooth(se=F, span=0.25)+geom_hline(yintercept = 0)
# ggplot(d, aes(1:41, offtask))+theme_minimal()+
#   geom_line(alpha=0.5)+geom_smooth(se=F, span=0.25)+geom_hline(yintercept = 0)

# inequality of engagement as signal of potential collab. problems?
library(ineq)
df$engagement2 <- df$engagement-min(df$engagement) # We make the values all positive
bygroup.ineq <- aggregate(engagement2 ~ timestamp+group+project+date, data=df, Gini)
bygroup.ineq$global.id <- paste(bygroup.ineq$project,
                                bygroup.ineq$date,
                                bygroup.ineq$group)

# d <- bygroup.ineq %>% filter(global.id == unique(global.id)[[16]])
# 
# ggplot(d, aes(1:35, engagement2))+theme_minimal()+
#   geom_line(alpha=0.5)+geom_smooth(se=F, span=0.25)+geom_hline(yintercept = 0)

# Various spot checks ###########################
# Distribution of dimension scores
ggplot(df, aes(x=engagement))+geom_density()+theme_minimal()+geom_vline(xintercept = mean(df$engagement))
ggplot(df, aes(x=offtask))+geom_density()+theme_minimal()+geom_vline(xintercept = mean(df$engagement))

# Plots of engagement per group, for different dates
df$time <- as.POSIXct(strptime(df$timestamp, "%d/%m/%Y %H:%M:%S"))
library(lubridate)
df$timeofday <- as.POSIXct(strptime(paste(hour(df$time),minute(df$time),second(df$time), sep=":"), "%H:%M:%S"))
ggplot(df, aes(x=timeofday, y=engagement, color=group))+
  geom_smooth(span=0.2, se=F)+facet_wrap(~date)+theme_minimal()
ggplot(df, aes(x=timeofday, y=engagement, color=group))+
  geom_smooth(method="lm",span=0.2, se=F)+facet_wrap(~date)+theme_minimal()
ggplot(df, aes(x=timeofday, y=engagement, color=group))+geom_point(alpha=0.3)+
  geom_smooth(span=0.2, se=F)+facet_wrap(~date)+theme_minimal()
ggplot(df, aes(x=group, y=engagement, fill=group))+geom_boxplot()+
  facet_wrap(~date)+theme_minimal()
ggplot(df, aes(x=engagement, y=offtask))+geom_jitter(width=0.2, height=0.2, alpha=0.1)+
  theme_minimal()

# MCA plots of different dates
for(d in unique(as.character(df$date))){
  data <- df[as.character(df$date)==d,]
  data <- data[,4:9]
  for(i in 1:ncol(data)){
    data[,i] <- as.factor(data[,i])
  }
  res.mca <- MCA(data, graph = T)
  print(fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45)))
  # fviz_mca_var(res.mca, choice = "mca.cor", 
  #              repel = TRUE, # Avoid text overlapping (slow)
  #              ggtheme = theme_minimal())
  # fviz_mca_var(res.mca, 
  #              repel = TRUE, # Avoid text overlapping (slow)
  #              ggtheme = theme_minimal())
  
}
# Conclusion: in some sessions the dim.1 looks like engagement, but not always! maybe we need a certain critical mass of observations??
