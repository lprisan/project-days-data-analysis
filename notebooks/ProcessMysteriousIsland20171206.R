library(gsheet)
library(ggplot2)

source("../src/processObservationData.R")

url_obs="https://docs.google.com/spreadsheets/d/1FSqm57ygxaIFa5V5oUCx7ht21ayuxMm7MO2IeczZ7ww/edit#gid=1838199304"

rawobs <- as.data.frame(gsheet2tbl(url_obs))
obs <- processObservationData(rawobs, 
                               date = as.POSIXct(strptime("06-12-2017", "%d-%m-%Y")),
                               observercol = F,
                               # activitycol = T, # Not sure why there are much less rows with this
                               namecols=c("timestamp","group","activity",
                                          "StudentA","StudentB","StudentC",
                                          "StudentD","StudentE","comment"))
obs$timestamp <- as.POSIXct(strptime(obs$timestamp, "%d/%m/%Y %H:%M:%S"))
obs <- obs[ order(obs$timestamp), ]
