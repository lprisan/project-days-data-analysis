library(jsonlite)
library(plyr)

# Reads a raw text file with the data from a multimodal tracker, 
# and spits a list with dataframes with the different timestamped 
# data streams
preprocessMultimodalTracker <- function(rawdatafilename){

  #For tests only
  #rawdatafilename <- "./multimodal-tracker-1507051956530.txt"
  #rawdatafilename <- "./multimodal-tracker-1507545914788.txt"
  #TODO: extract the device ID from the filename, and add it as a column
  
  # Read the file  
  lines <- paste(readLines(rawdatafilename),collapse="")
  # Retouch it a bit so that it is proper json
  lines <- gsub("[]", "", lines, fixed=T)
  lines <- gsub("][", ",", lines, fixed=T)
  # We parse the json
  json_data <- fromJSON(lines) #This gets us a dataframe of dataframes
  
  # Get the accelerometer data separately
  acceleration <- NULL
  if(!is.null(json_data$acceleration)){
    acceleration <- json_data$acceleration[complete.cases(json_data$acceleration),]
    acceleration <- unique(acceleration[,1:4]) # Remove duplicate values, just in case
    acceleration <- acceleration[order(acceleration$timestamp),]
  }else{
    warning("No acceleration data!")
  }
      
  # Get the beacons data separately
  beacons <- NULL
  if(!is.null(json_data$beacons)){
    beacons <- data.frame()
    if(!is.null(json_data$beacons) && length(json_data$beacons)>0){
      for(beacon in json_data$beacons){
        #print(summary(beacon))
        if(nrow(beacons)==0){
          beacons <- beacon[complete.cases(beacon),] 
        }else{
          beacons <- rbind(beacons,beacon[complete.cases(beacon),])
        }
      }
    }
    names(beacons)[8] <- "timestamp"
    beacons <- unique(beacons[,1:8]) # Remove duplicate rows (beacon values apparently get repeated several times)  
    beacons <- beacons[order(beacons$timestamp),]
  }else{
    warning("No beacons data!")
  }  

  geolocation <- NULL
  # Get the geolocation data
  if(!is.null(json_data$geolocation)){
    geolocation <- json_data$geolocation[complete.cases(json_data$geolocation[,c("latitude","longitude")]),]
    geolocation <- unique(geolocation[,1:8]) # Remove duplicate values, just in case
    geolocation <- geolocation[order(geolocation$timestamp),]
  }else{
    warning("No geolocation data!")
  }
  
    
  # Do a merged dataframe via the timestamp values
  # overall <- data.frame()
  # if(!is.null(acceleration)){ # If we don't have accelerometer data, something went REALLY wrong
  #   overall <- acceleration
  # }
  # if(!is.null(beacons)){
  #   overall <- merge(acceleration,beacons,all = T)
  # }
  # if(!is.null(geolocation)){
  #   overall <- merge(overall,geolocation,all = T)
  # }
  # overall <- overall[order(overall$timestamp),]
  
  # return a list with the whole and separate dataframes
  data <- list(acceleration = acceleration, beacons = beacons, geolocation = geolocation)#, overall = overall)
  data
}