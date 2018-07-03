library(tidyverse)
library(reshape2)
library(lubridate)
library(purrrlyr)
library(ggrepel)
library(dplyr)

## To install the next package: devtools::install_github("twitter/AnomalyDetection")
## Devtools must be installed prior to this but is not required to run this script
library(AnomalyDetection)
library(IsolationForest)


### This script is mainly based on https://www.r-bloggers.com/anomaly-detection-for-business-metrics-with-r/


### Returns a dataframe indication how normal/anomal a students observer values are
### Students are flaged if their anomaly score is above the threshold (in relation to all other students)
isolation_forest_per_student <- function(data, threshold = 0.95, plot = T, normalise = T){
  if(normalise){
    data <- normalise_per_day(data)
  }
  
  data <- data %>%
    dplyr::group_by(global.id) %>%
    dplyr::summarize(disengaged = mean(disengaged, na.rm = TRUE), looking = mean(looking, na.rm = TRUE),
                     talking = mean(talking, na.rm = TRUE), technology = mean(technology, na.rm = TRUE),
                     resources = mean(resources, na.rm = TRUE), external = mean(external, na.rm = TRUE))
  
  df_ses_if <- data %>%
    # select the metrics
    dplyr::select(global.id, disengaged, looking, talking, technology, resources, external
           ) #%>%
    # casting
    #dcast(., timestamp ~ disengaged + looking + talking + technology + resources + external, value.var = 'sessions') %>%
    # remove total values
    #select(-total)
  df_ses_if[is.na(df_ses_if)] <- 0
  
  # creating trees
  if_trees <- IsolationTrees(df_ses_if[, -1])
  
  # evaluating anomaly score
  if_anom_score <- AnomalyScore(df_ses_if[, -1], if_trees)
  
  # adding anomaly score
  df_ses_if$anom_score <- round(if_anom_score$outF, 4)
  
  df_ses_if <- df_ses_if %>%
    mutate(is_anomaly = ifelse(ecdf(anom_score)(anom_score) >= threshold, TRUE, FALSE))
  
  if(plot){
    # visualization
    df_if_plot <- df_ses_if %>% dplyr::select(-anom_score, -is_anomaly) %>% melt(., id.vars = 'global.id')
    
    df_if_plot <- full_join(df_if_plot, df_if_plot, by = 'global.id') %>%
      left_join(., df_ses_if %>% dplyr::select(global.id, anom_score, is_anomaly), by = 'global.id')
  
    #color palette
    cols <- c("#4ab04a", "#eec73a", "#ffd73e", "#f05336", "#ce472e")
    mv <- max(df_if_plot$anom_score)

  
    gp <- ggplot(df_if_plot, aes(x = value.x, y = value.y, color = anom_score)) +
      theme_minimal() +
      facet_grid(variable.x ~ variable.y) +

      scale_color_gradientn(colors = cols, limits = c(min(df_if_plot$anom_score), max(df_if_plot$anom_score)),
                          breaks = c(0, mv),
                          labels = c("0", mv),
                          guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +

      geom_point(aes(color = anom_score), size = 2, alpha = 0.4) +

      theme(legend.position = 'bottom',
            legend.direction = 'horizontal',
            panel.grid.major = element_blank())
  
      plot(gp)
  }
   
   df_ses_if
}


### Returns a dataframe indication how normal/anomal a groups observer values are
### Groups are flaged if their anomaly score is above the threshold (in relation to all other groups)
isolation_forest_per_group <- function(data, threshold = 0.95, plot = T, normalise = T){
  if(normalise){
    data <- normalise_per_day(data)
  }
  
  data$global.group <- paste(data$date, data$group)
  
  data <- data %>%
    dplyr::group_by(global.group) %>%
    dplyr::summarize(disengaged = mean(disengaged, na.rm = TRUE), looking = mean(looking, na.rm = TRUE),
                     talking = mean(talking, na.rm = TRUE), technology = mean(technology, na.rm = TRUE),
                     resources = mean(resources, na.rm = TRUE), external = mean(external, na.rm = TRUE))
  
  df_ses_if <- data %>%
    # select the metrics
    dplyr::select(global.group, disengaged, looking, talking, technology, resources, external
    ) #%>%
  # casting
  #dcast(., timestamp ~ disengaged + looking + talking + technology + resources + external, value.var = 'sessions') %>%
  # remove total values
  #select(-total)
  df_ses_if[is.na(df_ses_if)] <- 0
  
  # creating trees
  if_trees <- IsolationTrees(df_ses_if[, -1])
  
  # evaluating anomaly score
  if_anom_score <- AnomalyScore(df_ses_if[, -1], if_trees)
  
  # adding anomaly score
  df_ses_if$anom_score <- round(if_anom_score$outF, 4)
  
  df_ses_if <- df_ses_if %>%
    mutate(is_anomaly = ifelse(ecdf(anom_score)(anom_score) >= threshold, TRUE, FALSE))
  
  if(plot){
    # visualization
    df_if_plot <- df_ses_if %>% dplyr::select(-anom_score, -is_anomaly) %>% melt(., id.vars = 'global.group')
  
    df_if_plot <- full_join(df_if_plot, df_if_plot, by = 'global.group') %>%
      left_join(., df_ses_if %>% dplyr::select(global.group, anom_score, is_anomaly), by = 'global.group')
  
    #color palette
    cols <- c("#4ab04a", "#eec73a", "#ffd73e", "#f05336", "#ce472e")
    mv <- max(df_if_plot$anom_score)
  
  
    gp <- ggplot(df_if_plot, aes(x = value.x, y = value.y, color = anom_score)) +
      theme_minimal() +
      facet_grid(variable.x ~ variable.y) +
    
      scale_color_gradientn(colors = cols, limits = c(min(df_if_plot$anom_score), max(df_if_plot$anom_score)),
                          breaks = c(0, mv),
                          labels = c("0", mv),
                          guide = guide_colourbar(ticks = T, nbin = 50, barheight = .5, label = T, barwidth = 10)) +
    
      geom_point(aes(color = anom_score), size = 2, alpha = 0.4) +
    
      theme(legend.position = 'bottom',
            legend.direction = 'horizontal',
            panel.grid.major = element_blank())
  
      plot(gp)
  }
  
  df_ses_if
}


#### Curently doesn't work!!!!
timeseries_anomaly <- function(data, column){

  df_ts <- data %>%
    mutate(timestamp = as.POSIXct(timestamp, format = "%d/%m/%Y %H:%M:%S"))
  
  #df_ts_ses <- df_ts %>%
  #  dcast(., date ~ channel, value.var = 'sessions')
  df_ts <- df_ts %>%
    dcast(., timestamp ~ disengaged, value.var = 'student')
  df_ts[is.na(df_ts)] <- 0
  
  # example with Direct channel
  AnomalyDetectionTs(df_ts[, c(1,3)],
                     max_anoms = 0.05, direction = 'both', e_value = TRUE, plot = TRUE) # 5% of anomalies
  AnomalyDetectionTs(df_ts[, c(1,3)],
                     max_anoms = 0.1, direction = 'both', e_value = TRUE, plot = TRUE) # 10% of anomalies
}



normalise_per_day <- function(data){
  days <- unique(data$date)
  
  ret <- dplyr::filter(data, disengaged == -1)
  
  for(i in 1:length(days)){
    day_data <- dplyr::filter(data, date == days[[i]])
    
    day_data$disengaged <- day_data$disengaged - mean(day_data$disengaged)
    day_data$disengaged <- day_data$disengaged/sd(day_data$disengaged)
    
    day_data$looking <- day_data$looking - mean(day_data$looking)
    day_data$looking <- day_data$looking/sd(day_data$looking)
    
    day_data$talking <- day_data$talking - mean(day_data$talking)
    day_data$talking <- day_data$talking/sd(day_data$talking)
    
    day_data$technology <- day_data$technology - mean(day_data$technology)
    day_data$technology <- day_data$technology/sd(day_data$technology)
    
    day_data$resources <- day_data$resources - mean(day_data$resources)
    day_data$resources <- day_data$resources/sd(day_data$resources)
    
    day_data$external <- day_data$external - mean(day_data$external)
    day_data$external <- day_data$external/sd(day_data$external)
    
    ret <- rbind(ret, day_data)
  }
  
  ret
}



