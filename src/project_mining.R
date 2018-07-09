library(tidyverse)
library(igraph)
library(subprocess)
library(png)
library(gridBase)
library(humanFormat)
library(dplyr)
library(pracma)


### Based on https://www.kdnuggets.com/2017/11/process-mining-r-introduction.html


### preprocesses data. given a dataframe it returns a dataframe that can be used by the following functions.
### one has to select the columns that should be analysed as the obs.var input
preprocess_for_mining <- function(data, obs.var =
                                    c("disengaged", "looking", "talking", "technology", "resources", "external")){
  
  data <- data[, c("timestamp", "global.id", obs.var)]
  
  data$label <- data[, 3]
  
  if(length(obs.var) > 1){
    for(i in 4:(length(obs.var) + 2)){
      data$label <- paste(data$label, data[, i], sep = " - ")
    }
  }
  
  data$timestamp <- as.POSIXct(strptime(data$timestamp, format = "%d/%m/%Y %H:%M:%S"))
  
  data <- data[, c("global.id", "timestamp", "label")]
  
  data %<>% mutate(Row = row_number()) %>% ungroup
  
  data$Prev <- NA
  data$Next <- NA
  data$Duration <- NA
  
  last_student <- ""
  
  for(i in 1:nrow(data)){
    if(strcmp(last_student, data$global.id[i])){
      data$Prev[i] <- data$Row[i - 1]
    }
    
    last_student <- data$global.id[i]
  }
  
  last_student <- ""
  
  for(i in nrow(data):1){
    if(strcmp(last_student, data$global.id[i])){
      data$Next[i] <- data$Row[i + 1]
      data$Duration[i] <- data$timestamp[i + 1] - data$timestamp[i]
    }
    else{
      data$Duration[i] <- 5
    }
    
    last_student <- data$global.id[i]
  }
  
  data$Complete <- data$timestamp + data$Duration*60
  
  data
}


#Takes in preprocessed data and plots variants in the observer log
plot_variants <- function(data){
  data %>%
     arrange(timestamp) %>%
     group_by(global.id) %>%
     dplyr::summarize(Variant=paste(label, collapse='->', sep='')) %>%
     ggplot(aes(x=reorder(Variant, -table(Variant)[Variant]) )) +
     theme_minimal() +
     theme(axis.text.x=element_blank(),
           axis.ticks.x=element_blank()) +
     xlab('Variants') +
     geom_bar()
}


### creates a process mining model and returns an analysis as well as an optional plot.
### takes in a preprocessed dataframe
visualise_process_maps <- function(data, plot = FALSE){
  data$count <- 1
  
  activities.basic <- data %>% dplyr::select(global.id, Row, timestamp, Complete, count, act=label)
  
  edges.basic <- bind_rows(
    data %>% dplyr::select(global.id, a=Row, b=Next),
    data %>% dplyr::select(global.id, a=Prev, b=Row)) %>%
    dplyr::filter(!is.na(a), !is.na(b)) %>%
    distinct %>%
    left_join(data, by=c("a" = "Row"), copy=T, suffix=c("", ".prev")) %>%
    left_join(data, by=c("b" = "Row"), copy=T, suffix=c("", ".next")) %>%
    dplyr::select(global.id, count, a, b,
           a.act=label, b.act=label.next,
           #a.Duration=Duration, b.Duration=Duration.next,
           a.start=timestamp, b.start=timestamp.next,
           a.complete=Complete, b.complete=Complete.next)
  
  #activities.basic <- group_by(activities.basic, Duration)
  
  col.box.red <- colorRampPalette(c('#FEF0D9', '#B30000'))(20)
  col.arc.red <- colorRampPalette(c('#938D8D', '#B30000'))(20)
  linMap <- function(x, from, to) (x - min(x)) / max(x - min(x)) * (to - from) + from
  
  activities.counts <- activities.basic %>%
    group_by(act) %>% dplyr::summarize(metric=sum(count)) %>% ungroup %>%
    mutate(metric=ifelse(metric == 0, 'instant', metric), color=col.box.red[floor(linMap(metric, 1,20))])
  
  edges.counts <- edges.basic %>%
    group_by(a.act, b.act) %>% dplyr::summarize(metric=sum(count)) %>% ungroup %>%
    mutate(metric=ifelse(metric == 0, 'instant', metric), color=col.arc.red[floor(linMap(metric, 1, 20))],
           penwidth=floor(linMap(metric, 1, 5)))
    
  if(plot){
      gh <- graph_from_data_frame(edges.counts, vertices=activities.counts, directed=T)
      #gh <- label(edges.counts$metric)
    plot(gh)
  }

  edges.counts
}


