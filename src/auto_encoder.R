library(keras)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(cloudml)
library(Metrics)
library(pracma)


#Mainly based on https://www.r-bloggers.com/deep-learning-in-r-2/

### Builds a auto encoder given a preprocessed data (from processObservationData.R) and a number of desired units
build_autoencoder <- function(data){
  split <- round(nrow(data)/2)

  df_train <- data %>% filter(row_number() <= split)
  df_test <- data %>% filter(row_number() > split)
  
  desc <- df_train %>% 
     dplyr::select(disengaged, looking, talking, technology, resources, external) %>% 
     get_desc()
  
  x_train <- df_train %>%
     dplyr::select(disengaged, looking, talking, technology, resources, external) %>%
     normalization_minmax(desc) %>%
     as.matrix()
  x_test <- df_test %>%
     dplyr::select(disengaged, looking, talking, technology, resources, external) %>%
     normalization_minmax(desc) %>%
     as.matrix()

  y_train <- df_train$HHMPredS
  y_test <-df_test$HHMPredS
  
  model <- keras_model_sequential()
  invisible(model <- model %>%
    layer_dense(units = 3, activation = "tanh", input_shape = ncol(x_train)) %>%
    layer_dense(units = ncol(x_train)))
  
  print(summary(model))
  
  model <- model %>% compile(loss = "mean_squared_error", optimizer = "adam")
  
  checkpoint <- callback_model_checkpoint(
    filepath = "model.hdf5", 
    save_best_only = TRUE, 
    period = 1,
    verbose = 1
  )
  
  early_stopping <- callback_early_stopping(patience = 5)
  
  model %>% fit(
    x = x_train, 
    y = x_train, 
    epochs = 100,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_test, x_test), 
    callbacks = list(checkpoint, early_stopping)
  )
  
  model
}


### Extracts predictions 
get_predictions <- function(data, model, type = ""){
  if(strcmp(type, "Rec")){
    students <- unique(data$global.id)
    last <- 0
    
    data$`1` <- NA
    data$`2` <- NA
    data$`3` <- NA
    data$`4` <- NA
    data$`5` <- NA
    data$`6` <- NA
    
    for(i in 1:length(students)){
      batch <- data %>% dplyr::filter(global.id == students[[i]]) %>%
                        dplyr::select(disengaged, looking, talking, technology, resources, external)
      rows <- nrow(batch)
      
      batch <- as.matrix(batch)
      batch <- replicate(1, batch, simplify = "array")
      batch <- aperm(batch, c(3,1,2))
      
      new_columns <- predict(model, batch)
      
      for(j in 1:rows){
        data$`1`[j + last] <- round(new_columns[1, j, 1])
        data$`2`[j + last] <- round(new_columns[1, j, 2])
        data$`3`[j + last] <- round(new_columns[1, j, 3])
        data$`4`[j + last] <- round(new_columns[1, j, 4])
        data$`5`[j + last] <- round(new_columns[1, j, 5])
        data$`6`[j + last] <- round(new_columns[1, j, 6])
      }
      
      last <- last + rows
    }
    
    data$RecAEError <- abs(data$disengaged-data$`1`) + abs(data$looking-data$`2`) + abs(data$talking-data$`3`) +
      abs(data$technology-data$`4`) + abs(data$resources-data$`5`) + abs(data$external-data$`6`)
  }
  else{
    eval_data <- as.matrix(data[,c("disengaged", "looking", "talking", "technology", "resources", "external")])
    
    pred_train <- predict(model, eval_data)
    
    pred_train <- round(pred_train)
    
    data <- cbind(data, pred_train)
    
    data$error <- abs(data$disengaged-data$`1`) + abs(data$looking-data$`2`) + abs(data$talking-data$`3`) +
      abs(data$technology-data$`4`) + abs(data$resources-data$`5`) + abs(data$external-data$`6`)
    
    names(data)[length(names(data))] <- paste(type, "AEError", sep = "")
  }
  
  data <- data[, c(1:(ncol(data)-7), ncol(data))]
  
  data
}


### Adapted the above using
### https://blog.keras.io/building-autoencoders-in-keras.html
### Builds a sparse auto encoder given a preprocessed data (from processObservationData.R) and a number of units

build_sparse_autoencoder <- function(data, units = 3){
  split <- round(nrow(data)/2)

  df_train <- data %>% filter(row_number() <= split)
  df_test <- data %>% filter(row_number() > split)
  
  desc <- df_train %>% 
    dplyr::select(disengaged, looking, talking, technology, resources, external) %>% 
    get_desc()
  
  x_train <- df_train %>%
    dplyr::select(disengaged, looking, talking, technology, resources, external) %>%
    normalization_minmax(desc) %>%
    as.matrix()
  x_test <- df_test %>%
    dplyr::select(disengaged, looking, talking, technology, resources, external) %>%
    normalization_minmax(desc) %>%
    as.matrix()

  y_train <- df_train$HHMPredS
  y_test <-df_test$HHMPredS
  
  model <- keras_model_sequential()
  invisible(model <- model %>%
              layer_dense(units = units, activation = "tanh", input_shape = ncol(x_train),
                          activity_regularizer=regularizer_l1(10e-5)) %>%
              layer_dense(units = ncol(x_train)))
  
  print(summary(model))
  
  model <- model %>% compile(loss = "mean_squared_error", optimizer = "adam")
  
  checkpoint <- callback_model_checkpoint(
    filepath = "model.hdf5", 
    save_best_only = TRUE, 
    period = 1,
    verbose = 1
  )
  
  early_stopping <- callback_early_stopping(patience = 5)
  
  model %>% fit(
    x = x_train, 
    y = x_train, 
    epochs = 100,
    verbose = 0,
    batch_size = 32,
    validation_data = list(x_test, x_test), 
    callbacks = list(checkpoint, early_stopping)
  )
  
  model
}


### Adapted above using
### https://cran.rstudio.com/web/packages/keras/vignettes/sequential_model.html and
### https://datascience.stackexchange.com/questions/26366/training-an-rnn-with-examples-of-different-lengths-in-keras
### Currently doesn't work!!!!

build_recurrent_autoencoder <- function(data){
  
  
  model <- keras_model_sequential()
  invisible(model <- model %>%
              layer_lstm(units = 3, return_sequences = T, input_shape = list(NULL, 6)) %>%
              #layer_lstm(3, return_sequences = T) %>%
              #layer_dropout(rate = 0.5) %>%
              time_distributed(layer_dense(units = 6, activation = "sigmoid"))
            )
                               
  
  
  model <- model %>% compile(loss = "categorical_crossentropy", optimizer = "adam")
  
  checkpoint <- callback_model_checkpoint(
    filepath = "model.hdf5", 
    save_best_only = TRUE, 
    period = 1,
    verbose = 1
  )
  
  early_stopping <- callback_early_stopping(patience = 5)
  
  students <- unique(data$global.id)
  
  for(i in 1:length(students)){
    batch <- data %>% dplyr::filter(global.id == students[[i]]) %>%
                      dplyr::select(disengaged, looking, talking, technology, resources, external)
    
    #batch %<>% normalization_minmax(get_desc(batch)) %>% as.matrix()
    batch <- as.matrix(batch)
    batch <- replicate(1, batch, simplify = "array")
    batch <- aperm(batch, c(3,1,2))
    
    model %>% fit(
      x = batch,
      y = batch,
      epochs = 50,
      verbose = 0,
      #batch_size = nrow(batch),
      callbacks = list(checkpoint, early_stopping)
    )
  }
  
  model
}



extract_hidden_layer <- function(data, model){
  desc <- data %>% 
    dplyr::select(disengaged, looking, talking, technology, resources, external) %>% 
    get_desc()
  
  data <- data %>%
    dplyr::select(disengaged, looking, talking, technology, resources, external) %>%
    normalization_minmax(desc) %>%
    as.matrix()
  
  layer <- model$layers[[1]]
  intermediate_layer_model <- keras_model(inputs = model$input,
                                          outputs = layer$output)
  
  intermediate_output <- predict(intermediate_layer_model, data)
  
  intermediate_output
}

extract_recurrent_hidden_layer <- function(data, model){
  data <- data %>%
    dplyr::select(disengaged, looking, talking, technology, resources, external) %>%
    as.matrix()
  
  data <- replicate(1, data, simplify = "array")
  data <- aperm(data, c(3,1,2))
  
  layer <- model$layers[[1]]
  intermediate_layer_model <- keras_model(inputs = model$input,
                                          outputs = layer$output)
  
  intermediate_output <- predict(intermediate_layer_model, data)
  
  intermediate_output
}


insert_ae_units <- function(data, model, name = ""){
  if(strcmp(name, "Rec")){
    data$RecAEdim1 <- NA
    data$RecAEdim2 <- NA
    data$RecAEdim3 <- NA
    
    students <- unique(data$global.id)
    last <- 0
    
    for(i in 1:length(students)){
      batch <- data %>% dplyr::filter(global.id == students[[i]])
      
      new_columns <- extract_recurrent_hidden_layer(batch, model)

      for(j in 1:nrow(batch)){
        data$RecAEdim1[j + last] <- new_columns[1, j, 1]
        data$RecAEdim2[j + last] <- new_columns[1, j, 2]
        data$RecAEdim3[j + last] <- new_columns[1, j, 3]
      }
      
      last <- last + nrow(batch)
    }
  }
  else{
    new_columns <- extract_hidden_layer(data, model)
    new_columns <- data.frame(new_columns)
    
    
    for(i in 1:ncol(new_columns)){
      names(new_columns)[i] <-  paste(c(name, "AEdim", i), collapse = "")
    }
  
    data <- data.frame(data, new_columns)
  }
  
  data
}



## Just for internal use

get_desc <- function(x) {
  map(x, ~list(
    min = min(.x),
    max = max(.x),
    mean = mean(.x),
    sd = sd(.x)
  ))
}

normalization_minmax <- function(x, desc) {
  map2_dfc(x, desc, ~(.x - .y$min)/(.y$max - .y$min))
}

create_uniform_timeseries <- function(data){
  students <- unique(data$global.id)
  max_length <- 0
  
  for(i in 1:length(students)){
    this_length <- nrow(dplyr::filter(data, global.id == students[[i]]))
    
    if(this_length > max_length){max_length <- this_length}
  }
  
  new_data_frame <- dplyr::filter(data, disengaged == -1)
  
  for(i in 1:length(students)){
    filtered_data <- dplyr::filter(data, global.id == students[[i]])
    
    if(nrow(filtered_data) < max_length){
      n <- max_length - nrow(filtered_data)
      for(j in 1:n){
        filtered_data <- rbind(filtered_data, filtered_data[nrow(filtered_data), ])
      }
    }
    
    new_data_frame <- rbind(new_data_frame, filtered_data)
  }
  
  new_data_frame
}

#get_student_frequencies()
