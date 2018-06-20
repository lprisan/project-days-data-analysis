library(keras)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)
library(cloudml)
library(Metrics)


#Mainly based on https://www.r-bloggers.com/deep-learning-in-r-2/


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
  
  # FLAGS <- flags(
  #   flag_string("normalization", "minmax", "One of minmax, zscore"),
  #   flag_string("activation", "relu", "One of relu, selu, tanh, sigmoid"),
  #   flag_numeric("learning_rate", 0.001, "Optimizer Learning Rate"),
  #   flag_integer("hidden_size", 3, "The hidden layer size")
  # )
  # 
  # model %>% compile(
  #   optimizer = optimizer_adam(lr = FLAGS$learning_rate), 
  #   loss = "mean_squared_error"
  # )
  # 
  # 
  
  #model <- load_model_hdf5("runs/cloudml_2018_01_23_221244595-03/model.hdf5", compile = FALSE)
  
  # pred_train <- predict(model, x_train)
  # mse_train <- apply((x_train - pred_train)^2, 1, sum)
  # 
  # pred_test <- predict(model, x_test)
  # mse_test <- apply((x_test - pred_test)^2, 1, sum)
  # 
  # print(auc(y_train, mse_train))
  # print(auc(y_test, mse_test))
  
  model
}


get_predictions <- function(data, model){
  eval_data <- as.matrix(data[,c("disengaged", "looking", "talking", "technology", "resources", "external")])
  
  pred_train <- predict(model, eval_data)
  
  pred_train <- round(pred_train)
  
  data <- cbind(data, pred_train)
  
  data$AEerror <- abs(data$disengaged-data$`1`) + abs(data$looking-data$`2`) + abs(data$talking-data$`3`) +
                    abs(data$technology-data$`4`) + abs(data$resources-data$`5`) + abs(data$external-data$`6`)
  
  data
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


insert_ae_units <- function(data, model){
  new_columns <- extract_hidden_layer(data, model)
  new_columns <- data.frame(new_columns)
  
  for(i in 1:ncol(new_columns)){
    names(new_columns)[i] <-  paste(c("AEdim", i), collapse = "")
  }
  
  data <- data.frame(data, new_columns)
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
