library(keras)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ggridges)


build_autoencoder <- function(data){
  df_train <- data %>% filter(row_number() <= 2000)
  df_test <- data %>% filter(row_number() > 2000)
  
  desc <- df_train %>% 
     select(disengaged, looking, talking, technology, resources, external) %>% 
     get_desc()
  
  x_train <- df_train %>%
     select(disengaged, looking, talking, technology, resources, external) %>%
     normalization_minmax(desc) %>%
     as.matrix()
  x_test <- df_test %>%
     select(disengaged, looking, talking, technology, resources, external) %>%
     normalization_minmax(desc) %>%
     as.matrix()
  
  model <- keras_model_sequential()
  model %>%
    layer_dense(units = 3, activation = "tanh", input_shape = ncol(x_train)) %>%
    layer_dense(units = ncol(x_train))
  
  print(summary(model))
  
  model %>% compile(loss = "mean_squared_error", optimizer = "adam")
  
  checkpoint <- callback_model_checkpoint(
    filepath = "model.hdf5", 
    save_best_only = TRUE, 
    period = 1,
    verbose = 1
  )
  
  early_stopping <- callback_early_stopping(patience = 5)
  
  model %>% fit(
    x = x_train[y_train == 0,], 
    y = x_train[y_train == 0,], 
    epochs = 100, 
    batch_size = 32,
    validation_data = list(x_test[y_test == 0,], x_test[y_test == 0,]), 
    callbacks = list(checkpoint, early_stopping)
  )
  
  model
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
