library(keras)
source("common.R")
library(dplyr)
library(recipes)
library(ggplot2)

set.seed(11052019)
#Loading all data
dataset <- split_dataset(load_data())
training_data <- dataset$training
testing_data <- dataset$testing

categorical_columns <- training_data %>% gather(var, value) %>% distinct %>% count(var) %>% filter(n <= 2) %>% select(var)
pre_cateroized_data <- training_data %>% mutate_at(categorical_columns$var, as.factor)

recipe_object <- recipe( y ~ ., data = pre_cateroized_data) 
recipe_object <- recipe_object %>%  step_nzv(all_predictors()) %>% 
  step_modeimpute(all_nominal()) %>% 
  step_meanimpute(all_numeric()) %>% 
  step_YeoJohnson(all_numeric(), -all_outcomes()) 
recipe_object <- prep(recipe_object, data = pre_cateroized_data, verbose = TRUE)
cleaned_training_data <-bake(recipe_object, new_data = pre_cateroized_data)

#write.csv(cleaned_training_data,"data/imputed-trained-data.csv")


cleaned_training_data$y <- NULL
dim(cleaned_training_data) <- NULL

model <- keras_model_sequential() %>%
  layer_dense(units = 256, input_shape = ncol(cleaned_training_data)) %>%
  layer_dense(units = 128) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = "mse",
  optimizer = optimizer_rmsprop(),
  metrics = list("mean_squared_error")
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20)

#model %>% summary()

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    


# Fit the model and store training stats
model_history <- model %>% fit(
  as.matrix(cleaned_training_data),
  training_data$y,
  epochs = 1000,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(early_stop, print_dot_callback)
)

plot(model_history, metrics = "mean_squared_error", smooth = TRUE) + coord_cartesian(ylim = c(0, 20000000))


