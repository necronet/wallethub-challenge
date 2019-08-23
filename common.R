# Script for function general accorss diferent model strategies
library(rsample)
library(recipes)

load_data <- function(data_path = 'data/dataset_00_with_header.csv', col_name_path = 'data/colnames.rds') {
  # Loading only certain columns since previous analisis show too much sparsity in these columns
  valid_columns <- readRDS(col_name_path)
  read.csv(data_path)[, valid_columns]
}

split_dataset <- function(data, training_prop = 0.70) {
  initial_split <- initial_split(data, training_prop)
  training_data <- training(initial_split)
  testing_data <- testing(initial_split)
  
  return(list(training=training_data, testing=testing_data))
}

clean_data <- function(data) {
  
  categorical_columns <- data %>% gather(var, value) %>% distinct %>% count(var) %>% filter(n <= 2) %>% select(var)
  pre_categorized_data <- data %>% mutate_at(categorical_columns$var, as.factor)
  
  recipe_object <- recipe( y ~ ., data = pre_categorized_data) 
  recipe_object <- recipe_object %>% step_modeimpute(all_nominal()) %>% 
                      step_meanimpute(all_numeric()) %>% 
                      step_YeoJohnson(all_numeric(), -all_outcomes()) 
  recipe_object <- prep(recipe_object, data = pre_categorized_data, verbose = TRUE)
  cleaned_training_data <- bake(recipe_object, new_data = pre_categorized_data)
}