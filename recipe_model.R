# Leveraging recipe and caret to create multiple models and cross validate them 
library(recipes)

# Using tree regression 
source("common.R")

set.seed(11052019)
#Loading all data
dataset <- split_dataset(load_data())
training_data <- dataset$training
testing_data <- dataset$testing


# This is indication that this data might be very unbalance not a great way to get responses
# maybe by removing lot of features with near zero variance other models can have a more significant result
summary(training_data)xx