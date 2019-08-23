source('common.R')
library(ranger)
args = commandArgs(trailingOnly=TRUE)

if (length(args) == 1 && file.exists(args[1])) {
  test_data = load_data(args[1],'colstrained.rds')
  print("Data loaded successfully")
  tree_model <- readRDS(paste0('models/randomforest/forest-c166-ranger-meanimpute-corr.rds'))
  print("Model loaded successfully")
  
  test_data <- clean_data(test_data)
  
  #print(colnames(test_data))
  predictions <- predict(tree_model, data = test_data)
  
  accuracy <- sum(abs(predictions$predictions - test_data$y) < 3)/length(test_data$y)
  MSE_error <-sqrt((sum( (predictions$predictions - test_data$y)^2 ))/length(test_data$y))
  
  print(cat(paste0("Accuracy:", accuracy, "\nMSE:", MSE_error)))
  
} else if (length(args) != 1) {
  print("Usage run.R <path_to_test_data>")
} else if(!file.exists(args[1])) {
  print(paste("No file", args[1],"was found"))
}




