# Using tree regression 
source("common.R")
library(randomForest)
library(dplyr)
library(recipes)
library(caret)
library(ranger)
library(DescTools)
library(vip)

set.seed(11052019)
#Loading all data
dataset <- split_dataset(load_data())
training_data <- dataset$training
testing_data <- dataset$testing

glimpse(training_data)
categorical_columns <- training_data %>% gather(var, value) %>% distinct %>% count(var) %>% filter(n <= 2) %>% select(var)
pre_cateroized_data <- training_data %>% mutate_at(categorical_columns$var, as.factor)

recipe_object <- recipe( y ~ ., data = pre_cateroized_data) 
recipe_object <- recipe_object %>%  step_nzv(all_predictors()) %>% 
                    step_corr(all_numeric(), -all_outcomes(), threshold = .9) %>%
                    step_modeimpute(all_nominal()) %>% 
                    step_meanimpute(all_numeric()) %>% 
                    step_YeoJohnson(all_numeric(), -all_outcomes()) 
recipe_object <- prep(recipe_object, data = pre_cateroized_data, verbose = TRUE)
cleaned_training_data <-bake(recipe_object, new_data = pre_cateroized_data)

#training_data$x009 <- Winsorize(training_data$x009,  probs = c(0.02, 0.98))
#as.factor(training_data$x009)
#ggplot(training_data, aes(x=x009)) + geom_bar(fill="lightgreen", binwidth = 0.5) 

n_features <- length(setdiff(names(cleaned_training_data), "y"))

rf_t1 <- ranger(
  formula = y ~ ., 
  data = cleaned_training_data,
  mtry = floor(n_features / 3),
  respect.unordered.factors = "order",
  importance = "impurity",
  verbose = T,
  seed = 11052019
)

# na.omit won't work well because this dataset contains a lot of missing values
#tree_model <- randomForest::randomForest(y ~ ., data = training_data, importance = TRUE, do.trace = TRUE, na.action = na.omit)
#tree_model <- randomForest::randomForest(y ~ ., data = cleaned_training_data, importance = TRUE, do.trace = TRUE)

saveRDS(rf_t1, paste0('models/randomforest/forest-c',ncol(cleaned_training_data),'-ranger-meanimpute-corr.rds'))

#tree_model <- readRDS(paste0('models/randomforest/forest-c210-meanimpute.rds'))

plot(rf_t1)
summary(rf_t1)

vip::vip(rf_t1, num_features = 30, bar = FALSE) + ggtitle("Impurity features")

predictions <- predict(rf_t1, data = cleaned_training_data)

sum(abs(predictions$predictions - cleaned_training_data$y) < 3)/length(cleaned_training_data$y)
sqrt((sum( (predictions$predictions - cleaned_training_data$y)^2 ))/length(cleaned_training_data$y))
