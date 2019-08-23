# This is an exploratory analysis on wallethub data science challenge
library(rsample)
library(visdat)
library(dplyr)
library(ggplot2)
library(corrplot)
library(glmnet)
library(vip)
source("common.R")

set.seed(11052019)
#Loading all data
dataset <- split_dataset(load_data())
training_data <- dataset$training
testing_data <- dataset$testing

# Checking missingness accross features
vis_miss(training_data[sample(500),], cluster = TRUE)

# omit na values
training_data_no_na <- na.omit(training_data)
# showing that most record contains na columns with it so they need to be treated per column based
vis_miss(training_data_no_na[sample(500),], cluster = TRUE)

training_data_any_na <- training_data %>% select_if(function(d) {!any(is.na(d))})

# TODO: need to remove also the columns that are basically same value do not provide anything special

vis_miss( training_data %>% select_if(function(d) {!any(is.na(d))}) %>% sample_n(500), cluster = TRUE )

# Looking at what exactly the response variable is
min(training_data$y)
max(training_data$y)
as.factor(training_data$y)
ggplot(training_data, aes(x=y)) + geom_density(fill="lightgreen") + scale_x_continuous(trans='log10')

ggplot(training_data) + geom_histogram(mapping = aes(x=y), binwidth= 0.01) + scale_x_continuous(trans='log10')
ggplot(training_data) + geom_histogram(mapping = aes(x=y), binwidth= 1)
ggplot(training_data) + geom_histogram(mapping = aes(x=y), binwidth= 10)
ggplot(training_data) + geom_histogram(mapping = aes(x=y), binwidth= 20)

# A naive way to predict based on linear regression this will fall due to the many features.
lm_model <- lm(y ~ ., data=training_data_any_na)
#lm_model <- lm(y ~ ., data=training_data_any_na)
summary(lm_model)

sqrt(crossprod(lm_model$residuals)/length(lm_model$residuals))

# Very large MAE 35! 
sum(abs(lm_model$residuals))/length(lm_model$residuals)

# Sample some features to see their correlation between each other
select_random_columns <- function(data ,m = 20){
  data %>% colnames %>% sample(m) 
}

# plotting correlation among a random subset of columns in training set
training_data %>% select(select_random_columns(training_data, 80)) %>% cor %>% corrplot
training_data_any_na  %>% cor %>% corrplot
# After graph a good chunk of features linear correlation 
# Regularization may work on keeping collinearity between features in check

# Design matrix X
X <- model.matrix(y ~ ., cleaned_training_data)[, -1]
# ncol(training_data_any_na) == ncol(X) + 1
lm_ridge <- glmnet( x = X, y = training_data_any_na$y, alpha = 0)
plot(lm_ridge, xvar = "lambda")

# There seem to be some features that get's to be shrinked as expected with ridge
# Let's cross validate this to find an improvement in error

lm_ridge <- cv.glmnet(
  x = X,
  y = cleaned_training_data$y,
  alpha = 0,
  type.measure='mae'
)

lm_lasso <- cv.glmnet(
  x = X,
  y = cleaned_training_data$y,
  alpha = 1,
  type.measure='mae'
)

plot(lm_ridge, main = "Ridge regression")
plot(lm_lasso, main = "Lasso regression")

# No improvement by using penalties, indicates that the underlying relationship may not be linear accross features
min(lm_ridge$cvm) 
min(lm_lasso$cvm) 

vip(lm_ridge, num_features = 25, bar = FALSE)
vip(lm_lasso, num_features = 25, bar = FALSE)

training_data_any_na$x244


sum(abs(predict.glmnet(lm_ridge$glmnet.fit, newx=X, s=lm_ridge$lambda.1se) - cleaned_training_data$y) < 3) /length(cleaned_training_data$y)

sum(abs( - cleaned_training_data$y) < 3)/length(cleaned_training_data$y)
