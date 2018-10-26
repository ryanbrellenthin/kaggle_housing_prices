library(tidyverse)
library(glmnet)

train <- read.csv('kaggle_data/train.csv')
test  <- read.csv('kaggle_data/test.csv')

# Clean numeric variables
numeric_medians <- train %>%
  dplyr::select_if(is.numeric) %>%
  dplyr::summarise_all(funs(median), na.rm = TRUE)

test_numerics <- test %>%
  dplyr::select_if(is.numeric) %>%
  tidyr::replace_na(numeric_medians)

train_numerics <- train %>%
  dplyr::select_if(is.numeric) %>%
  tidyr::replace_na(numeric_medians)

# Clean factor variables
train_factors <- train %>%
  dplyr::select_if(is.factor) %>%
  dplyr::mutate_all(funs(forcats::fct_explicit_na), na_level = 'Missing') %>%
  dplyr::mutate_all(funs(as.character))

test_factors <- test %>%
  dplyr::select_if(is.factor) %>%
  dplyr::mutate_all(funs(forcats::fct_explicit_na), na_level = 'Missing') %>%
  dplyr::mutate_all(funs(as.character))

# Recombine numeric and factor variables
train_predictors <- dplyr::bind_cols(train_numerics, train_factors) %>%
  dplyr::select(-SalePrice)

test_predictors <- dplyr::bind_cols(test_numerics, test_factors)

all_predictors <- dplyr::bind_rows(train_predictors, test_predictors) %>%
  model.matrix( ~ .-1, .)

train_predictors_final <- all_predictors[1:nrow(train), ]
test_predictors_final  <- all_predictors[-(1:nrow(train)), ]

train_response <- dplyr::bind_cols(train_numerics, train_factors) %>%
  dplyr::select(SalePrice) %>%
  as.matrix()

# Build model using lasso
glm_cv_model <- cv.glmnet(train_predictors_final, train_response, alpha = 1)

# Predict sale prices
train_predictions <- predict(glm_cv_model, train_predictors_final, s = glm_cv_model$lambda.min)
test_predictions <- predict(glm_cv_model, test_predictors_final, s = glm_cv_model$lambda.min)

# Attach predictions to original data frames
train_final <- dplyr::bind_cols(train_predictors, as.data.frame(train_response), predictions = train_predictions)
test_final  <- dplyr::bind_cols(test_predictors, predictions = test_predictions)

# Create submission data frame
submission <- test_final %>%
  dplyr::select(Id, SalePrice = predictions)

# Write to CSV file
write.csv(submission, 'housing_prices_submission_lasso.csv', row.names = FALSE)
