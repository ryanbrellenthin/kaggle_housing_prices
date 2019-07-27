##### Setup
# Import packages
library(caret)
library(tidyverse)

# Source helper functions
source('helpers.R')

# Read in data
train <- readr::read_csv('data/train.csv')
test  <- readr::read_csv('data/test.csv')

# Separate IDs and dep_var from indpendent variables
train_vars <- train  %>% dplyr::select(-Id, -SalePrice)
train_dep_var  <- train  %>% dplyr::select(SalePrice)



##### Imputation
# Prepare training data
new_train_factor  <- clean_factor_variables(train_vars, frequency_threshold = 0.01)
new_train_numeric <- clean_numeric_variables(train_vars, impute = 'mean')
new_train         <- create_new_training_data(new_train_factor, new_train_numeric)

# Transform test data to match training data
new_test_factor   <- munge_factor_data(test, new_train_factor)
new_test_numeric  <- munge_numeric_data(test, new_train_numeric)
new_test          <- combine_new_test_data(new_test_factor, new_test_numeric, new_train)


##### Preprocessing
# Preprocess data set
train_pre <- caret::preProcess(new_train$data, method = c('center', 'scale', 'nzv'))

# Apply preprocessing to train and test data
train_transformed <- predict(train_pre, new_train$data)
train_set         <- dplyr::bind_cols(train_transformed, train_dep_var)
test_set          <- predict(train_pre, new_test$data)

# Set hyperparameters for model
fit_control <- trainControl(method = "repeatedcv",
                            number = 10,
                            verboseIter = TRUE)

gbm_grid2 <- expand.grid(interaction.depth = c(1, 5, 9),
                         n.trees = 1:30 * 50,
                         shrinkage = 0.1,
                         n.minobsinnode = 20)

##### Model training
# Train model
set.seed(321)
gbm_fit2 <- train(SalePrice ~ .,
                  data = train_set,
                  method = "gbm", 
                  trControl = fit_control,
                  verbose = FALSE,
                  tuneGrid = gbm_grid2)

##### Predictions
# Generate predictions
predictions <- predict(gbm_fit2, newdata = test_set)

# Join IDs and predictions
submission <- dplyr::tibble(Id = test$Id, SalePrice = predictions)

##### Output
# Create output file
readr::write_csv(submission, 'output/submission_gbm2.csv')
