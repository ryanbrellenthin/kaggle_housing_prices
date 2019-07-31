########## IMPORT (Get it in...) ##########

# Import packages
library(tidyverse)
library(tidymodels)

# Read in data
train <- readr::read_csv('data/train.csv')
test  <- readr::read_csv('data/test.csv')

# Generate cross-validation splits
train_splits <- rsample::vfold_cv(train, v = 10, repeats = 1, strata = NULL)

########## DATA (Get it ready...) ##########

# Munge data
munge_steps <- recipes::recipe(SalePrice ~ ., data = train) %>% 
  update_role(Id, new_role = 'id_column') %>% 
  step_medianimpute(all_numeric(), -Id, -SalePrice) %>%
  step_modeimpute(all_nominal()) %>%
  step_other(all_nominal(), threshold = 0.1, other = 'other') %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric(), -Id, -SalePrice) %>%
  step_dummy(all_nominal()) %>%
  check_missing(all_predictors())

munge_instructions <- munge_steps %>% 
  prep(retain = TRUE, verbose = TRUE)


# Preprocess model
iterate_linear_models <- function(df, penalty = 10, mixture = 0.1) {
  model <- parsnip::linear_reg(mode = 'regression', penalty = penalty, mixture = mixture) %>% 
    set_engine('glmnet') %>% 
    fit(SalePrice ~ ., data = df)
  model
}

iterate_xgb_models <- function(df, mtry = 1, trees = 15, min_n = 1, tree_depth = 6, learn_rate = 0.3, loss_reduction = 0, sample_size = 0.6) {
  model <- parsnip::boost_tree(mode = 'regression',
                               mtry = mtry,
                               trees = trees,
                               min_n = min_n,
                               tree_depth = tree_depth,
                               learn_rate = learn_rate,
                               loss_reduction = loss_reduction,
                               sample_size = sample_size) %>% 
    set_engine('xgboost') %>% 
    fit(SalePrice ~ ., data = df)
  model
}


model_building <- function(input_data, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction , sample_size) {
  result <- input_data %>% 
    dplyr::mutate(model = purrr::pmap(.l = list(train_transformed, mtry, trees, min_n, tree_depth, learn_rate, loss_reduction, sample_size),
                                      .f = ~ iterate_models(df = ..1, trees = ..2, tree_depth = ..3)))
  result
}

model_building_linear <- function(input_data, penalty, mixture) {
  result <- input_data %>% 
    dplyr::mutate(model = purrr::pmap(.l = list(train_transformed, penalty, mixture),
                                      .f = ~ iterate_linear_models(df = ..1, penalty = ..2, mixture = ..3)))
  result
}

extract_rmse <- function(prediction_df) {
  result <- prediction_df %>% 
    yardstick::metrics(truth = SalePrice, estimate = .pred) %>% 
    dplyr::filter(.metric == 'rmse') %>% 
    dplyr::summarise(rmse = mean(.estimate))
  result
}

# Grid search
define_grid_params_xgb <- function(mtry = 1, trees = 15, min_n = 1, tree_depth = 6, learn_rate = 0.3, loss_reduction = 0, sample_size = 0.6) {
  result <- expand.grid(mtry = mtry, 
                        trees = trees, 
                        min_n = min_n, 
                        tree_depth = tree_depth, 
                        learn_rate = learn_rate, 
                        loss_reduction = loss_reduction, 
                        sample_size = sample_size) %>% 
    as_tibble()
  result
}

define_grid_params_linear <- function(penalty, mixture) {
  result <- expand.grid(penalty = penalty,
                        mixture = mixture) %>% 
    as_tibble()
  result
}

grid_params <- define_grid_params_linear(penalty = 1:10, mixture = 1:10 / 10)

# Preprocess splits
set.seed(321)
prepared_splits <- train_splits %>% 
  dplyr::mutate(train_data = purrr::map(.x = splits, .f = rsample::analysis),
                train_transformed = purrr::map(.x = train_data, .f = ~ bake(object = munge_instructions, .x)),
                test_data = purrr::map(.x = splits, .f = rsample::assessment),
                test_transformed = purrr::map(.x = test_data, .f = ~ bake(object = munge_instructions, .x))) %>% 
  tidyr::crossing(grid_params) %>% 
  dplyr::select(fold = id, dplyr::one_of(names(grid_params)), everything())

# Build models
set.seed(321)
modeling <- prepared_splits %>% model_building_linear()

# Score predictions
set.seed(321)
holdout_transformed <- bake(munge_steps %>% prep(training = train), test)

predictions <- modeling %>% 
  dplyr::mutate(train_yhat = purrr::map2(.x = model, .y = train_transformed, .f = ~ dplyr::bind_cols(.y %>% dplyr::select(Id, SalePrice), predict(.x, new_data = .y))),
                test_yhat = purrr::map2(.x = model, .y = test_transformed, .f = ~ dplyr::bind_cols(.y %>% dplyr::select(Id, SalePrice), predict(.x, new_data = .y))),
                train_rmse = purrr::map(.x = train_yhat, .f = extract_rmse),
                test_rmse = purrr::map(.x = test_yhat, .f = extract_rmse),
                holdout_yhat = purrr::map(.x = model, .f = ~ predict(.x, new_data = holdout_transformed))) %>% 
  tidyr::unnest(train_rmse, test_rmse) %>% 
  rename(train_rmse = rmse, test_rmse = rmse1)

# Combine folds to find average RMSE
aggregated_models <- predictions %>% 
  dplyr::group_by_at(names(grid_params)) %>% 
  dplyr::summarize(train_rmse = mean(train_rmse),
                   test_rmse = mean(test_rmse)) %>% 
  dplyr::ungroup()



########## EXPORT (Get it out...) ##########
best_model <- aggregated_models %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(test_rmse == min(test_rmse))

best_predictions <- predictions %>% 
  dplyr::filter(penalty == best_model$penalty,
                mixture == best_model$mixture) %>% 
  dplyr::select(fold, holdout_yhat) %>% 
  tidyr::unnest() %>% 
  dplyr::mutate(Id = rep(holdout_transformed$Id, 10)) %>% 
  dplyr::group_by(Id) %>% 
  dplyr::summarize(SalePrice = mean(.pred))

readr::write_csv(best_predictions, 'output/submission_tidymodels_glmnet.csv')
