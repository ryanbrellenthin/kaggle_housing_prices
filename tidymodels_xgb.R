########## IMPORT ##########

# Import packages
library(tidyverse)
library(tidymodels)

# Source helper functions
source('helpers_new.R')

# Read in data
train <- readr::read_csv('data/train.csv')
test  <- readr::read_csv('data/test.csv')

# Generate cross-validation splits
set.seed(321)
train_splits <- rsample::vfold_cv(train, v = 10, repeats = 1, strata = NULL)

########## DATA ##########

# Define steps to munge data
set.seed(321)
munge_steps <- recipes::recipe(SalePrice ~ ., data = train) %>% 
  update_role(Id, new_role = 'id_column') %>% 
  update_role(YearBuilt, YearRemodAdd, YrSold, GarageYrBlt, new_role = 'no_longer_used') %>% 
  step_mutate(LotFrontage = log(LotFrontage + 1),
              LotArea = log(LotArea + 1),
              YearsOld = YrSold - YearBuilt,
              YearsSinceRemodel = YrSold - YearRemodAdd,
              MasVnrArea = log(MasVnrArea + 1),
              BsmtFinSF1 = log(BsmtFinSF1 + 1),
              BsmtUnfSF = log(BsmtUnfSF + 1),
              `1stFlrSF` = log(`1stFlrSF` + 1),
              GrLivArea = log(GrLivArea + 1),
              Fireplaces = factor(Fireplaces),
              GarageYearsOld = YrSold - GarageYrBlt,
              GarageYearsOld = log(GarageYearsOld + 1),
              OpenPorchSF = log(OpenPorchSF + 1),
              EnclosedPorch = log(EnclosedPorch + 1),
              MoSoldNew = factor(MoSold)) %>%
              step_log(SalePrice) %>% 
  step_medianimpute(all_predictors(), -all_nominal()) %>%
  step_modeimpute(all_nominal()) %>%
  step_other(all_nominal(), threshold = 0.1, other = 'other') %>%
  step_nzv(all_predictors()) %>%
  step_normalize(all_numeric(), -Id, -SalePrice) %>%
  step_dummy(all_nominal()) %>%
  step_rm(-all_predictors(), -all_outcomes(), -Id) %>% 
  check_cols(all_predictors()) %>% 
  check_missing(all_predictors())

# Preprocess model
preprocessing <- munge_steps %>% 
  prep(retain = TRUE, verbose = TRUE)



########## MODEL ##########

## Determine Grid Search hyperparameters
# mtry: number of variables available for splitting at each node
# trees: number of trees contained in the ensemble
# min_n: minimum number of data points in node allowing it to be further split
# tree_depth: maximum detph of tree (number of splits)
# learn_rate: learning rate (eta) of boosting algorithm
# loss_reduction: number for reduction in loss function
# sample_size: number (or proportion) of rows available

grid_params <- define_grid_params_xgb(trees = (1:30) * 50,
                                      tree_depth = c(1, 3, 5),
                                      learn_rate = c(0.1, 0.3))

# Fit model and evaluate cross-validated predictions
set.seed(321)
prepared_splits <- train_splits %>% 
  dplyr::mutate(train_data        = purrr::map(.x = splits, .f = rsample::analysis),
                train_transformed = purrr::map(.x = train_data, .f = ~ bake(object = preprocessing, .x)),
                test_data         = purrr::map(.x = splits, .f = rsample::assessment),
                test_transformed  = purrr::map(.x = test_data, .f = ~ bake(object = preprocessing, .x))) %>% 
  tidyr::crossing(grid_params) %>% 
  dplyr::select(fold = id, dplyr::one_of(names(grid_params)), everything())

set.seed(321)
modeling <- prepared_splits %>% model_building()

set.seed(321)
holdout_transformed <- bake(munge_steps %>% prep(training = train), test)

predictions <- modeling %>% 
  dplyr::mutate(train_yhat   = purrr::map2(.x = model, .y = train_transformed, .f = ~ dplyr::bind_cols(.y %>% dplyr::transmute(Id, SalePrice = exp(SalePrice)), exp(predict(.x, new_data = .y)))),
                test_yhat    = purrr::map2(.x = model, .y = test_transformed, .f = ~ dplyr::bind_cols(.y %>% dplyr::transmute(Id, SalePrice = exp(SalePrice)), exp(predict(.x, new_data = .y)))),
                train_rmse   = purrr::map(.x = train_yhat, .f = extract_rmse),
                test_rmse    = purrr::map(.x = test_yhat, .f = extract_rmse),
                holdout_yhat = purrr::map(.x = model, .f = ~ exp(predict(.x, new_data = holdout_transformed)))) %>% 
  tidyr::unnest(train_rmse, test_rmse) %>% 
  rename(train_rmse = rmse, test_rmse = rmse1)

# Combine folds to find average RMSE
aggregated_models <- predictions %>% 
  dplyr::group_by_at(names(grid_params)) %>% 
  dplyr::summarize(train_rmse = mean(train_rmse),
                   test_rmse = mean(test_rmse)) %>% 
  dplyr::ungroup()



########## EXPORT ##########
best_model <- aggregated_models %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(test_rmse == min(test_rmse))

best_predictions <- predictions %>% 
  dplyr::filter(trees == best_model$trees,
                tree_depth == best_model$tree_depth,
                learn_rate == best_model$learn_rate) %>% 
  dplyr::select(fold, holdout_yhat) %>% 
  tidyr::unnest() %>% 
  dplyr::mutate(Id = rep(holdout_transformed$Id, 10)) %>% 
  dplyr::group_by(Id) %>% 
  dplyr::summarize(SalePrice = mean(.pred))

readr::write_csv(best_predictions, 'output/submission_tidymodels_xgb3.csv')
