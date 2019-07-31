########## Define helper functions ##########

# Function to define XGB grid search parameters
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

# Function to run a single XGB model
iterate_models <- function(df, mtry = 1, trees = 15, min_n = 1, tree_depth = 6, learn_rate = 0.3, loss_reduction = 0, sample_size = 0.6) {
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

# Function to build all models
model_building <- function(input_data) {
  result <- input_data %>% 
    dplyr::mutate(model = purrr::pmap(.l = list(train_transformed, trees, tree_depth, learn_rate), .f = ~ iterate_models(df = ..1, trees = ..2, tree_depth = ..3, learn_rate = ..4)))
  result
}

# Function to extract RMSE from predictions
extract_rmse <- function(prediction_df) {
  result <- prediction_df %>% 
    yardstick::metrics(truth = SalePrice, estimate = .pred) %>% 
    dplyr::filter(.metric == 'rmse') %>% 
    dplyr::summarise(rmse = mean(.estimate))
  result
}
