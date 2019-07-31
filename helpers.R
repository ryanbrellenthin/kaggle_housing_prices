########## Old helper functions (pre-tidymodels) ##########

clean_factor_variables <- function(df, frequency_threshold = 0.01) {
  # Initialize result object
  result <- list()
  # Move low-frequency factors to 'Other' and classify NA values as 'Other'
  result$data <- df %>%
    dplyr::mutate_if(is.character, funs(forcats::fct_lump(., prop = frequency_threshold))) %>%  # Anything below threshold becomes "Other"
    # TODO: Fix this by moving it later and munging NAs with most common factor variable
    dplyr::mutate_if(is.factor, funs(forcats::fct_explicit_na(., na_level = 'Other'))) %>%  # Anything NA becomes "Other"
    dplyr::select_if(is.factor)
  # Gather names of fields with at least two factor levels
  result$fields <- result$data %>% 
    purrr::map_df(nlevels) %>% 
    dplyr::select_if(. > 1) %>% 
    names()
  # Limit data to only variables with at least two factor levels
  result$data <- result$data %>% 
    dplyr::select(dplyr::one_of(result$fields)) %>% 
    dplyr::mutate_if(is.factor, as.character)
  # Determine the most common factor level for each variable
  result$most_common <- result$data %>% 
    tidyr::gather() %>% 
    dplyr::count(key, value) %>% 
    dplyr::group_by(key) %>% 
    dplyr::top_n(1, n) %>% 
    dplyr::select(-n) %>% 
    dplyr::mutate(value = forcats::fct_explicit_na(value)) %>% 
    tidyr::spread(key = key, value = value)
  # Gather factor levels for each variable
  result$levels <- result$data %>% 
    purrr::map(~ unique(forcats::fct_inorder(.)))
  # Return result object
  result
}

clean_numeric_variables <- function(train_df, impute = 'mean') {
  numeric_data <- train_df %>% dplyr::select_if(is.numeric)
  # Initialize result object
  result <- list()
  # Compute mean of each numeric variable
  result$means <- numeric_data %>% 
    purrr::map_df(mean, na.rm = TRUE)
  # Compute median of each numeric variable
  result$medians <- numeric_data %>% 
    purrr::map_df(median, na.rm = TRUE)
  # Use impute argument to determine whether to impute NA values with mean or median
  if (impute == 'mean') {
    result$data <- numeric_data %>% 
      tidyr::replace_na(., replace = result$means)
  } else if (impute == 'median') {
    result$data <- numeric_data %>% 
      tidyr::replace_na(., replace = result$medians)
  } else {
    stop("Set impute to either 'mean' or 'median' and try again.")
  }
  # Gather field names
  result$fields <- names(result$data)
  # Return result object
  result
}

create_new_training_data <- function(factor_object, numeric_object) {
  # Initialize result object
  result <- list()
  # Combine factor fields and numeric fields into one vector
  result$fields <- c(factor_object$fields, numeric_object$fields)
  # Bring together factor data and numeric data and ensure consistent ordering
  result$data <- dplyr::bind_cols(factor_object$data, numeric_object$data) %>% 
    dplyr::select(dplyr::one_of(result$fields))
  # Return result object
  result
}

munge_factor_data <- function(test_df, factor_object) {
  # Initialize result object
  result <- list()
  # Select factor fields and then replace NA values with most common factor level from training set
  result$data <- test_df %>% 
    dplyr::select(dplyr::one_of(factor_object$fields)) %>% 
    tidyr::replace_na(., replace = factor_object$most_common)
  # Identify any factors in test data that were not in training data
  factors_to_convert <- purrr::map2(result$data %>% purrr::map(~ unique(forcats::fct_inorder(.))), factor_object$levels, setdiff)
  # Convert factors in test data that were not in training data to 'Other'
  result$data <- result$data %>% 
    purrr::map2_df(factors_to_convert, ~ forcats::fct_other(.x, drop = .y))
  # Return result
  result
}

munge_numeric_data <- function(test_df, numeric_object, impute = 'mean') {
  # Set replacement values as either means or medians of training set values
  if (impute == 'mean') {
    replacement_values <- numeric_object$means
  } else if (impute == 'median') {
    replacement_values <- numeric_object$medians
  } else {
    stop("Set impute to either 'mean' or 'median' and try again.")
  }
  # Initialize result object
  result <- list()
  # Select numeric columns and then replace NA values with predetermined replacement values
  result$data <- test_df %>% 
    dplyr::select_if(is.numeric) %>% 
    tidyr::replace_na(., replace = replacement_values)
  # Return result object
  result
}

combine_new_test_data <- function(factor_object, numeric_object, new_training_data) {
  # Initialize result object
  result <- list()
  # Bring together factor data and numeric data and ensure correct fields and consistent ordering
  result$data <- dplyr::bind_cols(factor_object$data, numeric_object$data) %>% 
    dplyr::select(dplyr::one_of(new_training_data$fields))
  # Return result object
  result
}
