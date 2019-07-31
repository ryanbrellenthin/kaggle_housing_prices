# Kaggle Housing Prices

Scripts for submission to [House Prices Advanced Regression Techniques](https://www.kaggle.com/c/house-prices-advanced-regression-techniques) Kaggle competition

## Data

* Available for download at [this location](https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data) on Kaggle website
* Stored in [data](data/) subdirectory

*NOTE: I generally prefer not to store CSV data in GitHub but am going against that preference for the sake of having a reproducible portfolio of work.*

## Scripts

### Current Best:
* [XGBoost](tidymodels_xgb.R): XGBoost using `tidymodels` package

### Legacy Scripts:
* [Gradient-Boosted Machines](gbm.R): `gbm` function through `caret` package
* [glmnet](tidymodels_glmnet.R): `glmnet` regression using `tidymodels` package
* [Lasso regression](lasso_regression.R): `glmnet` function with `alpha = 1`
* [Ridge regression](ridge_regression.R): `glmnet` function wiht `alpha = 0`

## Output
* [GBM model #1](output/submission_gbm1.csv)
* [glmnet #1](output/submission_tidymodels_glmnet.csv)
* [XGBoost model #3 (previous versions not proper outputs)](output/submission_tidymodels_xgb3.csv)
