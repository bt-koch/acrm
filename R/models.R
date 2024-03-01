get_dependent_variable <- function() {
  return("lgd")
}

get_predictors <- function() {
  return(c("loan.amount", "mortgage.collateral.MV", "additional.collateral.MV"))
}


linear_regression_calibrate <- function() {
  model <- linear_regression_fit(
    data = read_data(),
    dependent_variable = get_dependent_variable(),
    regressors = get_predictors()
  )
  saveRDS(model, "calibrated_models/linear_regression.rds")
}

linear_regression_fit <- function(data, dependent_variable, regressors) {
  model <- lm(reformulate(regressors, dependent_variable), data = data)
  return(model)
}

linear_regression_get <- function() {
  model <- readRDS("calibrated_models/linear_regression.rds")
}

linear_regression_predict <- function(model = linear_regression_get(), data) {
  predict.lm(model, newdata = data)
}