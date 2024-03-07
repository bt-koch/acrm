# general functions ----
get_dependent_variable <- function() {
  return("lgd")
}

get_predictors <- function() {
  return(names(prepare_data())[names(prepare_data()) != "lgd"])
}

# model evaluation ----
calculate_rmse <- function(actual, predicted) {
  rmse <- sqrt(sum(actual-predicted)^2/length(actual))
  return(rmse)
}

calculate_rmle <- function(actual, predicted) {
  rmle <- sqrt(sum(ln(predicted+1)-ln(actual+1))^2/length(actual))
  return(rmle)
}

cap_prediction <- function(prediction) {
  return(max(prediction, 0.05))
}

# linear regression ----
linear_regression_calibrate <- function() {
  model <- linear_regression_fit(
    data = prepare_data(),
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