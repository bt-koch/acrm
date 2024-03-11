# general functions ----
get_dependent_variable <- function() {
  return("lgd")
}

get_predictors <- function() {
  return(names(prepare_data()[[1]])[names(prepare_data()[[1]]) != "lgd"])
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
  # TODO: behavior not correct yet
  return(max(prediction, 0.05))
}

# linear regression ----
linear_regression_calibrate <- function() {
  
  data <- prepare_data()
  
  models <- lapply(data, function(x) {
    linear_regression_fit(
      data = x,
      dependent_variable = get_dependent_variable(),
      regressors = get_predictors()
    )
  })
  
  saveRDS(models, "calibrated_models/linear_regression.rds")
}

linear_regression_fit <- function(data, dependent_variable, regressors) {
  model <- lm(reformulate(regressors, dependent_variable), data = data)
  return(model)
}

linear_regression_get <- function(segment = get_relevant_segment(input)) {
  models <- readRDS("calibrated_models/linear_regression.rds")
  return(models[[segment]])
}

linear_regression_predict <- function(model = linear_regression_get(segment = get_relevant_segment(input)), data) {
  predict.lm(model, newdata = data)
}