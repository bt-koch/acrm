# general functions ----
get_dependent_variable <- function() {
  return("lgd")
}

get_predictors <- function() {
  return(names(prepare_data()[[1]])[names(prepare_data()[[1]]) != "lgd"])
}

cap_prediction <- function(prediction) {
  capped_prediction <- max(prediction, 0.05)
  capped_prediction <- min(capped_prediction, 1)
  return(capped_prediction)
}

scale_prediction <- function(prediction, input) {
  if (Sys.getenv("SCALE") == "nominal") {
    loan_amount <- read_input(input)$loan_amount
    recover <- read_input(input)$mortgage_collateral_mv + read_input(input)$additional_collateral_mv
    prediction <- (loan_amount - recover) / loan_amount
  } else if (Sys.getenv("SCALE") == "standardize") {
    data <- read_data()
    prediction <- prediction * sd(data$lgd) + mean(data$lgd)
  } else if (Sys.getenv("SCALE") != "percentage") {
    stop("invalid environment variable for 'SCALE'")
  }
  return(prediction)
}

# model evaluation ----
calculate_rmse <- function(actual, predicted) {
  rmse <- sqrt(sum((actual-predicted)^2)/length(actual))
  return(rmse)
}

calculate_rmle <- function(actual, predicted) {
  rmle <- sqrt(sum((ln(predicted+1)-ln(actual+1))^2)/length(actual))
  return(rmle)
}

cross_validation <- function(data, nfolds = 10,
                             dependent_variable = get_dependent_variable(),
                             regressors = get_predictors(),
                             convert_nom = F) {
  
  result <- c()
  data <- data[sample(nrow(data)),]
  folds <- cut(seq(1, nrow(data)), breaks = nfolds, labels = F)
  
  for (i in 1:nfolds) {
    index <- which(folds == i, arr.ind = T)
    test_set <- data[index,]
    train_set <- data[-index,]
    
    model <- linear_regression_fit(train_set, dependent_variable, regressors)
    predictions <- linear_regression_predict(model, test_set)
    
    if (convert_nom) predictions <- predictions / test_set$loan_amount
    
    result <- c(result, calculate_rmse(test_set$lgd, predictions))
  }
  
  return(mean(result))
}

calculate_differences <- function(actual, predicted) {
  difference <- actual - predicted
  return(difference)
}

plot_differences <- function(differences, main = "Distribution of observed - predicted LGD") {
  plot(density(differences), main = main)
  abline(v = mean(differences))
  abline(v = median(differences), lty = 2)
  abline(v = -sd(differences), col = "grey")
  abline(v = sd(differences), col = "grey")
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

linear_regression_predict <- function(model = linear_regression_get(segment = get_relevant_segment(read_input(input))), data) {
  predict.lm(model, newdata = data)
}