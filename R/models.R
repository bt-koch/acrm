# general functions ----
get_dependent_variable <- function() {
  return("lgd")
}

get_predictors <- function() {
  return(names(prepare_data()[[1]])[names(prepare_data()[[1]]) != "lgd"])
}

cap_prediction <- function(prediction) {
  return(max(min(prediction, 1), 0))
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
                             model = "lm",
                             dependent_variable = get_dependent_variable(),
                             regressors = get_predictors(),
                             convert_nom = F,
                             subset = NULL,
                             y_log = F,
                             nominal = F,
                             # only for tobit
                             tobit_dist = "gaussian") {
  
  result <- c()
  if (!is.null(subset)) data <- data[subset,]
  data <- data[sample(nrow(data)),]
  folds <- cut(seq(1, nrow(data)), breaks = nfolds, labels = F)
  
  for (i in 1:nfolds) {
    index <- which(folds == i, arr.ind = T)
    test_set <- data[index,]
    train_set <- data[-index,]
    
    if (model == "lm") {
      m <- linear_regression_fit(train_set, dependent_variable, regressors)
      predictions <- linear_regression_predict(m, test_set)
      predictions <- pmax(0, predictions)
    } else if (model == "tobit") {
      m <- AER::tobit(reformulate(regressors, dependent_variable),
                          data = train_set, #subset = tobit_subset_index,
                      dist = tobit_dist)
      predictions <- predict(m, test_set)
      if (y_log) exp(predictions)-1
      predictions <- pmax(predictions, 0)
    }
    
    
    
    if (convert_nom) predictions <- predictions / test_set$loan_amount
    if (nominal) result <- c(result, calculate_rmse(test_set$lgd_nom, predictions))
    if (!nominal) result <- c(result, calculate_rmse(test_set$lgd, predictions))
    
  }
  
  return(mean(result))
}

calculate_differences <- function(actual, predicted) {
  difference <- actual - predicted
  return(difference)
}

plot_differences <- function(differences, main = "Distribution of observed - predicted LGD in CHF",
                             xlim = c(min(differences), max(differences))) {
  # plot(density(differences), main = main, xlim = xlim)
  hist(differences, main = main, xlim = xlim, breaks = 50)
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

# two step method
two_step_estimation <- function(data, customer_type, real_estate_type, save = F) {
  
  df <- data[
    data$customer == customer_type &
      data$real_estate_type == real_estate_type &
      data$additional_collateral_mv == 0,
  ]
  
  ## step 1
  # mean approach
  df$y <- ((1-df$lgd)*df$loan_amount)/df$mortgage_collateral_mv
  haircut_mortgage_mean <- mean(df$y)
  
  # lm nominal
  df$y <- df$lgd*df$loan_amount
  step1_m1 <- lm(y~0+offset(1*loan_amount)+mortgage_collateral_mv, data = df)
  haircut_mortgage_m1 <- step1_m1$coefficients[1]*-1
  
  # lm %
  df$y <- df$lgd
  df$x <- df$mortgage_collateral_mv/df$loan_amount
  step2_m2 <- lm(I(y-1)~0+x, data = df)
  haircut_mortgage_m2 <- step2_m2$coefficients[1]*-1
  
  ## step 2
  df <- data[
    data$customer == customer_type &
      data$real_estate_type == real_estate_type &
      data$additional_collateral_mv > 0,
  ]
  
  # lm nominal
  df$y <- df$lgd*df$loan_amount+haircut_mortgage_m1*df$mortgage_collateral_mv
  step2_m1 <- lm(y~0+offset(1*loan_amount)+additional_collateral_mv, data = df)
  haircut_additional_m1 <- step2_m1$coefficients[1]*-1
  
  # lm %
  df$y <- df$lgd+haircut_mortgage_m2*df$mortgage_collateral_mv/df$loan_amount
  df$x <- df$additional_collateral_mv/df$loan_amount
  step2_m2 <- lm(I(y-1)~0+x, data = df)
  haircut_additional_m2 <- step2_m2$coefficients[1]*-1
  
  ## ad hoc coefficients
  adhoc_coefs <- list(
    "private" = list(
      "appartment" = list(
        "haircut_mortgage" = 0.77,
        "haircut_additional" = 0.82
      ),
      "single family house" = list(
        "haircut_mortgage" = 0.72,
        "haircut_additional" = 0.86
      )
    ),
    "corporate" = list(
      "office building" = list(
        "haircut_mortgage" = 0.65,
        "haircut_additional" = 0.93
      )
    )
  )
  
  # result
  result <- list(
    customer_type = customer_type,
    real_estate_type = real_estate_type,
    mean_approach = list(
      "haircut_mortgage" = haircut_mortgage_mean,
      "haircut_additional" = NA
    ),
    lm_nominal = list(
      "haircut_mortgage" = haircut_mortgage_m1,
      "haircut_additional" = haircut_additional_m1
    ),
    lm_percent = list(
      "haircut_mortgage" = haircut_mortgage_m2,
      "haircut_additional" = haircut_additional_m2
    ),
    adhoc = list(
      "haircut_mortgage" = adhoc_coefs[[customer_type]][[real_estate_type]]$haircut_mortgage,
      "haircut_additional" = adhoc_coefs[[customer_type]][[real_estate_type]]$haircut_additional
    )
  )
  
  if (save) {
    saveRDS(
      object = result,
      file = paste0("calibrated_models/", customer_type, "_", gsub(" ", "_", real_estate_type), ".rds")
    )
  } else {
    return(result)
  }
}

two_step_estimation_calibrate <- function() {
  data <- read_data()
  two_step_estimation(data, "private", "appartment", save = T)
  two_step_estimation(data, "private", "single family house", save = T)
  two_step_estimation(data, "corporate", "office building", save = T)
}

two_step_estimation_get <- function(customer_type, real_estate_type, specification = "lm_percent") {
  model <- readRDS(paste0("calibrated_models/", customer_type, "_", gsub(" ", "_", real_estate_type), ".rds"))
  return(list(
    haircut_mortgage = model[[specification]]$haircut_mortgage,
    haircut_additional = model[[specification]]$haircut_additional
  ))
}

two_step_estimation_estimate <- function(input, cap_prediction = TRUE) {
  input <- read_input(input)
  customer_type <- input$customer
  real_estate_type <- input$real_estate_type
  mortgage_collateral_mv <- input$mortgage_collateral_mv
  additional_collateral_mv <- input$additional_collateral_mv
  loan_amount <- input$loan_amount

  model <- two_step_estimation_get(customer_type, real_estate_type)
  haircut_mortgage <- model$haircut_mortgage
  haircut_additional <- model$haircut_additional
  
  lgd <- 1-haircut_mortgage*(mortgage_collateral_mv/loan_amount)-
    haircut_additional*(additional_collateral_mv/loan_amount)
  
  if (cap_prediction) pmin(pmax(lgd, 0), 1)
  return(lgd)
}
