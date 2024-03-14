validate_input <- function(user_input) {
  
  validation <- list()
  
  if (user_input$loan_amount < 0) {
    validation[[length(validation)+1]] <- list(
      type = "error",
      message = "Loan amount cannot be negative."
    )
  }
  
  if (user_input$mortgage_collateral_mv < 0) {
    validation[[length(validation)+1]] <- list(
      type = "error",
      message = "Market value of mortgage collateral cannot be negative."
    )
  }
  
  if (user_input$additional_collateral_mv < 0) {
    validation[[length(validation)+1]] <- list(
      type = "error",
      message = "Market value of additional collateral cannot be negative."
    )
  }
  
  if (user_input$loan_amount > user_input$mortgage_collateral_mv + user_input$additional_collateral_mv) {
    validation[[length(validation)+1]] <- list(
      type = "warning",
      message = "Loan amount exceeds market value of total collateral."
    )
  }
  
  if (!length(validation)) {
    validation[[1]] <- list(
      type = "success",
      message = "no problem"
    )
  }
  
  return(validation)
  
}

subset_validation_result <- function(validation_result, type) {
  message <- Filter(function(x) x[["type"]] == type, validation_result)
  message <- sapply(message, function(x) x[["message"]])
  message <- paste(message, collapse = "<br>")
  return(message)
}

