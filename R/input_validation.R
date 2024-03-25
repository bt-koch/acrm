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
  
  if (user_input$customer == "private" & 
      (!user_input$real_estate_type %in% c("appartment", "single family house") |
       !user_input$additional_collateral_type %in% c("retirement account", "none"))
      ) {
    validation[[length(validation)+1]] <- list(
      type = "error",
      message = paste("Invalid combination. For private clients, LGD can only be estimated for",
                      "appartments and single family houses and only for retirement account or no additional collateral.")
    )
  }
  
  if (user_input$customer == "corporate" & 
      (!user_input$real_estate_type %in% c("office building") |
       !user_input$additional_collateral_type %in% c("cash account", "none"))
  ) {
    validation[[length(validation)+1]] <- list(
      type = "error",
      message = paste("Invalid combination. For corporate clients, LGD can only be estimated for",
                      "office buildings and only for cash account or no additional collateral.")
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

