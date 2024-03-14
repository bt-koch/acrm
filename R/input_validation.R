validate_input <- function(user_input) {
  
  validation <- list()
  
  if (user_input$mortgage_collateral_mv < 0) {
    validation[[length(validation)+1]] <- list(
      type = "warning",
      message = "cannot be negative"
    )
  }
  
  if (user_input$customer == "private") {
    validation[[length(validation)+1]] <- list(
      type = "warning",
      message = "test message"
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