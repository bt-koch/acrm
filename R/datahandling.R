read_data <- function() {
  df <- read.csv("data/lgd_dataset.csv")
  return(df)
}

read_input <- function(input) {
  
  df <- data.frame(
    customer = input$customer_type,
    real.estate.type = input$realestate_type,
    loan.amount = input$loan_amount,
    mortgage.collateral.MV = input$mortgage_collateral_mv,
    additional.collateral.MV = input$additional_collateral_mv,
    additional.collateral.type = input$additional_collateral_type
  )
  
  return(df)
  
}