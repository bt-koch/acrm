read_data <- function() {
  df <- read.csv("data/lgd_dataset.csv")
  return(df)
}

prepare_data <- function(data = read_data()) {
  # TODO: Problem -> we have collinearity
  data$corporate <- ifelse(data$customer == "corporate", 1, 0)
  data$appartment <- ifelse(data$real.estate.type == "appartment", 1, 0)
  data$office_building <- ifelse(data$real.estate.type == "office building", 1, 0)
  data$retirement_account <- ifelse(data$additional.collateral.type == "retirement account", 1, 0)
  data$cash_account <- ifelse(data$additional.collateral.type == "cash account", 1, 0)
  data$X <- data$customer <- data$real.estate.type <- data$additional.collateral.type <- NULL
  return(data)
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
