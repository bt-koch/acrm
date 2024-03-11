read_data <- function() {
  df <- read.csv("data/lgd_dataset.csv")
  return(df)
}

preprocess_data <- function(data) {
  data$appartment <- ifelse(data$real.estate.type == "appartment", 1, 0)
  data$office_building <- ifelse(data$real.estate.type == "office building", 1, 0)
  data$retirement_account <- ifelse(data$additional.collateral.type == "retirement account", 1, 0)
  data$cash_account <- ifelse(data$additional.collateral.type == "cash account", 1, 0)
  data$X <- data$real.estate.type <- data$additional.collateral.type <- NULL
  names(data) <- gsub("\\.", "_", names(data)) |> tolower()
  return(data)
}

prepare_data <- function(data = read_data()) {

  data <- preprocess_data(data)
  
  data$lgd <- data$lgd * data$loan_amount
  
  segment_1 <- data[data$customer == "private",]
  segment_2 <- data[data$customer == "corporate",]
  segment_1$customer <- NULL
  segment_2$customer <- NULL
  
  return(list("segment_1" = segment_1, "segment_2" = segment_2))
}

get_relevant_segment <- function(input = read_input(input)) {
  if (unique(input$customer) == "private") {
    return("segment_1")
  } else if (unique(input$customer == "corporate")) {
    return("segment_2")
  } else {
    stop("segment cannot be assigned correctly")
  }
}

read_input <- function(input) {
  df <- data.frame(
    customer = input$customer_type,
    real.estate.type = input$real_estate_type,
    loan.amount = input$loan_amount,
    mortgage.collateral.MV = input$mortgage_collateral_mv,
    additional.collateral.MV = input$additional_collateral_mv,
    additional.collateral.type = input$additional_collateral_type
  )
  return(df)
}

dummy_input <- function(customer_type = "private",
                        real_estate_type = "appartment",
                        loan_amount = 6130452,
                        mortgage_collateral_mv = 7520761,
                        additional_collateral_mv = 311572,
                        additional_collateral_type = "retirement account") {
  input <- list(
    customer_type = customer_type,
    real_estate_type = real_estate_type,
    loan_amount = loan_amount,
    mortgage_collateral_mv = mortgage_collateral_mv,
    additional_collateral_mv = additional_collateral_mv,
    additional_collateral_type = additional_collateral_type
  )
  return(input)
}

render_value <- function(value, type = "percent") {
  if (type == "percent") {
    value <- round(value*100, 2)
    value <- paste0(value, "%")
  }
  return(value)
}
