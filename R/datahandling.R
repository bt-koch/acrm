read_data <- function(path = "data/lgd_dataset.csv") {
  data <- read.csv(path)
  names(data) <- gsub("\\.", "_", names(data)) |> tolower()
  data$x <- NULL
  return(data)
}

preprocess_data <- function(data) {
  data$appartment <- ifelse(data$real_estate_type == "appartment", 1, 0)
  data$office_building <- ifelse(data$real_estate_type == "office building", 1, 0)
  data$retirement_account <- ifelse(data$additional_collateral_type == "retirement account", 1, 0)
  data$cash_account <- ifelse(data$additional_collateral_type == "cash account", 1, 0)
  data$X <- data$real_estate_type <- data$additional_collateral_type <- NULL
  
  if (Sys.getenv("SCALE") == "percentage") {
    data$mortgage_collateral_mv <- data$mortgage_collateral_mv / data$loan_amount
    data$additional_collateral_mv <- data$additional_collateral_mv / data$loan_amount
    data$loan_amount <- 1
  } else if (Sys.getenv("SCALE") == "standardize") {
    data$mortgage_collateral_mv <- (data$mortgage_collateral_mv - mean(read_data()$mortgage_collateral_mv)) / sd(read_data()$mortgage_collateral_mv)
    data$additional_collateral_mv <- (data$additional_collateral_mv - mean(read_data()$additional_collateral_mv)) / sd(read_data()$additional_collateral_mv)
    data$loan_amount <- (data$loan_amount - mean(read_data()$loan_amount)) / sd(read_data()$loan_amount)
  } else if (Sys.getenv("SCALE") != "nominal") {
    stop("invalid environment variable for 'SCALE'")
  }
  
  return(data)
}

prepare_data <- function(data = read_data()) {

  data <- preprocess_data(data)
  
  if (Sys.getenv("SCALE") == "nominal") {
    data$lgd <- data$lgd * data$loan_amount
  } else if (Sys.getenv("SCALE") == "standardize") {
    data$lgd <- (data$lgd - mean(data$lgd)) / sd(data$lgd)
  } else if (Sys.getenv("SCALE") != "percentage") {
    stop("invalid environment variable for 'SCALE'")
  } 

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
    real_estate_type = input$real_estate_type,
    loan_amount = input$loan_amount,
    mortgage_collateral_mv = input$mortgage_collateral_mv,
    additional_collateral_mv = input$additional_collateral_mv,
    additional_collateral_type = input$additional_collateral_type
  )
  
  if (input$additional_collateral_type == "none") {
    df$additional_collateral_mv <- 0
  }
  
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
