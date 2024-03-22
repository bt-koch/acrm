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

read_input <- function(input, tab = "estimate_lgd") {
  
  if (tab == "estimate_lgd") {
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
  } else if (tab == "simulate_lgd") {
    df <- data.frame(
      n_houses = ifelse(is.na(input$n_houses), 0, input$n_houses),
      pd_houses = input$pd_houses,
      ead_houses = input$ead_houses,
      n_appartments = ifelse(is.na(input$n_appartments), 0, input$n_appartments),
      pd_appartments = input$pd_appartments,
      ead_appartments = input$ead_appartments,
      n_offices = ifelse(is.na(input$n_offices), 0, input$n_offices),
      pd_offices = input$pd_offices,
      ead_offices = input$ead_offices
    )
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


# segmentation <- function(data) {
#   
#   data$loan_to_mortg <- data$loan_amount / data$mortgage_collateral_mv
#   data$loan_to_colla <- data$loan_amount / (data$mortgage_collateral_mv + data$additional_collateral_mv)
#   boxplot(data$loan_to_mortg~data$additional_collateral_type)
#   boxplot(data$loan_to_colla~data$additional_collateral_type)
#   
#   private <- data[data$customer == "private",]
#   private$loan_to_mortg <- private$loan_amount / private$
#   
#   
#   private_house <- private[private$real_estate_type == "single family house",]
#   private_appartment <- private[private$real_estate_type == "appartment",]
#   
#   
#   private_house$loan_to_mortg <- private_house$loan_amount / private_house$mortgage_collateral_mv
#   boxplot(private_house$loan_to_mortg~private_house$additional_collateral_type)
#   
#   private_house
#   
# }