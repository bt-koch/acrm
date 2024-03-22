
# TODO: function to read relevant input: maybe modify read_input in R/datahandling.R

draw_loans <- function(n_houses, n_appartments, n_offices) {
  
  data <- read_data()
  houses <- data[data$real_estate_type == "single family house",]
  appartments <- data[data$real_estate_type == "appartment",]
  offices <- data[data$real_estate_type == "office building",]
  
  simulated_houses <- houses[sample(nrow(houses), n_houses),]
  simulated_appartments <- appartments[sample(nrow(appartments), n_appartments),]
  simulated_offices <- offices[sample(nrow(offices), n_offices),]
  
  simulation <- rbind(
    simulated_houses,
    simulated_appartments,
    simulated_offices
  )
  
  simulation$lgd <- NA
  
  return(simulation)
  
}

# TODO: function to estimate all LGDs
estimate_pf_lgd <- function(data) {
  
  model_houses <- two_step_estimation_get("private", "single family house")
  model_appart <- two_step_estimation_get("private", "appartment")
  model_office <- two_step_estimation_get("corporate", "office building")
  
  data$haircut_mortgage <- data$haircut_additional <- NA
  
  data[data$real_estate_type == "single family house",]$haircut_mortgage <- model_houses$haircut_mortgage
  data[data$real_estate_type == "single family house",]$haircut_additional <- model_houses$haircut_additional
  
  data[data$real_estate_type == "appartment",]$haircut_mortgage <- model_appart$haircut_mortgage
  data[data$real_estate_type == "appartment",]$haircut_additional <- model_appart$haircut_additional
  
  data[data$real_estate_type == "office building",]$haircut_mortgage <- model_office$haircut_mortgage
  data[data$real_estate_type == "office building",]$haircut_additional <- model_office$haircut_additional
  
  data$lgd <- 1-data$haircut_mortgage*(data$mortgage_collateral_mv/data$loan_amount)-
    data$haircut_additional*(data$additional_collateral_mv/data$loan_amount)
  
  data$lgd <- pmax(data$lgd, 0)
  
  return(data)
  
}


