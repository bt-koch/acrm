draw_loans <- function(n_houses, n_appartments, n_offices) {
  
  data <- read_data()
  houses <- data[data$real_estate_type == "single family house",]
  appartments <- data[data$real_estate_type == "appartment",]
  offices <- data[data$real_estate_type == "office building",]
  
  simulated_houses <- houses[sample(nrow(houses), n_houses, replace = T),]
  simulated_appartments <- appartments[sample(nrow(appartments), n_appartments, replace = T),]
  simulated_offices <- offices[sample(nrow(offices), n_offices, replace = T),]
  
  simulation <- rbind(
    simulated_houses,
    simulated_appartments,
    simulated_offices
  )
  
  if (nrow(simulation) > 0) simulation$lgd <- NA
  
  return(simulation)
  
}

estimate_pf_lgd <- function(data, spec = "lm_percent") {
  
  model_houses <- two_step_estimation_get("private", "single family house", spec)
  model_appart <- two_step_estimation_get("private", "appartment", spec)
  model_office <- two_step_estimation_get("corporate", "office building", spec)
  
  if (nrow(data) > 0) data$haircut_mortgage <- data$haircut_additional <- NA
  
  if ("single family house" %in% data$real_estate_type) {
    data[data$real_estate_type == "single family house",]$haircut_mortgage <- model_houses$haircut_mortgage
    data[data$real_estate_type == "single family house",]$haircut_additional <- model_houses$haircut_additional
  }
  
  if ("appartment" %in% data$real_estate_type) {
    data[data$real_estate_type == "appartment",]$haircut_mortgage <- model_appart$haircut_mortgage
    data[data$real_estate_type == "appartment",]$haircut_additional <- model_appart$haircut_additional
  }
  
  if ("office building" %in% data$real_estate_type) {
    data[data$real_estate_type == "office building",]$haircut_mortgage <- model_office$haircut_mortgage
    data[data$real_estate_type == "office building",]$haircut_additional <- model_office$haircut_additional
  }
  
  data$lgd <- 1-data$haircut_mortgage*(data$mortgage_collateral_mv/data$loan_amount)-
    data$haircut_additional*(data$additional_collateral_mv/data$loan_amount)
  
  data$lgd <- pmin(pmax(data$lgd, 0), 1)
  data$lgd_nom <- data$lgd * data$loan_amount
  
  return(data)
  
}

prepare_simulation_result <- function(data, level) {
  
  if (level == "portfolio") {
    index <- rep(T, nrow(data))
  } else if (level == "house") {
    index <- data$real_estate_type == "single family house"
  } else if (level == "appartment") {
    index <- data$real_estate_type == "appartment"
  } else if (level == "office") {
    index <- data$real_estate_type == "office building"
  }
  
  result <- paste0(
    render_value(sum(data[index,]$lgd_nom), type = "CHF"),
    " (", render_value(sum(data[index,]$lgd_nom)/sum(data[index,]$loan_amount)), ")"
  )
  
  return(result)
}


