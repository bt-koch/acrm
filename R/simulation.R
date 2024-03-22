
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
