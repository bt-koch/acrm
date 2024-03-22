server <- function(input, output) {
  
  observeEvent(input$estimate, {
    
    model_input <- input |>
      read_input() |>
      preprocess_data()
    
    validation_result <- validate_input(model_input)
    
    if (any(lapply(validation_result, function(x) x[["type"]]) == "warning")) {
      output$warnings <- renderInfoBox({
        message <- subset_validation_result(validation_result, type = "warning")
        infoBox(
          "Warnings",
          HTML(message),
          icon = icon("exclamation"),
          color = "yellow"
        )
      })
    } else {
      output$warnings <- NULL
    }
    
    if (any(lapply(validation_result, function(x) x[["type"]]) == "error")) {
      output$errors <- renderInfoBox({
        message <- subset_validation_result(validation_result, type = "error")
        infoBox(
          "Errors",
          HTML(message),
          icon = icon("exclamation"),
          color = "red"
        )
      })
    } else {
      output$errors <- NULL
    }
    
    if (any(lapply(validation_result, function(x) x[["type"]]) == "error")) {
      
      output$lgd_estimation <- renderInfoBox({
        valueBox(
          "Calculation not possible",
          "please see error message",
          icon = icon("bug"),
          color = "red"
        )
      })
      
    } else {
      estimated_lgd <- two_step_estimation_estimate(input)

      output$lgd_estimation <- renderValueBox({
        valueBox(
          estimated_lgd |> 
            cap_prediction() |>
            render_value(),
          "Loss Given Default",
          icon = icon("calculator"),
          color = "purple"
        )
      })
    }
    
    
  })
  
}