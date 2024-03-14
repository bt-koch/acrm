server <- function(input, output) {
  
  observeEvent(input$estimate, {
    
    model_input <- input |>
      read_input() |>
      preprocess_data()
    
    # model_input <- dummy_input(mortgage_collateral_mv = -12345) |> 
    #   read_input() |> 
    #   preprocess_data()
    validation_result <- validate_input(model_input)
    
    if (any(lapply(validation_result, function(x) x[["type"]]) == "warning")) {
      
      output$warnings <- renderInfoBox({
        
        warnings <- Filter(function(x) x[["type"]] == "warning", validation_result)
        warnings <- sapply(warnings, function(x) x[["message"]])
        warnings <- paste(warnings, collapse = "<br>")
        
        infoBox(
          "Warnings",
          HTML(warnings),
          icon = icon("exclamation"),
          color = "yellow"
        )
      })
      
    } else {
      
      output$warnings <- NULL
      
    }
    
    
    # stop if error
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
      estimated_lgd <- linear_regression_predict(
        model = linear_regression_get(segment = get_relevant_segment(read_input(input))),
        data = model_input
      ) |> scale_prediction(input = input)
      
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