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
      estimated_lgd <- two_step_estimation_estimate(input) |> 
        cap_prediction()
      
      estimated_lgd_nom <- estimated_lgd * model_input$loan_amount
      
      
      output$lgd_estimation <- renderValueBox({
        valueBox(
          paste0(
            render_value(estimated_lgd),
            " (", render_value(estimated_lgd_nom, type = "CHF"), ")"
          ),
          "Loss Given Default",
          icon = icon("calculator"),
          color = "purple"
        )
      })
    }
  })
  
  observeEvent(input$simulate, {
    
    output$pf_lgd <- renderValueBox({
      valueBox(
        paste(rnorm(1), " any stupid text"),
        "Overall Loss Given Default",
        icon = icon("list"),
        color = "purple"
      )
    })
    
    output$houses_lgd <- renderValueBox({
      valueBox(
        rnorm(1),
        "Loss Given Default for houses",
        icon = icon("house-chimney"),
        color = "aqua"
      )
    })
    
    output$appartments_lgd <- renderValueBox({
      valueBox(
        rnorm(1),
        "Loss Given Default for apartments",
        icon = icon("building"),
        color = "blue"
      )
    })
    
    output$offices_lgd <- renderValueBox({
      valueBox(
        rnorm(1),
        "Loss Given Default for offices",
        icon = icon("city"),
        color = "light-blue"
      )
    })
    
  })
  
}