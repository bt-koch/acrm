server <- function(input, output) {
  
  observeEvent(input$estimate, {
    
    model_input <- input |>
      read_input() |>
      preprocess_data()
    
    estimated_lgd <- linear_regression_predict(
      model = linear_regression_get(segment = get_relevant_segment(read_input(input))),
      data = model_input
    )
    
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
  })
  
}