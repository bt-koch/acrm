# Define server logic
server <- function(input, output) {
  
  # Event observer for the button press
  observeEvent(input$estimate, {
    output$lgd_estimation <- renderValueBox({
      valueBox(
        linear_regression_predict(data = prepare_data(data = read_input(input))) |> 
          cap_prediction() |> 
          render_value(),
        "Loss Given Default",
        icon = icon("calculator"),
        color = "purple"
      )
    })
  })
  
}