# Define server logic
server <- function(input, output) {
  
  # Event observer for the button press
  observeEvent(input$calculate, {

    df_header <- show_dataframe(read_input(input))
    
    # Update the output
    output$head_of_df <- renderTable({
      df_header
    })
    
    output$prediction <- renderText({
      paste(
        "Estimated Loss Given Default:",
        linear_regression_predict(data = prepare_data(data = read_input(input)))
      )
    })
    
  })
}