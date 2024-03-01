# Define server logic
server <- function(input, output) {
  
  # Event observer for the button press
  observeEvent(input$calculate, {
    printedText <- printSomething()
    
    # Update the output
    output$printedText <- renderText({
      printedText
    })
  })
}