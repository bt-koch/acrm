library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "MVP: LGD-Modeling"),
  dashboardSidebar(
    sidebarMenu(
      id = "sidebar",
      menuItem("Estimate LGD", tabName = "estimate_lgd", icon = icon("calculator")),
      menuItem("To Do", tabName = "todo", icon = icon("th"))
    ),
    conditionalPanel(
      "input.sidebar == 'estimate_lgd'",
      selectInput(
        inputId = "customer_type",
        label = "Select customer type",
        choices = read_data()$customer |> unique()
      ),
      selectInput(
        inputId = "real_estate_type",
        label = "Select type of real estate",
        choices = read_data()$real.estate.type |> unique()
      ),
      numericInput(
        inputId = "loan_amount",
        label = "Loan amount to customer",
        value = 6130452,
        min = 0
      ),
      numericInput(
        inputId = "mortgage_collateral_mv",
        label = "Market value of mortgage collateral",
        value = 7520761,
        min = 0
      ),
      numericInput(
        inputId = "additional_collateral_mv",
        label = "Market value of additional collateral",
        value = 311572,
        min = 0
      ),
      selectInput(
        inputId = "additional_collateral_type",
        label = "Type of additional collateral",
        choices = read_data()$additional.collateral.type |> unique()
      ),
      actionButton("estimate", "Estimate LGD")
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "estimate_lgd",
        fluidRow(
          valueBoxOutput("lgd_estimation")
        )
      ),
      tabItem(
        tabName = "todo",
        h2("To Do's:"),
        p("1. Model specification"),
        tags$ul(
          tags$li("Resolve problem with multicollinearity"),
          tags$li("Implement more models",
                  br(),
                  img(src='images/lgd_models.png', align = "left",
                      height="25%", width="25%"), 
                  )
        ),
        div(style = "height:325px"),
        p("2. Model training"),
        tags$ul(
          tags$li("Implement Cross Validation")
        ),
        p("3. How to evaluate models?"),
        tags$ul(
          tags$li("Using Root Mean Squared Error?"),
          tags$li("Root Mean Squared Logarithmic Error?")
        ),
        p("4. Reestimate LGD only upon button press"),
        h2("Ideas:"),
        p("1. Calculate DT LGD?"),
        p("2. Evaluate implications on regulatory capital?"),
        p("3. Try to calculate proposed interest rate for loan"),
        tags$ul(
          tags$li("to cover expected losses?"),
          tags$li("to cover costs from minimal required capital?")
        ),
        p("4. Somehow simulate portfolio over time?"),
        p("5. Implement decision rule: should loan be approved? with which interest rate?"),
        p("6. Show model performance metrics in the app?")
      )
    )
  )
)
