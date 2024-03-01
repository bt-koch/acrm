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
        inputId = "realestate_type",
        label = "Select type of real estate",
        choices = read_data()$real.estate.type |> unique()
      ),
      numericInput(
        inputId = "loan_amount",
        label = "Loan amount to customer",
        value = NA,
        min = 0
      ),
      numericInput(
        inputId = "mortgage_collateral_mv",
        label = "Market value of mortgage collateral",
        value = NA,
        min = 0
      ),
      numericInput(
        inputId = "additional_collateral_mv",
        label = "Market value of additional collateral",
        value = NA,
        min = 0
      ),
      selectInput(
        inputId = "additional_collateral_type",
        label = "Type of additional collateral",
        choices = read_data()$additional.collateral.type |> unique()
      ),
      actionButton("calculate", "Calculate")
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "estimate_lgd",
        tableOutput("head_of_df"),
        textOutput("prediction")
      ),
      tabItem(
        tabName = "todo",
        h2("Ideas:"),
        p("1. Calculate DT LGD"),
        # tags$ul(
        #   tags$li("item 1"),
        #   tags$li("item 2")
        # ),
        br(),
        p("2. Try to calculate proposed interest rate for loan"),
        tags$ul(
          tags$li("to cover expected losses?"),
          tags$li("to cover costs from minimal required capital?"),  # List item 2
          tags$li("Item 3")   # List item 3
        ),
      )
    )
  )
)
