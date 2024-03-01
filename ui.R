library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "MVP: LGD-Modeling"),
  dashboardSidebar(
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
  ),
  dashboardBody(
    tableOutput("head_of_df"),
    textOutput("prediction")
  )
)
