library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Loss Given Default Modeling", titleWidth = 300),
  dashboardSidebar(
    tags$style("@import url(https://use.fontawesome.com/releases/v6.5.1/css/all.css);"),
    width = 300,
    sidebarMenu(
      id = "sidebar",
      menuItem("Estimate loan-level LGD", tabName = "estimate_lgd", icon = icon("calculator")),
      menuItem("Simulate Portfolio LGD", tabName = "simulate_lgd", icon = icon("calculator"))
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
        choices = read_data()$real_estate_type |> unique()
      ),
      numericInput(
        inputId = "loan_amount",
        label = "Loan amount to customer (CHF)",
        value = 6130452,
        min = 0
      ),
      numericInput(
        inputId = "mortgage_collateral_mv",
        label = "Market value mortgage collateral (CHF)",
        value = 7520761,
        min = 0
      ),
      selectInput(
        inputId = "additional_collateral_type",
        label = "Type of additional collateral",
        choices = read_data()$additional_collateral_type |> unique()
      ),
      conditionalPanel(
        "input.additional_collateral_type != 'none'",
        numericInput(
          inputId = "additional_collateral_mv",
          label = "Market value additional collateral (CHF)",
          value = 311572,
          min = 0
        )
      ),
      actionButton("estimate", "Estimate LGD")
    ),
    conditionalPanel(
      "input.sidebar == 'simulate_lgd'",
      hr(style="border-color: grey"),
      h5(style = "position: relative;left: 15px;", strong("Single family houses")),
      numericInput(
        inputId = "n_houses",
        label = "Number of loans",
        value = 0,
        min = 0
      ),
      fluidRow(
        column(6,
          sliderInput(
            inputId = "pd_houses",
            label = "PD",
            value = 0.5,
            min = 0,
            max = 1,
            ticks = F
          )
        ),
        column(6,
          sliderInput(
            inputId = "ead_houses",
            label = "EAD",
            value = 0.5,
            min = 0,
            max = 1,
            ticks = F
          )
        )
      ),
      hr(style="border-color: grey"),
      h5(style = "position: relative;left: 15px;", strong("Apartments")),
      numericInput(
        inputId = "n_appartments",
        label = "Number of loans",
        value = 0,
        min = 0
      ),
      fluidRow(
        column(6,
               sliderInput(
                 inputId = "pd_appartments",
                 label = "PD",
                 value = 0.5,
                 min = 0,
                 max = 1,
                 ticks = F
               )
        ),
        column(6,
               sliderInput(
                 inputId = "ead_appartments",
                 label = "EAD",
                 value = 0.5,
                 min = 0,
                 max = 1,
                 ticks = F
               )
        )
      ),
      hr(style="border-color: grey"),
      h5(style = "position: relative;left: 15px;", strong("Office Buildings")),
      numericInput(
        inputId = "n_offices",
        label = "Number of loans",
        value = 0,
        min = 0
      ),
      fluidRow(
        column(6,
               sliderInput(
                 inputId = "pd_offices",
                 label = "PD",
                 value = 0.5,
                 min = 0,
                 max = 1,
                 ticks = F
               )
        ),
        column(6,
               sliderInput(
                 inputId = "ead_offices",
                 label = "EAD",
                 value = 0.5,
                 min = 0,
                 max = 1,
                 ticks = F
               )
        )
      ),
      actionButton("simulate", "Simulate LGD")
    )
    
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "estimate_lgd",
        fluidRow(
          infoBoxOutput("errors", width = 6)
        ),
        fluidRow(
          infoBoxOutput("warnings", width = 6)
        ),
        fluidRow(
          valueBoxOutput("lgd_estimation", width = 6)
        )
      ),
      tabItem(
        tabName = "simulate_lgd",
        fluidRow(
          infoBox(
            "warning",
            "Each calculation draws loans from dataset randomly with replacement, hence result changes in each calculation.",
            icon = icon("exclamation"),
            color = "yellow",
            width = 12
          )
        ),
        fluidRow(
          valueBoxOutput("pf_lgd", width = 12)
        ),
        fluidRow(
          valueBoxOutput("houses_lgd", width = 4),
          valueBoxOutput("appartments_lgd", width = 4),
          valueBoxOutput("offices_lgd", width = 4)
        )
      )
    )
  )
)
