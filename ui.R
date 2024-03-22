library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "MVP: LGD-Modeling", titleWidth = 300),
  dashboardSidebar(
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
        ),
        fluidRow(
          p("todo: more validation rules -> not allowed combinations, i.e. private customer with office loan"),
          p("todo: show LGD also in CHF")
        )
      ),
      tabItem(
        tabName = "simulate_lgd",
        fluidRow(
          h1("show LGD in CHF for total portfolio and for each mortgage type"),
          p("show LGD of current PF and LGD if we would increase PF by n simulated loans")
        ),
        fluidRow(
          h1("show EL = EAD * PD * LGD whereby EAD and PD from user input (same for all contracts of mortgage type)")
        ),
        fluidRow(
          h1("show interest we would need to cover EL (therefore user input for maturity of loan)")
        ),
        fluidRow(
          valueBoxOutput("lgd_simulation", width = 6)
        )
      )
    )
  )
)
