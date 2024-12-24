# global.R
library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(plotly)
library(DT)

# Database connection
db_connect <- function() {
  con <- dbConnect(
    RSQLite::SQLite(),
    "path_to_your_db.sqlite"
  )
  return(con)
}

# ui.R
ui <- fluidPage(
  titlePanel("Bitcoin Mining Project Finance Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      # Project Parameters
      selectInput("miner_model", "Select Miner Model", choices = NULL),
      selectInput("storage_system", "Select Storage System", choices = NULL),
      
      # Financial Parameters
      numericInput("btc_price", "Bitcoin Price (USD)", value = 50000),
      numericInput("electricity_cost", "Electricity Cost ($/kWh)", value = 0.05),
      numericInput("discount_rate", "Discount Rate (%)", value = 10),
      numericInput("project_duration", "Project Duration (Years)", value = 5)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Project Overview",
                 h3("Key Metrics"),
                 verbatimTextOutput("npv_output"),
                 verbatimTextOutput("irr_output"),
                 plotlyOutput("cashflow_chart")
        ),
        tabPanel("Sensitivity Analysis",
                 plotlyOutput("sensitivity_plot")
        ),
        tabPanel("Operating Strategy",
                 plotlyOutput("daily_operation_plot")
        )
      )
    )
  )
)

# server.R
server <- function(input, output, session) {
  # Reactive database queries
  miners <- reactive({
    con <- db_connect()
    miners_data <- dbGetQuery(con, "SELECT * FROM miners")
    dbDisconnect(con)
    return(miners_data)
  })
  
  storage_systems <- reactive({
    con <- db_connect()
    storage_data <- dbGetQuery(con, "SELECT * FROM storage_systems")
    dbDisconnect(con)
    return(storage_data)
  })
  
  # Update UI choices based on database
  observe({
    updateSelectInput(session, "miner_model",
                      choices = miners()$model_name)
    updateSelectInput(session, "storage_system",
                      choices = storage_systems()$system_name)
  })
  
  # Financial calculations
  project_metrics <- reactive({
    # Add your NPV/IRR calculations here
    # This will need to account for:
    # - Mining revenue (BTC generation * price)
    # - Energy costs
    # - Storage system optimization
    # - Equipment depreciation
    # - Operating costs
  })
  
  # Outputs
  output$npv_output <- renderText({
    # Format and display NPV
  })
  
  output$irr_output <- renderText({
    # Format and display IRR
  })
  
  output$cashflow_chart <- renderPlotly({
    # Create cashflow visualization
  })
  
  output$sensitivity_plot <- renderPlotly({
    # Create sensitivity analysis plot
  })
  
  output$daily_operation_plot <- renderPlotly({
    # Create daily operation strategy visualization
  })
}

# Run the app
shinyApp(ui = ui, server = server)
