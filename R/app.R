# Load required packages ----
library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(plotly)
library(DT)

# Load all helper functions
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

# UI definition ----
sidebarLayout(
  sidebarPanel(
    # Project Parameters
    selectInput("miner_model", 
                "Select Mining Hardware", 
                choices = NULL),  # Will be populated from miners$model_name
    
    selectInput("storage_system", 
                "Select Storage System", 
                choices = NULL),  # Will be populated from storage_systems$model_name
      
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
                 plotlyOutput("daily_operation_plot"),
        ),
        # Add this new tab:
        tabPanel("Debug", 
                 h4("Miners Data:"),
                 verbatimTextOutput("debug_miners"),
                 h4("Storage Systems Data:"),
                 verbatimTextOutput("debug_storage")
        )
      )
    )
  )

# Server logic ----
server <- function(input, output, session) {
  # Database queries remain the same
  miners <- reactive({
    con <- db_connect()
    on.exit(dbDisconnect(con))
    get_miners(con)
  })
  
  storage_systems <- reactive({
    con <- db_connect()
    on.exit(dbDisconnect(con))
    get_storage_systems(con)
  })
  
  # Update UI choices with more informative labels
  observe({
    # Get the data
    miner_data <- miners()
    storage_data <- storage_systems()
    
    # Update miner choices with hashrate info
    updateSelectInput(session, "miner_model",
                      choices = setNames(
                        miner_data$model_name,
                        paste0(miner_data$model_name, 
                               " (", miner_data$hashrate, " TH/s)")
                      ))
    
    # Update storage choices with capacity info
    updateSelectInput(session, "storage_system",
                      choices = setNames(
                        storage_data$model_name,
                        paste0(storage_data$model_name, 
                               " (", storage_data$capacity_kwh, " kWh)")
                      ))
  })
  

  # Add these debug outputs:
  output$debug_miners <- renderPrint({
    str(miners())
  })
  
  output$debug_storage <- renderPrint({
    str(storage_systems())
  })
  
  # Selected equipment
  selected_miner <- reactive({
    req(input$miner_model)
    miners() %>% filter(model_name == input$miner_model)
  })
  
  selected_storage <- reactive({
    req(input$storage_system)
    storage_systems() %>% filter(model_name == input$storage_system)  # Changed from system_name to model_name
  })
  
  # Project metrics calculation ----
  project_metrics <- reactive({
    req(selected_miner(), selected_storage())
    
    calculate_project_metrics(
      miner_data = selected_miner(),
      storage_data = selected_storage(),
      btc_price = input$btc_price,
      electricity_cost = input$electricity_cost,
      discount_rate = input$discount_rate,
      project_duration = input$project_duration
    )
  })
  
  # Outputs ----
  output$npv_output <- renderText({
    req(project_metrics())
    paste("NPV: $", format(round(project_metrics()$npv), big.mark=","))
  })
  
  output$irr_output <- renderText({
    req(project_metrics())
    paste("IRR:", round(project_metrics()$irr * 100, 1), "%")
  })
  
  output$cashflow_chart <- renderPlotly({
    req(project_metrics())
    
    plot_ly(data = project_metrics()$annual_metrics,
            x = ~year,
            y = ~cumulative_cf,
            type = 'scatter',
            mode = 'lines+markers') %>%
      layout(title = "Cumulative Cash Flow",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Cumulative Cash Flow ($)"))
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)