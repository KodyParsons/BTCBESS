# ui.R - Define the user interface

ui <- fluidPage(
  titlePanel("BTCBESS Analysis"),
  
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
        
        tabPanel("Mining Analysis",
                 fluidRow(
                   column(4,
                          numericInput("network_hashrate", 
                                       "Network Hashrate (EH/s)", 
                                       value = 500,
                                       min = 1,
                                       max = 1000),
                          numericInput("block_reward",
                                       "Block Reward (BTC)",
                                       value = 6.25,
                                       min = 0)
                   ),
                   column(8,
                          plotlyOutput("profitability_plot", height = "400px")
                   )
                 ),
                 fluidRow(
                   column(6,
                          plotlyOutput("efficiency_plot", height = "400px")
                   ),
                   column(6,
                          plotlyOutput("power_hashrate_plot", height = "400px")
                   )
                 )
        ),
        
        tabPanel("Sensitivity Analysis",
                 plotlyOutput("sensitivity_plot")
        ),
        
        tabPanel("Operating Strategy",
                 plotlyOutput("daily_operation_plot")
        ),
        
        tabPanel("Debug", 
                 h4("Miners Data:"),
                 verbatimTextOutput("debug_miners"),
                 h4("Storage Systems Data:"),
                 verbatimTextOutput("debug_storage")
        )
      )
    )
  )
)