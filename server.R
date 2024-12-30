# server.R - Define server logic

server <- function(input, output, session) {
  # Database queries
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
  
  # Update UI choices with informative labels
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
  
  # Debug outputs
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
    storage_systems() %>% filter(model_name == input$storage_system)
  })
  
  # Project metrics calculation
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
  
  # Outputs
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
  
  # Mining Analysis Plots
  output$profitability_plot <- renderPlotly({
    req(miners())
    
    # Generate BTC price range
    btc_prices <- seq(30000, 70000, by = 5000)
    
    # Calculate profitability for each miner at different BTC prices
    plot_data <- expand.grid(
      model_name = miners()$model_name,
      btc_price = btc_prices
    )
    
    plot_data <- plot_data %>%
      left_join(miners(), by = "model_name") %>%
      rowwise() %>%
      mutate(
        profit = calculate_profitability(
          list(hashrate = hashrate, power_consumption = power_consumption),
          btc_price,
          input$electricity_cost,
          input$network_hashrate
        )$daily_profit
      )
    
    plot_ly(plot_data, x = ~btc_price, y = ~profit, color = ~model_name, 
            type = 'scatter', mode = 'lines') %>%
      layout(
        title = "Daily Profitability by BTC Price",
        xaxis = list(title = "BTC Price (USD)"),
        yaxis = list(title = "Daily Profit (USD)"),
        showlegend = TRUE
      )
  })
  
  output$efficiency_plot <- renderPlotly({
    req(miners())
    
    plot_ly() %>%
      add_bars(
        data = miners(),
        x = ~model_name,
        y = ~efficiency,
        name = "Efficiency (J/TH)",
        marker = list(color = 'blue')
      ) %>%
      add_bars(
        data = miners(),
        x = ~model_name,
        y = ~hashrate,
        name = "Hashrate (TH/s)",
        yaxis = "y2",
        marker = list(color = 'red')
      ) %>%
      layout(
        title = "Miner Efficiency Comparison",
        xaxis = list(title = "Miner Model", tickangle = -45),
        yaxis = list(title = "Efficiency (J/TH)", side = "left"),
        yaxis2 = list(title = "Hashrate (TH/s)", side = "right", overlaying = "y"),
        showlegend = TRUE,
        legend = list(x = 0.7, y = 1),
        margin = list(b = 100)
      )
  })
  
  output$power_hashrate_plot <- renderPlotly({
    req(miners())
    
    plot_ly(
      data = miners(),
      x = ~hashrate,
      y = ~power_consumption,
      type = 'scatter',
      mode = 'markers+text',
      text = ~model_name,
      textposition = 'top',
      marker = list(
        size = 12,
        color = 'purple'
      )
    ) %>%
      layout(
        title = "Power Consumption vs Hashrate",
        xaxis = list(title = "Hashrate (TH/s)"),
        yaxis = list(title = "Power Consumption (W)"),
        showlegend = FALSE
      )
  })
}