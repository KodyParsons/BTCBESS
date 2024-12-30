# global.R - Load packages and shared resources

# Load required packages ----
library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(plotly)
library(DT)

# Configuration
DB_CONFIG <- list(
  db_path = "data/btcbess.sqlite",
  miners_csv = "data/miners.csv",
  storage_csv = "data/storage.csv"
)

# Load all helper functions from R directory
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) {
  source(file)
}

# Database connection function
db_connect <- function(config = DB_CONFIG) {
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), config$db_path)
    if (!dbIsValid(con)) {
      stop("Invalid database connection")
    }
    return(con)
  }, error = function(e) {
    log_error(paste("Database connection failed:", e$message))
    stop(paste("Could not connect to database:", e$message))
  })
}

# Mining calculations
calculate_daily_btc <- function(hashrate_ths, network_hashrate_ehs) {
  blocks_per_day <- 144  # 24 hours * 6 blocks per hour
  network_hashrate_ths <- network_hashrate_ehs * 1000 * 1000  # Convert EH/s to TH/s
  
  daily_btc <- (hashrate_ths * blocks_per_day * 6.25) / network_hashrate_ths
  return(daily_btc)
}

calculate_profitability <- function(miner_data, btc_price, electricity_cost, network_hashrate) {
  daily_power_cost <- (miner_data$power_consumption / 1000) * 24 * electricity_cost
  daily_btc <- calculate_daily_btc(miner_data$hashrate, network_hashrate)
  daily_revenue <- daily_btc * btc_price
  daily_profit <- daily_revenue - daily_power_cost
  
  return(list(
    daily_profit = daily_profit,
    daily_revenue = daily_revenue,
    daily_power_cost = daily_power_cost,
    daily_btc = daily_btc
  ))
}