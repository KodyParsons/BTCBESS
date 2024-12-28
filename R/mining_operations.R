# Mining operations calculations

#' Calculate daily BTC mining rewards
#' @param hashrate_ths Hashrate in TH/s
#' @param difficulty Current network difficulty
#' @return Daily BTC rewards
calculate_daily_btc <- function(hashrate_ths, difficulty) {
  # Basic mining reward calculation
  # Current block reward is 6.25 BTC
  # Blocks are found every 10 minutes on average
  blocks_per_day <- 144  # 24 hours * 6 blocks per hour
  network_hashrate <- difficulty * 2^32 / 600  # Hash/second
  
  daily_btc <- (hashrate_ths * 1e12 * blocks_per_day * 6.25) / network_hashrate
  return(daily_btc)
}

#' Calculate energy consumption and costs
#' @param power_watts Power consumption in watts
#' @param electricity_cost Cost per kWh
#' @return Daily energy cost in USD
calculate_energy_costs <- function(power_watts, electricity_cost) {
  kwh_per_day <- power_watts * 24 / 1000
  daily_cost <- kwh_per_day * electricity_cost
  return(daily_cost)
}