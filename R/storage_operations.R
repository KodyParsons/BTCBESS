# Storage system operations

#' Calculate storage system benefits
#' @param storage_data Storage system specifications
#' @param energy_cost Base energy cost
#' @param daily_energy_kwh Daily energy consumption
#' @return List of benefits including cost savings and revenue
calculate_storage_benefits <- function(storage_data, energy_cost, daily_energy_kwh) {
  # Assume we can shift some load to off-peak hours
  peak_hours <- 6  # Number of peak pricing hours
  peak_price_multiplier <- 1.5  # Peak prices are 50% higher
  
  # Calculate potential savings from load shifting
  peak_energy <- daily_energy_kwh * (peak_hours / 24)
  potential_savings <- peak_energy * energy_cost * (peak_price_multiplier - 1)
  
  # Account for round-trip efficiency losses
  actual_savings <- potential_savings * storage_data$roundtrip_efficiency
  
  # Calculate battery degradation
  cycles_per_year <- 365
  annual_degradation <- storage_data$degradation_rate_annual
  
  return(list(
    daily_savings = actual_savings,
    annual_degradation = annual_degradation,
    cycles_per_year = cycles_per_year
  ))
}