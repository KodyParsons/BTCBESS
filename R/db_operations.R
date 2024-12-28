# # 1. First sync the database with your CSV files
# sync_db()
# 
# # 2. Then connect to the database
# con <- db_connect()
# 
# # 3. Query both tables
# miners <- get_miners(con)
# storage <- get_storage_systems(con)
# 
# # 4. View the data
# miners
# storage
# 
# # 5. Clean up connection
# dbDisconnect(con)

# Database operations
library(DBI)
library(RSQLite)
library(readr)

# Central path configuration
DB_CONFIG <- list(
  db_path = "../data/btcbess.sqlite",
  miners_csv = "../data/miners.csv",
  storage_csv = "../data/storage.csv"
)

#' Sync database with CSV files
#' @param config List of file paths (optional, uses DB_CONFIG by default)
sync_db <- function(config = DB_CONFIG) {
  # Connect to database
  con <- dbConnect(RSQLite::SQLite(), config$db_path)
  
  # Read CSVs
  miners <- read_csv(config$miners_csv)
  storage <- read_csv(config$storage_csv)
  
  # Recreate tables with latest data
  dbWriteTable(con, "miners", miners, overwrite = TRUE)
  dbWriteTable(con, "storage_systems", storage, overwrite = TRUE)
  
  # Close connection
  dbDisconnect(con)
}

#' Connect to SQLite database
#' @param config List of file paths (optional, uses DB_CONFIG by default)
#' @return DBI connection object
db_connect <- function(config = DB_CONFIG) {
  con <- dbConnect(RSQLite::SQLite(), config$db_path)
  return(con)
}

#' Get all miners from database
#' @param con Database connection
#' @return Dataframe of miners
get_miners <- function(con) {
  dbGetQuery(con, "SELECT * FROM miners")
}

#' Get all storage systems from database
#' @param con Database connection
#' @return Dataframe of storage systems
get_storage_systems <- function(con) {
  dbGetQuery(con, "SELECT * FROM storage_systems")
}

