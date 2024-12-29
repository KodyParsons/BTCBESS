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
  on.exit(dbDisconnect(con))
  
  # Read CSVs with specific column types
  miners <- read_csv(config$miners_csv, show_col_types = FALSE)
  storage <- read_csv(config$storage_csv, show_col_types = FALSE)
  
  # Recreate tables with latest data
  dbWriteTable(con, "miners", miners, overwrite = TRUE)
  dbWriteTable(con, "storage_systems", storage, overwrite = TRUE)
  
  cat("Database successfully synced with CSV files\n")
}

#' Connect to SQLite database
#' @param config List of file paths (optional, uses DB_CONFIG by default)
#' @return DBI connection object
db_connect <- function(config = DB_CONFIG) {
  dbConnect(RSQLite::SQLite(), config$db_path)
}

#' Get all miners from database
#' @param con Optional database connection (if not provided, will create and close one)
#' @return Dataframe of miners
get_miners <- function(con = NULL) {
  # If no connection provided, create one temporarily
  local_connection <- is.null(con)
  if (local_connection) {
    con <- db_connect()
    on.exit(dbDisconnect(con))
  }
  
  dbGetQuery(con, "SELECT * FROM miners")
}

#' Get all storage systems from database
#' @param con Optional database connection (if not provided, will create and close one)
#' @return Dataframe of storage systems
get_storage_systems <- function(con = NULL) {
  # If no connection provided, create one temporarily
  local_connection <- is.null(con)
  if (local_connection) {
    con <- db_connect()
    on.exit(dbDisconnect(con))
  }
  
  dbGetQuery(con, "SELECT * FROM storage_systems")
}

#' Test database connection and data retrieval
#' @param config List of file paths (optional, uses DB_CONFIG by default)
#' @return Logical indicating success
test_database <- function(config = DB_CONFIG) {
  tryCatch({
    con <- db_connect(config)
    on.exit(dbDisconnect(con))
    
    miners <- get_miners(con)
    storage <- get_storage_systems(con)
    
    cat("Database connection successful!\n")
    cat("Number of miners:", nrow(miners), "\n")
    cat("Number of storage systems:", nrow(storage), "\n")
    
    return(TRUE)
  }, error = function(e) {
    cat("Error connecting to database:", e$message, "\n")
    return(FALSE)
  })
}
