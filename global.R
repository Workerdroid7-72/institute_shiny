# Load necessary libraries
library(DBI)
library(RPostgres)
library(dotenv)
library(config)

# 1. Load the .env file into the system environment
# This reads the .env file and makes the variables available to R
dotenv::load_dot_env()

# 2. Establish the database connection using Sys.getenv()
# Sys.getenv() safely pulls the string from the environment. 
# We provide a default fallback (like "MISSING") just in case, to help with debugging.
con <- DBI::dbConnect(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME", unset = "MISSING_DB_NAME"),
  host = Sys.getenv("DB_HOST", unset = "MISSING_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT", unset = "5432")),
  user = Sys.getenv("DB_USER", unset = "MISSING_USER"),
  password = Sys.getenv("DB_PASSWORD", unset = "MISSING_PASSWORD")
)

# Optional: Test the connection on startup to catch errors early
if (inherits(con, "try-error")) {
  stop("Failed to connect to the database. Check your .env file.")
}

# ============================================================================
# APP PASSWORD (for login protection)
# ============================================================================
# For Posit Connect: set APP_PASSWORD as an environment variable in content settings
# For local dev: set it in your .Renviron file or hardcode temporarily
APP_PASSWORD <- Sys.getenv("APP_PASSWORD", unset = "changeme")