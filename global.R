# Load necessary libraries
library(DBI)
library(RPostgres)
library(dotenv)



# 1. Conditionally load the .env file
# If the file exists (on your local computer), load it.
# If it doesn't exist (on Posit Cloud), skip this step.
if (file.exists(".env")) {
  dotenv::load_dot_env()
}

# 2. Establish the database connection using Sys.getenv()
# This works perfectly in BOTH environments now!
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
APP_PASSWORD <- Sys.getenv("APP_PASSWORD", unset = "")
if (APP_PASSWORD == "") {
  # Fallback: read from project .Renviron manually
  renviron_path <- file.path(getwd(), ".Renviron")
  if (file.exists(renviron_path)) {
    lines <- readLines(renviron_path)
    for (line in lines) {
      if (grepl("^APP_PASSWORD=", line)) {
        APP_PASSWORD <- sub("^APP_PASSWORD=", "", line)
        break
      }
    }
  }
}
