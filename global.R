# Load necessary libraries
library(DBI)
library(RPostgres)
library(dotenv)
library(writexl)




# 1. Conditionally load the .env file
# If the file exists (on your local computer), load it.
# If it doesn't exist (on Posit Cloud), skip this step.
if (file.exists(".env")) {
  dotenv::load_dot_env()
}

VISITS_EARLIEST_DATE <- as.Date("2026-08-24")

# 2. Establish the database connection using Sys.getenv()
# This works perfectly in BOTH environments now!
#
# `con` is a connection POOL (not a single connection): each query borrows a
# connection and returns it, dead connections are detected and replaced, and
# the pool is closed when the app stops. DBI::dbGetQuery(con, ...) works on it
# exactly as before. If the database is unreachable this fails at startup.
con <- pool::dbPool(
  drv = RPostgres::Postgres(),
  dbname = Sys.getenv("DB_NAME", unset = "MISSING_DB_NAME"),
  host = Sys.getenv("DB_HOST", unset = "MISSING_HOST"),
  port = as.numeric(Sys.getenv("DB_PORT", unset = "5432")),
  user = Sys.getenv("DB_USER", unset = "MISSING_USER"),
  password = Sys.getenv("DB_PASSWORD", unset = "MISSING_PASSWORD"),
  minSize = 1,
  maxSize = 5, # per R process; Connect may run several processes
  idleTimeout = 60 # seconds before an unused extra connection is closed
)

# Close all pooled connections when the app shuts down
shiny::onStop(function() {
  pool::poolClose(con)
})

# ============================================================================
# APP PASSWORD (for login protection)
# ============================================================================
APP_PASSWORD <- Sys.getenv("APP_PASSWORD", unset = "")

# ============================================================================
# "REMEMBER ME" TOKENS
# ============================================================================
# The auth cookie holds "<expiry>.<HMAC-SHA256(expiry)>", signed with a server
# secret, so it cannot be forged from the browser. Set AUTH_SECRET in the
# environment for a dedicated secret; otherwise APP_PASSWORD is used as the key
# (so changing the password also invalidates all remembered logins).
AUTH_SECRET <- Sys.getenv("AUTH_SECRET", unset = "")
if (AUTH_SECRET == "") {
  AUTH_SECRET <- APP_PASSWORD
}

make_auth_token <- function(days = 7) {
  expiry <- format(floor(as.numeric(Sys.time()) + days * 86400), scientific = FALSE)
  paste0(expiry, ".", digest::hmac(AUTH_SECRET, expiry, "sha256"))
}

verify_auth_token <- function(token) {
  if (AUTH_SECRET == "") {
    return(FALSE)
  }
  if (!is.character(token) || length(token) != 1 || is.na(token)) {
    return(FALSE)
  }
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 2 || !grepl("^[0-9]+$", parts[1])) {
    return(FALSE)
  }
  if (as.numeric(parts[1]) < as.numeric(Sys.time())) {
    return(FALSE)
  }
  identical(parts[2], digest::hmac(AUTH_SECRET, parts[1], "sha256"))
}
