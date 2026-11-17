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

# ============================================================================
# SHARED VIEW CACHE
# ============================================================================
# Whole views are fetched once and shared by every session in this R process,
# then re-fetched only when older than `ttl` seconds (default 10 minutes) or
# after refresh_views(). Filtering happens in R on the cached data frame, so a
# filter change no longer costs a database round trip. R is single-threaded per
# process, so no locking is needed.
.view_cache <- new.env(parent = emptyenv())

get_view <- function(name, ttl = 600) {
  entry <- .view_cache[[name]]
  if (is.null(entry) || difftime(Sys.time(), entry$fetched, units = "secs") > ttl) {
    entry <- list(
      data = DBI::dbGetQuery(con, sprintf("SELECT * FROM %s;", name)),
      fetched = Sys.time()
    )
    assign(name, entry, envir = .view_cache)
  }
  entry$data
}

# Cache the result of an arbitrary fetch function under `key` (same store,
# TTL and refresh as get_view).
get_cached <- function(key, fetch, ttl = 600) {
  entry <- .view_cache[[key]]
  if (is.null(entry) || difftime(Sys.time(), entry$fetched, units = "secs") > ttl) {
    entry <- list(data = fetch(), fetched = Sys.time())
    assign(key, entry, envir = .view_cache)
  }
  entry$data
}

refresh_views <- function() {
  rm(list = ls(.view_cache), envir = .view_cache)
  invisible(NULL)
}

# Cached view, plus a dependency on this session's refresh counter so reactives
# re-run when the Refresh button is pressed.
view_data <- function(name, session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(session$userData$data_version)) {
    session$userData$data_version()
  }
  get_view(name)
}

# ============================================================================
# SIDEBAR FILTERS (country / region / user type / account manager)
# ============================================================================
apply_sidebar_filters <- function(
  df,
  input,
  skip_country = FALSE,
  blank_is_all = FALSE
) {
  # blank_is_all = TRUE treats the "" placeholder (before the dropdowns have
  # loaded) like "All"; a filter is also skipped if its column is not in `df`.
  is_active <- function(x) {
    !is.null(x) && x != "All" && !(blank_is_all && x == "")
  }
  if (!skip_country && is_active(input$filter_country) && "country_name" %in% names(df)) {
    df <- dplyr::filter(df, country_name == input$filter_country)
  }
  if (is_active(input$filter_region) && "region_name" %in% names(df)) {
    df <- dplyr::filter(df, region_name == input$filter_region)
  }
  if (is_active(input$filter_user_type) && "user_type" %in% names(df)) {
    df <- dplyr::filter(df, user_type == input$filter_user_type)
  }
  if (is_active(input$filter_account_mgr) && "account_manager_name" %in% names(df)) {
    if (input$filter_account_mgr == "None / Company Staff") {
      df <- dplyr::filter(
        df,
        is.na(account_manager_name) |
          account_manager_name == "None / Company Staff"
      )
    } else {
      df <- dplyr::filter(df, account_manager_name == input$filter_account_mgr)
    }
  }
  df
}

# Like view_data(), but for an arbitrary cached fetch (e.g. a custom query).
cached_query <- function(key, fetch, session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(session$userData$data_version)) {
    session$userData$data_version()
  }
  get_cached(key, fetch)
}
