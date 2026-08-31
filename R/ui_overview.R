# ============================================================================
# UI: Overview Tab
# ============================================================================

ui_overview <- bslib::nav_panel(
  title = "Overview",
  icon = shiny::icon("gauge-high"),
  
  bslib::layout_column_wrap(
    width = 1 / 6,
    heights_equal = "all",
    
    # --- Total Users ---
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::value_box(
        title = "Total Users",
        value = shiny::textOutput("metric_total_users"),
        showcase = shiny::icon("users"),
        theme = "primary"
      ),
      shiny::actionButton(
        "show_total_users",
        label = NULL,
        icon = shiny::icon("expand"),
        class = "btn-sm",
        style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      )
    ),
    
    # --- Active Users ---
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::value_box(
        title = "Active Users (30d)",
        value = shiny::textOutput("metric_active_users"),
        showcase = shiny::icon("user-check"),
        theme = "success"
      ),
      shiny::actionButton(
        "show_active_users",
        label = NULL,
        icon = shiny::icon("expand"),
        class = "btn-sm",
        style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      )
    ),
    
    # --- New Users ---
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::value_box(
        title = "New Users (30d)",
        value = shiny::textOutput("metric_new_users"),
        showcase = shiny::icon("user-plus"),
        theme = "info"
      ),
      shiny::actionButton(
        "show_new_users",
        label = NULL,
        icon = shiny::icon("expand"),
        class = "btn-sm",
        style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      )
    ),
    
    # --- Core 1 Completed ---
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::value_box(
        title = "Core 1 Completed",
        value = shiny::textOutput("metric_core1"),
        showcase = shiny::icon("graduation-cap"),
        theme = "warning"
      ),
      shiny::actionButton(
        "show_core1",
        label = NULL,
        icon = shiny::icon("expand"),
        class = "btn-sm",
        style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      )
    ),
    
    # --- Inactive Users ---
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::value_box(
        title = "Inactive Users (%)",
        value = shiny::textOutput("metric_inactive_pct"),
        showcase = shiny::icon("user-slash"),
        theme = "danger"
      ),
      shiny::actionButton(
        "show_inactive",
        label = NULL,
        icon = shiny::icon("expand"),
        class = "btn-sm",
        style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      )
    ),
    
    # --- Not Started ---
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::value_box(
        title = "Not Started",
        value = shiny::textOutput("metric_not_started"),
        showcase = shiny::icon("hourglass-start"),
        theme = "secondary"
      ),
      shiny::actionButton(
        "show_not_started",
        label = NULL,
        icon = shiny::icon("expand"),
        class = "btn-sm",
        style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      )
    )
  ),
  
  # Charts section
  bslib::layout_column_wrap(
    width = 1,
    heights_equal = "all",
    plotly::plotlyOutput("chart_new_users_over_time", height = "400px"),
    plotly::plotlyOutput("chart_core1_over_time", height = "400px")
  )
)