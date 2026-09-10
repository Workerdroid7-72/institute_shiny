# ============================================================================
# UI: Overview Tab
# ============================================================================

# ============================================================================
# HELPER: KPI value box with expand button
# ============================================================================
overview_kpi_card <- function(title, output_id, icon_name, theme, button_id) {
  htmltools::div(
    style = "position: relative; width: 100%; height: 100%;",
    bslib::value_box(
      title = title,
      value = shiny::uiOutput(output_id),
      showcase = shiny::icon(icon_name),
      theme = theme
    ),
    shiny::actionButton(
      button_id,
      label = NULL,
      icon = shiny::icon("expand"),
      class = "btn-sm",
      style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
    )
  )
}

ui_overview <- bslib::nav_panel(
  title = "Overview",
  icon = shiny::icon("gauge-high"),

  # --------------------------------------------------------------------------
  # KPI CARDS
  # --------------------------------------------------------------------------
  bslib::layout_column_wrap(
    width = 1 / 6,
    heights_equal = "all",

    overview_kpi_card(
      "Core 1 Completed",
      "metric_core1",
      "graduation-cap",
      "warning",
      "show_core1"
    ),

    overview_kpi_card(
      "Total Users",
      "metric_total_users",
      "users",
      "light",
      "show_total_users"
    ),

    overview_kpi_card(
      "New Users (30d)",
      "metric_new_users",
      "user-plus",
      "info",
      "show_new_users"
    ),

    overview_kpi_card(
      "Active Users (30d)",
      "metric_active_users",
      "user-check",
      "success",
      "show_active_users"
    ),

    overview_kpi_card(
      "Not Started",
      "metric_not_started",
      "hourglass-start",
      "dark",
      "show_not_started"
    ),

    overview_kpi_card(
      "Inactive Users (%)",
      "metric_inactive_pct",
      "user-slash",
      "danger",
      "show_inactive"
    )
  ),

  # --------------------------------------------------------------------------
  # KEY INSIGHTS
  # --------------------------------------------------------------------------
  shiny::div(
    class = "mt-3",
    bslib::card(
      style = "background-color: rgba(13, 110, 253, 0.07); border: 1px solid rgba(13, 110, 253, 0.3);",
      bslib::card_header(
        style = "background-color: rgba(13, 110, 253, 0.15); border-bottom: 1px solid rgba(13, 110, 253, 0.3);",
        shiny::div(
          shiny::icon(
            "lightbulb",
            style = "color: #ffc107; margin-right: 8px;"
          ),
          shiny::strong("Key Insights")
        )
      ),
      bslib::card_body(shiny::uiOutput("overview_insights"))
    )
  ),

  # --------------------------------------------------------------------------
  # HERO: Core 1 cumulative growth
  # --------------------------------------------------------------------------
  bslib::card(
    bslib::card_body(
      plotly::plotlyOutput("chart_core1_over_time", height = "450px")
    )
  ),

  # --------------------------------------------------------------------------
  # SECONDARY: Country adoption + New users
  # --------------------------------------------------------------------------
  bslib::layout_column_wrap(
    width = 1 / 2,

    bslib::card(
      bslib::card_body(
        shiny::radioButtons(
          inputId = "country_chart_metric",
          label = NULL,
          choices = c(
            "Total Users" = "total",
            "New Users (30d)" = "new30",
            "Core 1 Completions" = "core1"
          ),
          selected = "new30",
          inline = TRUE
        ),
        plotly::plotlyOutput("chart_country_adoption", height = "350px")
      )
    ),

    bslib::card(
      bslib::card_body(
        shiny::radioButtons(
          inputId = "new_users_chart_mode",
          label = NULL,
          choices = c("New Users" = "new", "Cumulative" = "cumulative"),
          selected = "cumulative",
          inline = TRUE
        ),
        plotly::plotlyOutput("chart_new_users_over_time", height = "350px")
      )
    )
  )
)
