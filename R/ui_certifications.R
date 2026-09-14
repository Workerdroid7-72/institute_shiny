# ============================================================================
# UI: Certifications Tab
# ============================================================================

ui_certifications <- bslib::nav_panel(
  title = "Certifications",
  icon = shiny::icon("medal"),

  # --- KPI row ---
  bslib::layout_column_wrap(
    width = 1 / 3,
    heights_equal = "all",

    bslib::value_box(
      title = "Level 1 Certified",
      value = shiny::textOutput("cert_total"),
      showcase = shiny::icon("medal"),
      theme = "primary"
    ),

    bslib::value_box(
      title = "Core 1 → Certified",
      value = shiny::textOutput("cert_conversion"),
      showcase = shiny::icon("percent"),
      theme = "info"
    ),

    bslib::value_box(
      title = "Median Time to Certify",
      value = shiny::textOutput("cert_median_days"),
      showcase = shiny::icon("stopwatch"),
      theme = "warning"
    )
  ),

  # --- Charts row ---
  bslib::layout_column_wrap(
    width = 1 / 2,
    heights_equal = "all",

    bslib::card(
      bslib::card_body(
        plotly::plotlyOutput("cert_over_time_chart", height = "440px")
      )
    ),

    bslib::card(
      bslib::card_body(
        shiny::div(
          class = "mb-3",
          style = "max-width: 280px;",
          shiny::selectInput(
            inputId = "cert_dimension",
            label = "Break down by",
            choices = c(
              "Country" = "country_name",
              "User Type" = "user_type",
              "Account Manager" = "account_manager_name"
            ),
            selected = "country_name",
            selectize = FALSE
          )
        ),
        plotly::plotlyOutput("cert_by_dimension_chart", height = "380px")
      )
    )
  ),

  # --- Certified Learners Directory (full-width) ---
  bslib::card(
    bslib::card_header(
      shiny::h5(
        "Certified Learners Directory",
        class = "mb-0",
        style = "font-weight: 700;"
      )
    ),
    bslib::card_body(
      DT::DTOutput("cert_directory_table")
    )
  )
)
