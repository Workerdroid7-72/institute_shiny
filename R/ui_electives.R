# ============================================================================
# UI: Electives Tab
# ============================================================================

ui_electives <- bslib::nav_panel(
  title = "Electives",
  icon = shiny::icon("book-open"),

  # --- KPI cards ---
  bslib::layout_column_wrap(
    width = 1 / 4,
    heights_equal = "all",

    bslib::value_box(
      title = "Total Elective Views",
      value = shiny::textOutput("electives_total_views"),
      showcase = shiny::icon("eye"),
      theme = "primary"
    ),
    bslib::value_box(
      title = "Unique Learners",
      value = shiny::textOutput("electives_unique_learners"),
      showcase = shiny::icon("users"),
      theme = "success"
    ),
    bslib::value_box(
      title = "Electives Accessed",
      value = shiny::textOutput("electives_courses_accessed"),
      showcase = shiny::icon("book-open"),
      theme = "info"
    ),
    bslib::value_box(
      title = "Median Time per View",
      value = shiny::textOutput("electives_median_time"),
      showcase = shiny::icon("clock"),
      theme = "warning"
    )
  ),

  # --- Elective Adoption chart ---
  bslib::card(
    bslib::card_body(
      plotly::plotlyOutput("electives_adoption_chart", height = "420px")
    )
  ),

  # --- Who's Engaging + Time on Electives (side by side) ---
  bslib::layout_column_wrap(
    width = 1 / 2,

    # Who's Engaging (with drill-down button)
    htmltools::div(
      style = "position: relative; width: 100%; height: 100%;",
      bslib::card(
        bslib::card_body(
          shiny::div(
            class = "mb-3",
            style = "max-width: 280px;",
            shiny::selectInput(
              inputId = "electives_audience_dimension",
              label = "Break down by",
              choices = c(
                "User Type" = "user_type",
                "Country" = "country_name"
                # "Region" = "region_name"
              ),
              selected = "user_type",
              selectize = FALSE
            )
          ),
          plotly::plotlyOutput("electives_audience_chart", height = "380px")
        )
      ),
      shiny::actionButton(
        "show_electives_audience",
        label = "View Learners",
        icon = shiny::icon("users"),
        class = "btn-sm btn-primary",
        style = "position: absolute; top: 10px; right: 12px; z-index: 100;"
      )
    ),

    # Time on Electives
    bslib::card(
      bslib::card_body(
        plotly::plotlyOutput("electives_time_chart", height = "450px")
      )
    )
  ),

  # --- Elective Deep-Dive ---
  bslib::card(
    bslib::card_header(
      shiny::h5(
        "Elective Deep-Dive",
        class = "mb-0",
        style = "font-weight: 700;"
      )
    ),
    bslib::card_body(
      shiny::div(
        style = "max-width: 420px;",
        shiny::selectInput(
          inputId = "deepdive_elective",
          label = "Select an elective to examine",
          choices = NULL
        )
      ),
      plotly::plotlyOutput("deepdive_reach_chart", height = "380px"),
      DT::DTOutput("deepdive_step_table")
    )
  ),

  # --- Completion Metrics ---
  bslib::layout_column_wrap(
    width = 1 / 3,
    heights_equal = "all",

    bslib::value_box(
      title = "Total Completions",
      value = shiny::textOutput("electives_total_completions"),
      showcase = shiny::icon("check-circle"),
      theme = "success"
    ),
    bslib::value_box(
      title = "Unique Completers",
      value = shiny::textOutput("electives_unique_completers"),
      showcase = shiny::icon("award"),
      theme = "info"
    ),
    bslib::value_box(
      title = "Median Time to Complete",
      value = shiny::textOutput("electives_median_time_to_complete"),
      showcase = shiny::icon("stopwatch"),
      theme = "warning"
    )
  ),

  # --- Completions Chart ---
  bslib::card(
    bslib::card_body(
      plotly::plotlyOutput("electives_completions_chart", height = "420px")
    )
  ),

  # --- Who's Completing (Faceted by Country) ---
  bslib::card(
    bslib::card_body(
      shiny::div(
        style = "display: flex; gap: 20px; margin-bottom: 15px;",

        # Dropdown to select specific elective or "All Electives"
        shiny::div(
          style = "max-width: 320px;",
          shiny::selectInput(
            inputId = "completions_elective_filter",
            label = "Filter by Elective",
            choices = c("All Electives" = "all"),
            selected = "all",
            selectize = FALSE
          )
        ),
      ),
      plotly::plotlyOutput(
        "electives_completions_audience_chart",
        height = "800px"
      )
    )
  ),

  # --- Start vs Completion Comparison ---
  bslib::card(
    bslib::card_header(
      shiny::h5(
        "Start vs Completion Comparison",
        class = "mb-0",
        style = "font-weight: 700;"
      )
    ),
    bslib::card_body(
      DT::DTOutput("start_vs_complete_table")
    )
  ),

  # --- Completers Drill-Down ---
  bslib::card(
    bslib::card_header(
      shiny::h5(
        "Who Completed Each Elective",
        class = "mb-0",
        style = "font-weight: 700;"
      )
    ),
    bslib::card_body(
      shiny::div(
        style = "max-width: 420px;",
        shiny::selectInput(
          inputId = "completers_elective",
          label = "Select an elective to view completers",
          choices = NULL
        )
      ),
      DT::DTOutput("completers_table")
    ),
    # Use card_footer for the download button
    bslib::card_footer(
      shiny::downloadButton(
        "download_completers",
        "Download Excel",
        class = "btn-success me-auto"
      )
    )
  ),
)
