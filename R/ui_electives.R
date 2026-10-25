# ============================================================================
# UI: Electives Tab
# ============================================================================

# Helper for plain KPI cards (no button, but same wrapper structure for consistent sizing)
electives_kpi_card_plain <- function(title, output_id, icon_name, theme) {
  htmltools::div(
    style = "position: relative; width: 100%; height: 100%;",
    bslib::value_box(
      title = title,
      value = shiny::textOutput(output_id),
      showcase = shiny::icon(icon_name),
      theme = theme
    )
  )
}

# Helper function for Electives KPI cards with drill-down button
electives_kpi_card <- function(title, output_id, icon_name, theme, button_id) {
  htmltools::div(
    style = "position: relative; width: 100%; height: 100%;",
    bslib::value_box(
      title = title,
      value = shiny::textOutput(output_id), # Kept as textOutput to match your original
      showcase = shiny::icon(icon_name),
      theme = theme
    ),
    shiny::actionButton(
      button_id,
      label = NULL,
      icon = shiny::icon("magnifying-glass-chart"), # Updated icon
      class = "btn-sm",
      # Exact same styling as your overview tab for consistency
      style = "position: absolute; top: 8px; right: 8px; z-index: 100; background-color: transparent; border: none; color: #ffffff; text-shadow: 0px 1px 3px rgba(0, 0, 0, 0.5); font-size: 1.2rem; opacity: 0.9;"
    )
  )
}

ui_electives <- bslib::nav_panel(
  title = "Electives",
  icon = shiny::icon("book-open"),

  # --- KPI cards ---
  bslib::layout_column_wrap(
    width = 1 / 4,
    heights_equal = "all",

    # Total Elective Views (UPDATED)
    electives_kpi_card_plain(
      title = "Total Elective Views",
      output_id = "electives_total_views",
      icon_name = "eye",
      theme = "primary"
    ),
    # Unique Learners (UPDATED)
    electives_kpi_card(
      title = "Unique Learners",
      output_id = "electives_unique_learners",
      icon_name = "users",
      theme = "success",
      button_id = "btn_electives_unique_learners"
    ),
    # Electives Accessed (UPDATED)
    electives_kpi_card_plain(
      title = "Electives Accessed",
      output_id = "electives_courses_accessed",
      icon_name = "book-open",
      theme = "info"
    ),

    # Median Time per View (UPDATED)
    electives_kpi_card_plain(
      title = "Median Time per View",
      output_id = "electives_median_time",
      icon_name = "clock",
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

    # Total Completions (UPDATED)
    electives_kpi_card(
      title = "Total Completions",
      output_id = "electives_total_completions",
      icon_name = "check-circle",
      theme = "success",
      button_id = "btn_electives_total_completions"
    ),

    # Unique Completers (UPDATED)
    electives_kpi_card(
      title = "Unique Completers",
      output_id = "electives_unique_completers",
      icon_name = "award",
      theme = "info",
      button_id = "btn_electives_unique_completers"
    ),

     # Median Time to Complete (UPDATED)
    electives_kpi_card_plain(
      title = "Median Time to Complete",
      output_id = "electives_median_time_to_complete",
      icon_name = "stopwatch",
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
