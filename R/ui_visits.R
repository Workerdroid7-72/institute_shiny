# R/ui_visits.R

if (!exists("VISITS_EARLIEST_DATE")) {
  VISITS_EARLIEST_DATE <- as.Date("2026-08-24")
}

# Helper: placeholder card for skeleton build
visits_placeholder_card <- function(title, subtitle = NULL) {
  bslib::card(
    bslib::card_header(shiny::div(
      shiny::h5(title, class = "mb-0", style = "font-weight: 700;"),
      if (!is.null(subtitle)) {
        shiny::div(class = "text-muted small mt-1", subtitle)
      }
    )),

    bslib::card_body(
      shiny::p("Placeholder section — this will be built out next."),
      shiny::tags$ul(
        shiny::tags$li("KPI cards"),
        shiny::tags$li("Charts"),
        shiny::tags$li("Tables")
      )
    )
  )
}

# Helper: simple KPI value box
visits_kpi_box <- function(
  title,
  value,
  icon_name = "chart-line",
  theme = "secondary"
) {
  bslib::value_box(
    title = title,
    value = value,
    showcase = shiny::icon(icon_name),
    theme = theme
  )
}


ui_visits <- bslib::nav_panel(
  title = "Visits",
  value = "visits",
  icon = shiny::icon("chart-line"),

  # --------------------------------------------------------------------------
  # Visit-level filter controls
  # --------------------------------------------------------------------------
  bslib::card(bslib::card_body(
    bslib::layout_column_wrap(
      width = 1 / 3,

      shiny::dateRangeInput(
        inputId = "visits_date_range",
        label = "Date range",
        start = max(Sys.Date() - 29, VISITS_EARLIEST_DATE),
        end = Sys.Date(),
        format = "yyyy-mm-dd"
      ),

      shiny::selectInput(
        inputId = "visits_date_preset",
        label = "Quick range",
        choices = c(
          "All dates" = "all_dates",
          "Last 7 days" = "last_7",
          "Last 30 days" = "last_30",
          "Last 90 days" = "last_90",
          "Custom" = "custom"
        ),
        selected = "last_30",
        selectize = FALSE
      )
    )
  )),

  bslib::navset_tab(
    # ==========================================================================
    # SUBTAB 1: OVERVIEW
    # ==========================================================================
    bslib::nav_panel(
      title = "Overview",
      value = "visits_overview",

      # ------------------------------------------------------------------------
      # 1) KPI STRIP
      # ------------------------------------------------------------------------
      shiny::div(
        class = "mt-3",
        bslib::layout_column_wrap(
          width = 1 / 5,
          visits_kpi_box(
            "Total Visits",
            shiny::uiOutput("visits_kpi_total_visits"),
            "chart-line",
            theme = "primary"
          ),
          visits_kpi_box(
            "Unique Visitors",
            shiny::uiOutput("visits_kpi_unique_visitors"),
            "users",
            theme = "success"
          ),
          visits_kpi_box(
            "Visits per Visitor",
            shiny::uiOutput("visits_kpi_visits_per_visitor"),
            "repeat",
            theme = "info"
          ),
          visits_kpi_box(
            "Pages per Visit",
            shiny::uiOutput("visits_kpi_pages_per_visit"),
            "layer-group",
            theme = "warning"
          ),
          visits_kpi_box(
            "Median Visit Duration",
            shiny::uiOutput("visits_kpi_median_duration"),
            "clock",
            theme = "danger"
          )
        )
      ),

      # ------------------------------------------------------------------------
      # INSIGHT SUMMARY CARD
      # ------------------------------------------------------------------------
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
          bslib::card_body(shiny::htmlOutput("visits_insight_summary"))
        )
      ),

      # ------------------------------------------------------------------------
      # ENGAGEMENT QUALITY
      # ------------------------------------------------------------------------
      shiny::div(
        class = "mt-4",
        shiny::h5(
          shiny::strong("Engagement Quality"),
          style = "margin-bottom: 2px;"
        ),
        shiny::p(
          "How meaningful are the visits to the platform?",
          style = "color: #adb5bd; font-size: 0.9em; margin-bottom: 12px;"
        ),
        bslib::layout_column_wrap(
          width = 1 / 2,
          bslib::card(bslib::card_body(
            plotly::plotlyOutput("visits_engagement_chart", height = "340px")
          )),
          bslib::card(bslib::card_body(
            plotly::plotlyOutput("visits_content_chart", height = "340px")
          ))
        )
      ),

      # ------------------------------------------------------------------------
      # 3) MAIN TREND AREA
      # ------------------------------------------------------------------------
      # ------------------------------------------------------------------------
      # 3) MAIN TREND AREA
      # ------------------------------------------------------------------------
      shiny::div(
        class = "mt-4",
        bslib::card(
          bslib::card_header(shiny::textOutput(
            "visits_trend_title",
            inline = TRUE
          )),
          bslib::card_body(
            shiny::div(
              class = "mb-3",
              shiny::radioButtons(
                inputId = "visits_trend_metric",
                label = "Metric",
                choices = c(
                  "Visits" = "visits",
                  "Unique Visitors" = "unique_visitors",
                  "Pages per Visit" = "pages_per_visit",
                  "Median Duration" = "median_duration"
                ),
                selected = "visits",
                inline = TRUE
              ),
              shiny::div(
                style = "max-width: 220px;",
                shiny::selectInput(
                  inputId = "visits_trend_granularity",
                  label = "Trend granularity",
                  choices = c(
                    "Daily" = "daily",
                    "Weekly" = "weekly",
                    "Monthly" = "monthly"
                  ),
                  selected = "daily",
                  selectize = FALSE
                )
              )
            ),
            plotly::plotlyOutput("visits_usage_trend_plot", height = "350px")
          )
        )
      ),

      # ------------------------------------------------------------------------
      # 4) BREAKDOWN AREA
      # ------------------------------------------------------------------------
      shiny::div(
        class = "mt-4",
        bslib::layout_column_wrap(
          width = 1 / 2,

          bslib::card(bslib::card_body(
            plotly::plotlyOutput("visits_country_chart", height = "320px")
          )),

          bslib::card(bslib::card_body(
            plotly::plotlyOutput("visits_user_type_chart", height = "320px")
          ))
        )
      ),

      shiny::div(
        class = "mt-4",
        bslib::card(
          bslib::card_body(
            plotly::plotlyOutput("visits_language_chart", height = "350px")
          )
        )
      )
    ),

    # ==========================================================================
    # SUBTAB 2: PAGES & CONTENT
    # ==========================================================================
    bslib::nav_panel(
      title = "Pages & Content",
      value = "visits_pages_content",

      # ------------------------------------------------------------------------
      # SUMMARY: VIEWS BY CATEGORY
      # ------------------------------------------------------------------------
      bslib::card(
        bslib::card_header(shiny::h5(
          "Views by Category",
          class = "mb-0",
          style = "font-weight: 700;"
        )),
        bslib::card_body(
          plotly::plotlyOutput(
            "visits_category_summary_chart",
            height = "350px"
          )
        )
      ),

      bslib::layout_column_wrap(
        width = 1 / 2,

        bslib::card(
          bslib::card_header(shiny::h5(
            "Top Pages",
            class = "mb-0",
            style = "font-weight: 700;"
          )),
          bslib::card_body(
            shiny::div(
              class = "mb-3",
              style = "max-width: 280px;",
              shiny::selectInput(
                inputId = "visits_pages_category",
                label = "Page category",
                choices = c(
                  "All categories" = "all",
                  "Core Learning" = "Core Learning",
                  "Elective Learning" = "Elective Learning",
                  "Assessment" = "Assessment",
                  "Completion" = "Completion",
                  "Functional / Admin" = "Functional / Admin",
                  "Admin pages" = "Admin pages",
                  "Regional Admin" = "Regional Admin",
                  "Owner Admin" = "Owner Admin",
                  "Record Detail" = "Record Detail"
                ),
                selected = "all",
                selectize = FALSE
              )
            ),
            plotly::plotlyOutput("visits_top_pages_chart", height = "400px")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::h5(
            "Entry Pages",
            class = "mb-0",
            style = "font-weight: 700;"
          )),
          bslib::card_body(
            plotly::plotlyOutput("visits_entry_pages_chart", height = "400px")
          )
        )
      ),

      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header(shiny::h5(
            "Exit Pages",
            class = "mb-0",
            style = "font-weight: 700;"
          )),
          bslib::card_body(
            shiny::div(
              class = "mb-3",
              style = "max-width: 280px;",
              shiny::selectInput(
                inputId = "visits_exit_category",
                label = "Page category",
                choices = c(
                  "All categories" = "all",
                  "Core Learning" = "Core Learning",
                  "Elective Learning" = "Elective Learning",
                  "Assessment" = "Assessment",
                  "Completion" = "Completion",
                  "Functional / Admin" = "Functional / Admin",
                  "Admin pages" = "Admin pages",
                  "Regional Admin" = "Regional Admin",
                  "Owner Admin" = "Owner Admin",
                  "Record Detail" = "Record Detail"
                ),
                selected = "all",
                selectize = FALSE
              )
            ),
            plotly::plotlyOutput("visits_exit_pages_chart", height = "400px")
          )
        ),
        bslib::card(
          bslib::card_header(shiny::h5(
            "Time on Page",
            class = "mb-0",
            style = "font-weight: 700;"
          )),
          bslib::card_body(
            shiny::div(
              class = "mb-3",
              style = "max-width: 280px;",
              shiny::selectInput(
                inputId = "visits_time_category",
                label = "Page category",
                choices = c(
                  "All categories" = "all",
                  "Core Learning" = "Core Learning",
                  "Elective Learning" = "Elective Learning",
                  "Assessment" = "Assessment",
                  "Completion" = "Completion",
                  "Functional / Admin" = "Functional / Admin",
                  "Admin pages" = "Admin pages",
                  "Regional Admin" = "Regional Admin",
                  "Owner Admin" = "Owner Admin",
                  "Record Detail" = "Record Detail"
                ),
                selected = "all",
                selectize = FALSE
              )
            ),
            plotly::plotlyOutput("visits_time_on_page_chart", height = "400px")
          )
        )
      )
    ),

    # ==========================================================================
    # SUBTAB 3: JOURNEYS & PATHS
    # ==========================================================================
    bslib::nav_panel(
      title = "Journeys & Paths",
      value = "visits_journeys_paths",

      bslib::layout_column_wrap(
        width = 1 / 2,
        visits_placeholder_card(
          "Common Paths",
          "Frequent page-to-page transitions"
        ),
        visits_placeholder_card(
          "Learning Flow",
          "Discovery → course → lesson → quiz"
        )
      ),

      bslib::layout_column_wrap(
        width = 1,
        visits_placeholder_card(
          "Sankey / Flow Diagram",
          "Interactive navigation paths"
        )
      )
    ),

    # ==========================================================================
    # SUBTAB 4: RETENTION & LOYALTY
    # ==========================================================================
    bslib::nav_panel(
      title = "Retention & Loyalty",
      value = "visits_retention_loyalty",

      bslib::layout_column_wrap(
        width = 1 / 3,
        visits_placeholder_card("Active Users", "DAU / WAU / MAU"),
        visits_placeholder_card(
          "Repeat Visits",
          "Users returning more than once"
        ),
        visits_placeholder_card("Retention", "Return behaviour over time")
      ),

      bslib::layout_column_wrap(
        width = 1,
        visits_placeholder_card(
          "Cohort Heatmap",
          "First-use cohorts and subsequent activity"
        )
      )
    ),

    # ==========================================================================
    # SUBTAB 5: DETAIL / EXPORT
    # ==========================================================================
    bslib::nav_panel(
      title = "Detail / Export",
      value = "visits_detail_export",

      bslib::layout_column_wrap(
        width = 1,
        visits_placeholder_card(
          "Visit Detail Table",
          "Filtered visit-level summary table"
        ),
        visits_placeholder_card(
          "Page Detail Table",
          "Filtered page-level summary table"
        )
      )
    )
  )
)
