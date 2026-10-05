ui_leaderboard <- bslib::nav_panel(
  title = "Leaderboard",
  icon = shiny::icon("trophy"),

  # 1. Summary Statistics (Value Boxes)
  bslib::layout_column_wrap(
    width = 1 / 4,
    heights_equal = "all",
    bslib::value_box(
      "Total Points Awarded",
      shiny::textOutput("stat_total_points"),
      showcase = shiny::icon("star"),
      theme = "bg-primary" # Uses your app's primary blue (#0d6efd)
    ),
    bslib::value_box(
      "Active Users",
      shiny::textOutput("stat_active_users"),
      showcase = shiny::icon("users"),
      theme = "bg-success" # Green
    ),
    bslib::value_box(
      "Avg Points / User",
      shiny::textOutput("stat_avg_points"),
      showcase = shiny::icon("chart-line"),
      theme = "bg-info" # Cyan/Teal
    ),
    bslib::value_box(
      "Top Score",
      shiny::textOutput("stat_top_score"),
      showcase = shiny::icon("crown"),
      theme = "bg-warning" # Orange/Yellow
    )
  ),

  shiny::br(),

  # 2. Charts (Country vs User Type)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(
      bslib::card_header("Average Points by Country"),
      plotly::plotlyOutput("plot_country", height = "350px")
    ),
    bslib::card(
      bslib::card_header("Average Points by User Type"),
      plotly::plotlyOutput("plot_usertype", height = "350px")
    )
  ),

  shiny::br(),

  # 3. The Leaderboard Table
  bslib::card(
    bslib::card_header(
      shiny::div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        shiny::tags$span("User Rankings"),

        shiny::tags$div(
          style = "margin-bottom: 0;",
          shiny::radioButtons(
            "leaderboard_view",
            NULL,
            choices = c(
              "Total Overall" = "total",
              "Level 1" = "1",
              "Level 2" = "2",
              "Level 3" = "3"
            ),
            inline = TRUE
          )
        )
      )
    ),
    DT::DTOutput("leaderboard_table"),

    # Updated download button to match app style
    bslib::card_body(
      shiny::div(
        style = "padding: 15px 0;",
        shiny::downloadButton(
          "download_leaderboard",
          "Download Excel",
          class = "btn-success me-auto"
        )
      )
    )
  )
)
