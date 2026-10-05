# ============================================================================
# SERVER: Leaderboard Tab
# ============================================================================

server_leaderboard <- function(input, output, session) {
  # --------------------------------------------------------------------------
  # REACTIVE: Fetch data from the view and apply global sidebar filters
  # --------------------------------------------------------------------------
  leaderboard_data <- shiny::reactive({
    # 1. Fetch from database (Lazy loaded when tab is active)
    df <- DBI::dbGetQuery(con, "SELECT * FROM vw_leaderboard_user_points;")

    # 2. Apply filters (Matching your exact logic from server_electives.R)
    if (
      !is.null(input$filter_country) &&
        input$filter_country != "" &&
        input$filter_country != "All"
    ) {
      df <- dplyr::filter(df, country_name == input$filter_country)
    }
    if (
      !is.null(input$filter_region) &&
        input$filter_region != "" &&
        input$filter_region != "All"
    ) {
      df <- dplyr::filter(df, region_name == input$filter_region)
    }
    if (
      !is.null(input$filter_user_type) &&
        input$filter_user_type != "" &&
        input$filter_user_type != "All"
    ) {
      df <- dplyr::filter(df, user_type == input$filter_user_type)
    }
    if (
      !is.null(input$filter_account_mgr) &&
        input$filter_account_mgr != "" &&
        input$filter_account_mgr != "All"
    ) {
      if ("account_manager_name" %in% names(df)) {
        if (input$filter_account_mgr == "None / Company Staff") {
          df <- dplyr::filter(
            df,
            is.na(account_manager_name) |
              account_manager_name == "None / Company Staff"
          )
        } else {
          df <- dplyr::filter(
            df,
            account_manager_name == input$filter_account_mgr
          )
        }
      }
    }

    df
  })

  # --------------------------------------------------------------------------
  # REACTIVE: Count of ALL approved users (baseline population)
  # --------------------------------------------------------------------------
  total_user_count <- shiny::reactive({
    # Query the total number of approved users, respecting global filters
    df <- DBI::dbGetQuery(
      con,
      "
      SELECT 
          u.user_id,
          COALESCE(r.region_name, 'Unassigned Region') AS region_name,
          COALESCE(c.country_name, 'Unknown Country') AS country_name,
          COALESCE(jt.extra_info, 'Unknown Role') AS user_type,
          COALESCE((am.fname || ' ') || am.lname, 'None / Company Staff') AS account_manager_name
      FROM z_institute_users u
      LEFT JOIN z_institute_lookup_job_title jt ON u.job_id = jt.job_id
      LEFT JOIN z_institute_lookup_region r ON u.region_id = r.region_id
      LEFT JOIN z_institute_lookup_country c ON r.country_id = c.country_id
      LEFT JOIN z_institute_users am ON u.account_manager = am.user_id
      WHERE u.date_approved IS NOT NULL 
        AND u.password IS NOT NULL 
        AND u.date_registered >= '2026-03-01 00:00:00'
      "
    )

    # Apply the same filters as the leaderboard
    if (
      !is.null(input$filter_country) &&
        input$filter_country != "" &&
        input$filter_country != "All"
    ) {
      df <- dplyr::filter(df, country_name == input$filter_country)
    }
    if (
      !is.null(input$filter_region) &&
        input$filter_region != "" &&
        input$filter_region != "All"
    ) {
      df <- dplyr::filter(df, region_name == input$filter_region)
    }
    if (
      !is.null(input$filter_user_type) &&
        input$filter_user_type != "" &&
        input$filter_user_type != "All"
    ) {
      df <- dplyr::filter(df, user_type == input$filter_user_type)
    }
    if (
      !is.null(input$filter_account_mgr) &&
        input$filter_account_mgr != "" &&
        input$filter_account_mgr != "All"
    ) {
      if ("account_manager_name" %in% names(df)) {
        if (input$filter_account_mgr == "None / Company Staff") {
          df <- dplyr::filter(
            df,
            is.na(account_manager_name) |
              account_manager_name == "None / Company Staff"
          )
        } else {
          df <- dplyr::filter(
            df,
            account_manager_name == input$filter_account_mgr
          )
        }
      }
    }

    nrow(df)
  })

  # --------------------------------------------------------------------------
  # REACTIVE: Calculate GRAND TOTAL points per user (across all levels)
  # --------------------------------------------------------------------------
  leaderboard_total <- shiny::reactive({
    df <- leaderboard_data()

    df %>%
      dplyr::group_by(
        user_id,
        full_name,
        user_type,
        country_name,
        region_name,
        account_manager_name
      ) %>%
      dplyr::summarise(
        # Sum core points across all levels (assuming core doesn't rollover)
        total_core = sum(core_points, na.rm = TRUE),
        # Take the MAX of true_total_elective. Because it's only populated on
        # the highest level row, MAX() safely grabs the true cumulative total!
        total_elective = max(true_total_elective, na.rm = TRUE),
        grand_total = total_core + total_elective,
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(grand_total)) %>%
      dplyr::mutate(rank = dplyr::row_number())
  })

  # --------------------------------------------------------------------------
  # REACTIVE: Calculate points for a SPECIFIC level
  # --------------------------------------------------------------------------
  leaderboard_level <- shiny::reactive({
    shiny::req(input$leaderboard_view)
    lvl <- as.integer(input$leaderboard_view)
    df <- leaderboard_data()

    df %>%
      dplyr::filter(level_id == lvl) %>% # Only users who have started this level
      # For individual levels, we use the display_elective_points column
      dplyr::mutate(level_total = core_points + display_elective_points) %>%
      dplyr::select(
        user_id,
        full_name,
        user_type,
        country_name,
        core_points,
        display_elective_points,
        level_total
      ) %>%
      dplyr::arrange(dplyr::desc(level_total)) %>%
      dplyr::mutate(rank = dplyr::row_number())
  })

  # --------------------------------------------------------------------------
  # REACTIVE: The final table data based on the radio button selection
  # --------------------------------------------------------------------------
  current_leaderboard <- shiny::reactive({
    if (input$leaderboard_view == "total") {
      leaderboard_total() %>%
        dplyr::select(
          rank,
          full_name,
          user_type,
          country_name,
          total_core,
          total_elective,
          grand_total
        ) %>%
        dplyr::rename(
          `Core Points` = total_core,
          `Elective Points` = total_elective,
          `Total Points` = grand_total
        )
    } else {
      leaderboard_level() %>%
        dplyr::select(
          rank,
          full_name,
          user_type,
          country_name,
          core_points,
          display_elective_points,
          level_total
        ) %>%
        dplyr::rename(
          `Core Points` = core_points,
          `Elective Points` = display_elective_points,
          `Level Points` = level_total
        )
    }
  })

  # --------------------------------------------------------------------------
  # KPI OUTPUTS (Summary Stats)
  # --------------------------------------------------------------------------
  output$stat_total_points <- shiny::renderText({
    df <- current_leaderboard()
    pts_col <- if (input$leaderboard_view == "total") {
      "Total Points"
    } else {
      "Level Points"
    }
    formatC(sum(df[[pts_col]], na.rm = TRUE), format = "d", big.mark = ",")
  })

  output$stat_active_users <- shiny::renderText({
    formatC(total_user_count(), format = "d", big.mark = ",")
  })

  output$stat_avg_points <- shiny::renderText({
    df <- current_leaderboard()
    if (nrow(df) == 0) {
      return("0.0")
    }
    pts_col <- if (input$leaderboard_view == "total") {
      "Total Points"
    } else {
      "Level Points"
    }

    # Calculate mean and format to exactly 1 decimal place
    avg_val <- mean(df[[pts_col]], na.rm = TRUE)
    formatC(avg_val, format = "f", digits = 1, big.mark = ",")
  })

  output$stat_top_score <- shiny::renderText({
    df <- current_leaderboard()
    if (nrow(df) == 0) {
      return("0")
    }
    pts_col <- if (input$leaderboard_view == "total") {
      "Total Points"
    } else {
      "Level Points"
    }
    formatC(max(df[[pts_col]], na.rm = TRUE), format = "d", big.mark = ",")
  })

  # --------------------------------------------------------------------------
  # CHARTS (Always based on the Grand Total view for high-level comparison)
  # --------------------------------------------------------------------------
  output$plot_country <- plotly::renderPlotly({
    df <- leaderboard_total() %>%
      dplyr::group_by(country_name) %>%
      dplyr::summarise(
        avg_points = round(mean(grand_total, na.rm = TRUE), 1),
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(avg_points)) %>%
      utils::head(10)

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = stats::reorder(country_name, avg_points),
        y = avg_points,
        fill = country_name
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      # Position text at 85% of bar length (avoids hjust + coord_flip bug)
      ggplot2::geom_text(
        ggplot2::aes(y = avg_points * 0.9, label = paste0("n = ", n)),
        color = "white",
        fontface = "bold",
        size = 3.5
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Average Points by Country",
        x = "Country",
        y = "Avg Points"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(face = "bold")
      )

    plotly::ggplotly(p)
  })

  output$plot_usertype <- plotly::renderPlotly({
    df <- leaderboard_total() %>%
      dplyr::group_by(user_type) %>%
      dplyr::summarise(
        avg_points = round(mean(grand_total, na.rm = TRUE), 1),
        n = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(avg_points))

    p <- ggplot2::ggplot(
      df,
      ggplot2::aes(
        x = stats::reorder(user_type, avg_points),
        y = avg_points,
        fill = user_type
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      # Position text at 85% of bar length (avoids hjust + coord_flip bug)
      ggplot2::geom_text(
        ggplot2::aes(y = avg_points * 0.85, label = paste0("n = ", n)),
        color = "white",
        fontface = "bold",
        size = 3.5
      ) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Average Points by User Type",
        x = "User Type",
        y = "Avg Points"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        legend.position = "none",
        plot.title = ggplot2::element_text(face = "bold")
      )

    plotly::ggplotly(p)
  })

  # --------------------------------------------------------------------------
  # TABLE: The Interactive Leaderboard
  # --------------------------------------------------------------------------
  output$leaderboard_table <- DT::renderDT({
    df <- current_leaderboard()

    # Standardize column names for the final display
    names(df) <- c(
      "Rank",
      "User Name",
      "User Type",
      "Country",
      "Core Pts",
      "Elective Pts",
      "Total/Level Pts"
    )

    DT::datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 15,
        order = list(list(0, 'asc')), # Sort by Rank
        columnDefs = list(
          list(className = 'dt-center', targets = c(0, 4, 5, 6)),
          list(width = '40px', targets = 0)
        )
      )
    ) %>%
      DT::formatRound(
        columns = c("Core Pts", "Elective Pts", "Total/Level Pts"),
        digits = 0
      )
  })

  # --------------------------------------------------------------------------
  # DOWNLOAD: Export the current leaderboard view to Excel
  # --------------------------------------------------------------------------
  output$download_leaderboard <- shiny::downloadHandler(
    filename = function() {
      # Create a dynamic filename based on the current view
      view_label <- if (input$leaderboard_view == "total") {
        "Total"
      } else {
        paste0("Level_", input$leaderboard_view)
      }

      # Add timestamp to avoid overwriting
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")

      paste0("Leaderboard_", view_label, "_", timestamp, ".xlsx")
    },

    content = function(file) {
      # Get the current leaderboard data
      df <- current_leaderboard()

      # Write to Excel using writexl (lightweight, no Java required)
      writexl::write_xlsx(df, file)
    }
  )
}
