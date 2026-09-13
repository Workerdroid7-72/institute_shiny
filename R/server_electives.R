# ============================================================================
# SERVER: Electives Tab
# ============================================================================

server_electives <- function(input, output, session) {
  # --------------------------------------------------------------------------
  # HELPER: extract the elective (course) name from a page path
  # e.g. "/logged/learning/elective/mastering-the-consultation-process/step"
  #      -> "Mastering The Consultation Process"
  # --------------------------------------------------------------------------
  extract_elective_name <- function(page_paths) {
    sapply(
      page_paths,
      function(p) {
        if (is.na(p) || p == "") {
          return("Unknown")
        }
        clean <- sub("^/?logged/", "", p)
        clean <- sub("^/", "", clean)
        parts <- strsplit(clean, "/")[[1]]
        idx <- which(parts == "elective")
        if (length(idx) > 0 && idx[1] + 1 <= length(parts)) {
          slug <- parts[idx[1] + 1]
          tools::toTitleCase(gsub("-", " ", slug))
        } else {
          "Unknown"
        }
      },
      USE.NAMES = FALSE
    )
  }

  # --------------------------------------------------------------------------
  # HELPER: format milliseconds as a readable duration
  # --------------------------------------------------------------------------
  format_duration <- function(ms) {
    if (is.na(ms)) {
      return("0s")
    }
    secs <- ms / 1000
    if (secs < 60) {
      paste0(round(secs), "s")
    } else {
      mins <- floor(secs / 60)
      rem <- round(secs %% 60)
      paste0(mins, "m ", rem, "s")
    }
  }

  # --------------------------------------------------------------------------
  # REACTIVE: elective page views, respecting the global sidebar filters
  # --------------------------------------------------------------------------
  elective_page_views <- shiny::reactive({
    df <- DBI::dbGetQuery(
      con,
      "
      SELECT * FROM vw_visit_page_views
      WHERE page_path LIKE '%learning/elective%'
    "
    )

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
    # Account-manager filter applied defensively (only if the column exists)
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
  # KPI OUTPUTS
  # --------------------------------------------------------------------------
  output$electives_total_views <- shiny::renderText({
    df <- elective_page_views()
    formatC(nrow(df), format = "d", big.mark = ",")
  })

  output$electives_unique_learners <- shiny::renderText({
    df <- elective_page_views()
    formatC(dplyr::n_distinct(df$user_id), format = "d", big.mark = ",")
  })

  output$electives_courses_accessed <- shiny::renderText({
    df <- elective_page_views()
    if (nrow(df) == 0) {
      return("0")
    }
    courses <- extract_elective_name(df$page_path)
    courses <- courses[courses != "Unknown"]
    formatC(length(unique(courses)), format = "d", big.mark = ",")
  })

  output$electives_median_time <- shiny::renderText({
    df <- elective_page_views()
    if (nrow(df) == 0 || all(is.na(df$time_spent_ms))) {
      return("0s")
    }
    format_duration(median(df$time_spent_ms, na.rm = TRUE))
  })

  # --------------------------------------------------------------------------
  # CHART: Elective Adoption (views per elective)
  # --------------------------------------------------------------------------
  output$electives_adoption_chart <- plotly::renderPlotly({
    df <- elective_page_views()
    shiny::req(nrow(df) > 0)

    df$elective_name <- extract_elective_name(df$page_path)
    df <- df %>% dplyr::filter(elective_name != "Unknown")
    shiny::req(nrow(df) > 0)

    adoption <- df %>%
      dplyr::group_by(elective_name) %>%
      dplyr::summarise(
        views = dplyr::n(),
        unique_learners = dplyr::n_distinct(user_id),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(views))

    adoption$elective_name <- factor(
      adoption$elective_name,
      levels = rev(adoption$elective_name)
    )

    p <- ggplot2::ggplot(
      adoption,
      ggplot2::aes(x = elective_name, y = views, fill = elective_name)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Elective Adoption",
        subtitle = "Total page views per elective",
        x = "Elective",
        y = "Views"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      ) +
      ggplot2::scale_y_continuous(
        breaks = function(limits) {
          breaks <- pretty(limits)
          int_breaks <- unique(round(breaks))
          if (length(int_breaks) < 2) {
            int_breaks <- seq(floor(limits[1]), ceiling(limits[2]))
          }
          int_breaks
        }
      )

    plotly::ggplotly(p)
  })

  # --------------------------------------------------------------------------
  # CHART: Who's Engaging (unique learners by selected dimension)
  # --------------------------------------------------------------------------
  output$electives_audience_chart <- plotly::renderPlotly({
    df <- elective_page_views()
    shiny::req(nrow(df) > 0)

    df$elective_name <- extract_elective_name(df$page_path)
    df <- df %>% dplyr::filter(elective_name != "Unknown")
    shiny::req(nrow(df) > 0)

    dimension <- input$electives_audience_dimension

    audience <- df %>%
      dplyr::group_by(group_label = .data[[dimension]]) %>%
      dplyr::summarise(
        unique_learners = dplyr::n_distinct(user_id),
        .groups = "drop"
      )

    # Clean up missing / empty labels
    audience$group_label <- ifelse(
      is.na(audience$group_label) | audience$group_label == "",
      "Unknown",
      audience$group_label
    )

    audience <- audience %>%
      dplyr::arrange(dplyr::desc(unique_learners))

    audience$group_label <- factor(
      audience$group_label,
      levels = rev(audience$group_label)
    )

    dimension_title <- switch(
      dimension,
      "user_type" = "User Type",
      "country_name" = "Country",
      "region_name" = "Region"
    )

    p <- ggplot2::ggplot(
      audience,
      ggplot2::aes(x = group_label, y = unique_learners, fill = group_label)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Who's Engaging with Electives",
        subtitle = paste0("Unique learners by ", tolower(dimension_title)),
        x = dimension_title,
        y = "Unique Learners"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      ) +
      ggplot2::scale_y_continuous(
        breaks = function(limits) {
          breaks <- pretty(limits)
          int_breaks <- unique(round(breaks))
          if (length(int_breaks) < 2) {
            int_breaks <- seq(floor(limits[1]), ceiling(limits[2]))
          }
          int_breaks
        }
      )

    plotly::ggplotly(p)
  })

  # --------------------------------------------------------------------------
  # CHART: Time on Electives (median time per view, per elective)
  # --------------------------------------------------------------------------
  output$electives_time_chart <- plotly::renderPlotly({
    df <- elective_page_views()
    shiny::req(nrow(df) > 0)

    df$elective_name <- extract_elective_name(df$page_path)
    df <- df %>% dplyr::filter(elective_name != "Unknown")
    shiny::req(nrow(df) > 0)

    time_by_elective <- df %>%
      dplyr::filter(!is.na(time_spent_ms)) %>%
      dplyr::group_by(elective_name) %>%
      dplyr::summarise(
        median_time_ms = median(time_spent_ms, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(median_time_ms))

    shiny::req(nrow(time_by_elective) > 0)

    time_by_elective$elective_name <- factor(
      time_by_elective$elective_name,
      levels = rev(time_by_elective$elective_name)
    )

    p <- ggplot2::ggplot(
      time_by_elective,
      ggplot2::aes(x = elective_name, y = median_time_ms, fill = elective_name)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Time on Electives",
        subtitle = "Median time spent per view",
        x = "Elective",
        y = "Median Time"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      ) +
      ggplot2::scale_y_continuous(
        labels = function(x) sapply(x, format_duration)
      )

    plotly::ggplotly(p)
  })

  # --------------------------------------------------------------------------
  # MODAL: open the audience drill-down table
  # --------------------------------------------------------------------------
  shiny::observeEvent(input$show_electives_audience, {
    shiny::req(input$show_electives_audience > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Elective Learners",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("electives_audience_table"),
        footer = shiny::modalButton("Close")
      )
    )
  })

  # --------------------------------------------------------------------------
  # TABLE: one row per engaged learner, with their details and engagement
  # --------------------------------------------------------------------------
  output$electives_audience_table <- DT::renderDT({
    df <- elective_page_views()
    shiny::req(nrow(df) > 0)

    df$elective_name <- extract_elective_name(df$page_path)
    df <- df %>% dplyr::filter(elective_name != "Unknown")
    shiny::req(nrow(df) > 0)

    has_name <- "full_name" %in% names(df)

    if (has_name) {
      user_summary <- df %>%
        dplyr::group_by(user_id, full_name) %>%
        dplyr::summarise(
          user_type = dplyr::first(user_type),
          country_name = dplyr::first(country_name),
          region_name = dplyr::first(region_name),
          account_manager_name = dplyr::first(account_manager_name),
          elective_views = dplyr::n(),
          electives_accessed = dplyr::n_distinct(elective_name),
          total_time_ms = sum(time_spent_ms, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      user_summary <- df %>%
        dplyr::group_by(user_id) %>%
        dplyr::summarise(
          user_type = dplyr::first(user_type),
          country_name = dplyr::first(country_name),
          region_name = dplyr::first(region_name),
          account_manager_name = dplyr::first(account_manager_name),
          elective_views = dplyr::n(),
          electives_accessed = dplyr::n_distinct(elective_name),
          total_time_ms = sum(time_spent_ms, na.rm = TRUE),
          .groups = "drop"
        )
    }

    # Most-engaged first
    user_summary <- user_summary %>%
      dplyr::arrange(dplyr::desc(elective_views))

    display <- data.frame(
      "Name" = if (has_name) {
        user_summary$full_name
      } else {
        as.character(user_summary$user_id)
      },
      "User Type" = user_summary$user_type,
      "Country" = user_summary$country_name,
      # "Region" = user_summary$region_name,
      "Account Manager" = user_summary$account_manager_name,
      "Elective Views" = user_summary$elective_views,
      "Electives Accessed" = user_summary$electives_accessed,
      "Total Time" = sapply(user_summary$total_time_ms, format_duration),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      display,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE,
      filter = "top"
    )
  })

  # --------------------------------------------------------------------------
  # REACTIVE: elective page views WITH lesson metadata (for the deep-dive)
  # --------------------------------------------------------------------------
  elective_detail_data <- shiny::reactive({
    df <- DBI::dbGetQuery(con, "SELECT * FROM vw_elective_page_views")

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
  # OBSERVER: populate the deep-dive elective dropdown
  # --------------------------------------------------------------------------
  shiny::observe({
    df <- elective_detail_data()
    electives <- sort(unique(df$elective_name[!is.na(df$elective_name)]))
    shiny::updateSelectInput(session, "deepdive_elective", choices = electives)
  })

  # --------------------------------------------------------------------------
  # TABLE: step-by-step breakdown for the selected elective
  # --------------------------------------------------------------------------
  output$deepdive_step_table <- DT::renderDT({
    df <- elective_detail_data()
    shiny::req(input$deepdive_elective)
    df <- df %>% dplyr::filter(elective_name == input$deepdive_elective)
    shiny::req(nrow(df) > 0)

    # Denominator for Reach %: everyone who viewed any page of this elective
    total_viewers <- dplyr::n_distinct(df$user_id)

    steps <- df %>%
      dplyr::group_by(lesson_num, lesson_name) %>%
      dplyr::summarise(
        visits = dplyr::n(),
        viewers = dplyr::n_distinct(user_id),
        median_time_ms = median(time_spent_ms, na.rm = TRUE),
        .groups = "drop"
      )

    # Step 0 = entry page; cast lesson_num (text) to integer for ordering
    steps$step <- ifelse(
      is.na(steps$lesson_num),
      0,
      suppressWarnings(as.integer(steps$lesson_num))
    )
    steps$lesson_label <- ifelse(
      is.na(steps$lesson_name),
      "(Entry page)",
      steps$lesson_name
    )
    steps$reach_pct <- if (total_viewers > 0) {
      round(steps$viewers / total_viewers * 100, 1)
    } else {
      0
    }
    steps$median_time <- sapply(steps$median_time_ms, format_duration)

    steps <- steps %>% dplyr::arrange(step)

    display <- data.frame(
      "Step" = steps$step,
      "Lesson" = steps$lesson_label,
      "Visits" = steps$visits,
      "Viewers" = steps$viewers,
      "Median Time" = steps$median_time,
      "Reach %" = steps$reach_pct,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    DT::datatable(
      display,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        order = list(list(0, "asc"))
      ),
      rownames = FALSE
    )
  })

  # --------------------------------------------------------------------------
  # CHART: step-by-step reach funnel for the selected elective
  # --------------------------------------------------------------------------
  output$deepdive_reach_chart <- plotly::renderPlotly({
    df <- elective_detail_data()
    shiny::req(input$deepdive_elective)
    df <- df %>% dplyr::filter(elective_name == input$deepdive_elective)
    shiny::req(nrow(df) > 0)

    total_viewers <- dplyr::n_distinct(df$user_id)

    steps <- df %>%
      dplyr::group_by(lesson_num, lesson_name) %>%
      dplyr::summarise(viewers = dplyr::n_distinct(user_id), .groups = "drop")

    steps$step <- ifelse(
      is.na(steps$lesson_num),
      0,
      suppressWarnings(as.integer(steps$lesson_num))
    )
    steps$reach_pct <- if (total_viewers > 0) {
      round(steps$viewers / total_viewers * 100, 1)
    } else {
      0
    }

    # Order by step so it reads as a funnel: Step 0 at the top, counting down
    steps <- steps %>% dplyr::arrange(step)
    steps$step_label <- paste0("Step ", steps$step)
    steps$step_label <- factor(steps$step_label, levels = rev(steps$step_label))

    p <- ggplot2::ggplot(
      steps,
      ggplot2::aes(x = step_label, y = reach_pct, fill = step_label)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Step-by-Step Reach",
        subtitle = "% of this elective's learners who reached each step",
        x = "Step",
        y = "Reach %"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      ) +
      ggplot2::scale_y_continuous(
        limits = c(0, 100),
        labels = function(x) paste0(x, "%")
      )

    plotly::ggplotly(p)
  })
}
