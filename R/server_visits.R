# R/server_visits.R

# Helper: Format Delta as HTML badge
# Helper: Format Delta as HTML badge
format_delta_html <- function(delta) {
  if (is.na(delta)) {
    return(
      shiny::span("New", style = "font-size: 0.75em; color: #000000; font-weight: bold; text-transform: uppercase;")
    )
  }
  
  pct <- round(delta * 100, 1)
  
  if (delta > 0) {
    icon_name <- "arrow-up"
    text <- paste0("+", pct, "%")
  } else if (delta < 0) {
    icon_name <- "arrow-down"
    text <- paste0(pct, "%")
  } else {
    icon_name <- "minus"
    text <- "0%"
  }
  
  shiny::span(shiny::icon(icon_name), text, style = "font-size: 0.75em; color: #000000; font-weight: bold;")
}

# ==============================================================================
# HELPER: FORMAT DURATION
# ==============================================================================
format_visit_duration_ms <- function(ms) {
  if (is.null(ms) || length(ms) == 0 || is.na(ms) || ms == 0) {
    return("0s")
  }
  
  total_seconds <- round(ms / 1000)
  
  if (total_seconds < 60) {
    return(paste0(total_seconds, "s"))
  }
  
  minutes <- floor(total_seconds / 60)
  seconds <- total_seconds %% 60
  
  if (minutes < 60) {
    if (seconds > 0) {
      return(paste0(minutes, "m ", seconds, "s"))
    } else {
      return(paste0(minutes, "m"))
    }
  }
  
  hours <- floor(minutes / 60)
  minutes <- minutes %% 60
  
  paste0(hours, "h ", minutes, "m")
}


# ==============================================================================
# SERVER MODULE: VISITS
# ==============================================================================
server_visits <- function(input, output, session) {
  # ----------------------------------------------------------------------------
  # Load visit summary data for this tab
  # ----------------------------------------------------------------------------
  visit_summary_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_visit_summary;")
  visit_page_views_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_visit_page_views;")
  
  # ----------------------------------------------------------------------------
  # Track active visits subtab
  # ----------------------------------------------------------------------------
  active_visits_subtab <- shiny::reactive({
    if (is.null(input$visits_subtabs)) {
      "visits_overview"
    } else {
      input$visits_subtabs
    }
  })
  
  # ----------------------------------------------------------------------------
  # Centralised current filter state
  # ----------------------------------------------------------------------------
  current_visit_filters <- shiny::reactive({
    list(
      date_start = input$visits_date_range[1],
      date_end = input$visits_date_range[2],
      date_preset = input$visits_date_preset,
      trend_granularity = input$visits_trend_granularity,
      country = input$filter_country,
      region = input$filter_region,
      user_type = input$filter_user_type,
      account_manager = input$filter_account_mgr,
      visits_subtab = input$visits_subtabs
    )
  })
  
  # ----------------------------------------------------------------------------
  # Helper: Calculate KPIs from a dataframe
  # ----------------------------------------------------------------------------
  calculate_kpis <- function(df) {
    total_visits <- nrow(df)
    unique_visitors <- dplyr::n_distinct(df$user_id, na.rm = TRUE)
    
    avg_visits_per_visitor <- if (unique_visitors > 0) {
      total_visits / unique_visitors
    } else {
      0
    }
    
    avg_pages_per_visit <- if (total_visits > 0) {
      mean(df$page_view_count, na.rm = TRUE)
    } else {
      0
    }
    
    median_visit_duration_ms <- if (total_visits > 0) {
      median(df$total_time_spent_ms, na.rm = TRUE)
    } else {
      0
    }
    
    list(
      total_visits = total_visits,
      unique_visitors = unique_visitors,
      avg_visits_per_visitor = avg_visits_per_visitor,
      avg_pages_per_visit = avg_pages_per_visit,
      median_visit_duration_ms = median_visit_duration_ms
    )
  }
  
  # ----------------------------------------------------------------------------
  # Reactive: Data filtered by GLOBAL filters only (no dates yet)
  # ----------------------------------------------------------------------------
  global_filtered_data <- shiny::reactive({
    shiny::req(visit_summary_data)
    
    df <- visit_summary_data
    
    # Ensure types are correct
    df$visit_date <- as.Date(df$visit_date)
    df$page_view_count <- as.numeric(df$page_view_count)
    df$total_time_spent_ms <- as.numeric(df$total_time_spent_ms)
    
    # Helper to decide whether a global filter is active
    filter_is_active <- function(x) {
      !is.null(x) && nzchar(x) && x != "All"
    }
    
    # Apply global filters
    if (filter_is_active(input$filter_country)) {
      df <- df %>% dplyr::filter(country_name == input$filter_country)
    }
    
    if (filter_is_active(input$filter_region)) {
      df <- df %>% dplyr::filter(region_name == input$filter_region)
    }
    
    if (filter_is_active(input$filter_user_type)) {
      df <- df %>% dplyr::filter(user_type == input$filter_user_type)
    }
    
    if (filter_is_active(input$filter_account_mgr)) {
      df <- df %>% dplyr::filter(account_manager_name == input$filter_account_mgr)
    }
    
    df
  })
  
  # ----------------------------------------------------------------------------
  # Reactive: Current Period Data
  # ----------------------------------------------------------------------------
  filtered_visit_summary <- shiny::reactive({
    df <- global_filtered_data()
    
    # Get selected dates
    start_date <- as.Date(input$visits_date_range[1])
    end_date <- as.Date(input$visits_date_range[2])
    
    shiny::req(start_date, end_date)
    
    # Clamp to earliest tracking date
    start_date <- max(start_date, VISITS_EARLIEST_DATE)
    
    # Validate date range
    shiny::req(start_date <= end_date)
    
    # Date filter
    df %>%
      dplyr::filter(visit_date >= start_date, visit_date <= end_date)
  })
  
  # ----------------------------------------------------------------------------
  # KPI calculations (Current Period)
  # ----------------------------------------------------------------------------
  visit_kpis <- shiny::reactive({
    calculate_kpis(filtered_visit_summary())
  })
  
  # ----------------------------------------------------------------------------
  # Reactive: Previous Period KPIs
  # ----------------------------------------------------------------------------
  previous_visit_kpis <- shiny::reactive({
    # Get current selected dates
    start_date <- as.Date(input$visits_date_range[1])
    end_date <- as.Date(input$visits_date_range[2])
    
    shiny::req(start_date, end_date)
    
    # Calculate duration of current period (in days)
    period_length <- as.numeric(end_date - start_date) + 1
    
    # Define previous period range
    prev_end <- start_date - 1
    prev_start <- start_date - period_length
    
    # Filter data for previous period
    # Note: We don't clamp prev_start to VISITS_EARLIEST_DATE here because
    # we want to know if there was actually data in that specific previous window.
    # If the window is before tracking started, the filter will naturally return 0 rows.
    
    df_prev <- global_filtered_data() %>%
      dplyr::filter(visit_date >= prev_start, visit_date <= prev_end)
    
    calculate_kpis(df_prev)
  })
  
  # ----------------------------------------------------------------------------
  # Reactive: KPI Deltas (Percentage Change)
  # ----------------------------------------------------------------------------
  kpi_deltas <- shiny::reactive({
    curr <- visit_kpis()
    prev <- previous_visit_kpis()
    
    calc_delta <- function(current, previous) {
      if (is.null(previous) || previous == 0) {
        return(NA) # Cannot calculate % change from 0
      }
      (current - previous) / previous
    }
    
    list(
      total_visits = calc_delta(curr$total_visits, prev$total_visits),
      unique_visitors = calc_delta(curr$unique_visitors, prev$unique_visitors),
      avg_visits_per_visitor = calc_delta(
        curr$avg_visits_per_visitor,
        prev$avg_visits_per_visitor
      ),
      avg_pages_per_visit = calc_delta(curr$avg_pages_per_visit, prev$avg_pages_per_visit),
      median_visit_duration_ms = calc_delta(
        curr$median_visit_duration_ms,
        prev$median_visit_duration_ms
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # Insight Summary Text Generation
  # ----------------------------------------------------------------------------
  visit_insights_text <- shiny::reactive({
    df <- filtered_visit_summary()
    kpis <- visit_kpis()
    
    shiny::req(nrow(df) > 0)
    
    insights <- character(0)
    
    # 1. Volume
    total_v <- format(kpis$total_visits, big.mark = ",")
    unique_u <- format(kpis$unique_visitors, big.mark = ",")
    insights <- c(
      insights,
      paste0(
        "There were <b>",
        total_v,
        " visits</b> from <b>",
        unique_u,
        " unique users</b> in the selected period."
      )
    )
    
    # 2. Engagement
    avg_dur <- format_visit_duration_ms(kpis$median_visit_duration_ms)
    avg_pages <- sprintf("%.1f", kpis$avg_pages_per_visit)
    insights <- c(
      insights,
      paste0(
        "Average engagement was <b>",
        avg_pages,
        " pages</b> per visit, with a median duration of <b>",
        avg_dur,
        "</b>."
      )
    )
    
    
    # 3. Top Country
    top_country_df <- df %>%
      dplyr::count(country_name, name = "n") %>%
      dplyr::slice_max(n, n = 1, with_ties = FALSE)
    
    if (nrow(top_country_df) > 0 &&
        !is.na(top_country_df$country_name[1])) {
      insights <- c(
        insights,
        paste0(
          "The most active country was <b>",
          top_country_df$country_name[1],
          "</b>."
        )
      )
    }
    
    # 4. Top Language
    top_lang_df <- df %>%
      dplyr::count(language, name = "n") %>%
      dplyr::slice_max(n, n = 1, with_ties = FALSE)
    
    if (nrow(top_lang_df) > 0 && !is.na(top_lang_df$language[1])) {
      insights <- c(
        insights,
        paste0(
          "The most used language was <b>",
          top_lang_df$language[1],
          "</b>."
        )
      )
    }
    
    # 5. Engagement quality
    engagement_df <- df %>%
      dplyr::mutate(
        engagement = dplyr::case_when(
          page_view_count == 1 ~ "Bounce",
          page_view_count >= 4 & total_time_spent_ms >= 180000 ~ "Deep Dive",
          TRUE ~ "Browse"
        )
      ) %>%
      dplyr::count(engagement, name = "n")
    
    total_for_pct <- sum(engagement_df$n)
    
    if (total_for_pct > 0) {
      deep_n <- engagement_df$n[engagement_df$engagement == "Deep Dive"]
      deep_n <- if (length(deep_n) == 0) 0 else deep_n
      deep_pct <- round(deep_n / total_for_pct * 100)
      
      bounce_n <- engagement_df$n[engagement_df$engagement == "Bounce"]
      bounce_n <- if (length(bounce_n) == 0) 0 else bounce_n
      bounce_pct <- round(bounce_n / total_for_pct * 100)
      
      insights <- c(insights, paste0(
        "<b>", deep_pct, "%</b> of visits were deep dives, while <b>",
        bounce_pct, "%</b> were single-page bounces."
      ))
    }
    
    # 6. Content focus
    page_df <- filtered_page_views()
    if (nrow(page_df) > 0) {
      content_count <- sum(page_df$is_content == TRUE, na.rm = TRUE)
      content_pct <- round(content_count / nrow(page_df) * 100)
      
      insights <- c(insights, paste0(
        "<b>", content_pct, "%</b> of page views were focused on course content."
      ))
    }
    
    # 7. Context note (addressing your caveat)
    insights <- c(insights,
                  "<i>Note: Detailed visit tracking began on August 24, 2026.</i>")
    
    # Combine into an HTML list
    paste0(
      "<ul style='margin-bottom: 0; padding-left: 20px;'><li>",
      paste(insights, collapse = "</li><li>"),
      "</li></ul>"
    )
  })
  
  output$visits_insight_summary <- shiny::renderUI({
    shiny::HTML(visit_insights_text())
  })
  
  # ----------------------------------------------------------------------------
  # KPI outputs (with Deltas)
  # ----------------------------------------------------------------------------
  output$visits_kpi_total_visits <- shiny::renderUI({
    val <- format(visit_kpis()$total_visits, big.mark = ",")
    delta <- kpi_deltas()$total_visits
    shiny::div(val, shiny::br(), format_delta_html(delta))
  })
  
  output$visits_kpi_unique_visitors <- shiny::renderUI({
    val <- format(visit_kpis()$unique_visitors, big.mark = ",")
    delta <- kpi_deltas()$unique_visitors
    shiny::div(val, shiny::br(), format_delta_html(delta))
  })
  
  output$visits_kpi_visits_per_visitor <- shiny::renderUI({
    val <- sprintf("%.1f", visit_kpis()$avg_visits_per_visitor)
    delta <- kpi_deltas()$avg_visits_per_visitor
    shiny::div(val, shiny::br(), format_delta_html(delta))
  })
  
  output$visits_kpi_pages_per_visit <- shiny::renderUI({
    val <- sprintf("%.1f", visit_kpis()$avg_pages_per_visit)
    delta <- kpi_deltas()$avg_pages_per_visit
    shiny::div(val, shiny::br(), format_delta_html(delta))
  })
  
  output$visits_kpi_median_duration <- shiny::renderUI({
    val <- format_visit_duration_ms(visit_kpis()$median_visit_duration_ms)
    delta <- kpi_deltas()$median_visit_duration_ms
    shiny::div(val, shiny::br(), format_delta_html(delta))
  })
  
  # ----------------------------------------------------------------------------
  # Quick range observer: update date range and granularity
  # ----------------------------------------------------------------------------
  shiny::observeEvent(input$visits_date_preset, {
    shiny::req(input$visits_date_preset)
    
    end_date <- Sys.Date()
    
    start_date <- switch(
      input$visits_date_preset,
      "all_dates" = VISITS_EARLIEST_DATE,
      "last_7" = end_date - 6,
      "last_30" = end_date - 29,
      "last_90" = end_date - 89,
      "custom" = NULL
    )
    
    granularity <- switch(
      input$visits_date_preset,
      "all_dates" = "monthly",
      "last_7" = "daily",
      "last_30" = "weekly",
      "last_90" = "weekly",
      "custom" = NULL
    )
    
    if (!is.null(start_date)) {
      start_date <- max(start_date, VISITS_EARLIEST_DATE)
      
      shiny::updateDateRangeInput(
        session = session,
        inputId = "visits_date_range",
        start = start_date,
        end = end_date
      )
    }
    
    if (!is.null(granularity)) {
      shiny::updateSelectInput(session = session,
                               inputId = "visits_trend_granularity",
                               selected = granularity)
    }
  }, ignoreInit = TRUE)
  
  # ----------------------------------------------------------------------------
  # Real Usage Trend chart
  # ----------------------------------------------------------------------------
  output$visits_usage_trend_plot <- plotly::renderPlotly({
    # Get inputs
    granularity <- input$visits_trend_granularity
    start_date <- input$visits_date_range[1]
    end_date <- input$visits_date_range[2]
    
    shiny::req(start_date, end_date, granularity)
    
    # Clean dates
    start_date <- max(as.Date(start_date), VISITS_EARLIEST_DATE)
    end_date <- as.Date(end_date)
    
    shiny::req(start_date <= end_date)
    
    # Get filtered visit-level data
    df <- filtered_visit_summary()
    df$visit_date <- as.Date(df$visit_date)
    
    # Assign aggregation period and create a full period grid
    if (granularity == "daily") {
      df$period <- df$visit_date
      
      grid <- data.frame(period = seq.Date(start_date, end_date, by = "day"))
      
    } else if (granularity == "weekly") {
      # Week starts Monday
      df$period <- df$visit_date - (as.integer(format(df$visit_date, "%u")) - 1)
      
      grid_start <- start_date - (as.integer(format(start_date, "%u")) - 1)
      grid_end <- end_date - (as.integer(format(end_date, "%u")) - 1)
      
      grid <- data.frame(period = seq.Date(grid_start, grid_end, by = "week"))
      
    } else {
      # Month starts on the 1st
      df$period <- as.Date(format(df$visit_date, "%Y-%m-01"))
      
      grid_start <- as.Date(format(start_date, "%Y-%m-01"))
      grid_end <- as.Date(format(end_date, "%Y-%m-01"))
      
      grid <- data.frame(period = seq.Date(grid_start, grid_end, by = "month"))
    }
    
    # Count visits per period
    summary_data <- df %>%
      dplyr::group_by(period) %>%
      dplyr::summarise(visits = dplyr::n(), .groups = "drop")
    
    # Join onto full grid so missing periods become 0
    plot_data <- grid %>%
      dplyr::left_join(summary_data, by = "period") %>%
      dplyr::mutate(visits = dplyr::coalesce(visits, 0L)) %>%
      dplyr::arrange(period)
    
    # Ensure period is Date
    plot_data$period <- as.Date(plot_data$period)
    
    # Choose x-axis date labels by granularity
    x_date_labels <- switch(
      granularity,
      "daily" = "%d %b",
      "weekly" = "%d %b",
      "monthly" = "%b %Y"
    )
    
    # Build chart using ggplot2 for consistency with other tabs
    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = period, y = visits)) +
      ggplot2::geom_line(color = "#0d6efd",
                         linewidth = 1,
                         group = 1) +
      ggplot2::geom_point(color = "#0d6efd", size = 2.5) +
      ggplot2::scale_x_date(date_labels = x_date_labels, breaks = if (granularity %in% c("weekly", "monthly")) {
        unique(plot_data$period)
      } else {
        ggplot2::waiver()
      }) +
      ggplot2::scale_y_continuous(
        breaks = function(limits) {
          breaks <- pretty(limits)
          int_breaks <- unique(round(breaks))
          if (length(int_breaks) < 2) {
            int_breaks <- seq(floor(limits[1]), ceiling(limits[2]))
          }
          int_breaks
        }
      ) +
      ggplot2::labs(title = NULL, x = "Time Interval", y = "Visits") +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      )
    
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ----------------------------------------------------------------------------
  # Breakdown chart: Visits by Country
  # ----------------------------------------------------------------------------
  output$visits_country_chart <- plotly::renderPlotly({
    df <- filtered_visit_summary()
    shiny::req(nrow(df) > 0)
    
    country_counts <- df %>%
      dplyr::count(country_name, name = "count") %>%
      dplyr::arrange(dplyr::desc(count)) %>%
      head(10)
    
    country_counts$country_name <- reorder(country_counts$country_name, country_counts$count)
    
    p <- ggplot2::ggplot(country_counts,
                         ggplot2::aes(x = country_name, y = count, fill = country_name)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Where are our visitors from?",
        x = "Country",
        y = "Visits",
        fill = "Country"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      )
    
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ----------------------------------------------------------------------------
  # Breakdown chart: Visits by User Type
  # ----------------------------------------------------------------------------
  output$visits_user_type_chart <- plotly::renderPlotly({
    df <- filtered_visit_summary()
    shiny::req(nrow(df) > 0)
    
    user_type_counts <- df %>%
      dplyr::count(user_type, name = "count") %>%
      dplyr::arrange(dplyr::desc(count)) %>%
      head(10)
    
    user_type_counts$user_type <- reorder(user_type_counts$user_type, user_type_counts$count)
    
    p <- ggplot2::ggplot(user_type_counts,
                         ggplot2::aes(x = user_type, y = count, fill = user_type)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Who is using the platform?",
        x = "User Type",
        y = "Visits",
        fill = "User Type"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      )
    
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ----------------------------------------------------------------------------
  # Breakdown chart: Visits by Language
  # ----------------------------------------------------------------------------
  output$visits_language_chart <- plotly::renderPlotly({
    df <- filtered_visit_summary()
    shiny::req(nrow(df) > 0)
    
    # Clean language values
    df$language <- ifelse(is.na(df$language) |
                            trimws(df$language) == "",
                          "Unknown",
                          df$language)
    
    language_counts <- df %>%
      dplyr::count(language, name = "count") %>%
      dplyr::arrange(dplyr::desc(count)) %>%
      head(10)
    
    language_counts$language <- reorder(language_counts$language, language_counts$count)
    
    p <- ggplot2::ggplot(language_counts,
                         ggplot2::aes(x = language, y = count, fill = language)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Are translated pages being used?",
        x = "Language",
        y = "Visits",
        fill = "Language"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
      )
    
    plotly::ggplotly(p) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  
  # ----------------------------------------------------------------------------
  # Engagement Quality: Bounce vs Deep Dive
  # ----------------------------------------------------------------------------
  output$visits_engagement_chart <- plotly::renderPlotly({
    df <- filtered_visit_summary()
    shiny::req(nrow(df) > 0)
    
    # Classify each visit into an engagement tier
    engagement_data <- df %>%
      dplyr::mutate(
        engagement = dplyr::case_when(
          page_view_count == 1 ~ "Bounce",
          page_view_count >= 4 &
            total_time_spent_ms >= 180000 ~ "Deep Dive",
          TRUE ~ "Browse"
        )
      ) %>%
      dplyr::mutate(engagement = factor(engagement, levels = c("Bounce", "Browse", "Deep Dive"))) %>%
      dplyr::count(engagement, name = "count", .drop = FALSE)
    
    # Donut chart
    plot_ly(
      data = engagement_data,
      labels = ~ engagement,
      values = ~ count,
      type = "pie",
      hole = 0.5,
      sort = FALSE,
      textinfo = "label+percent",
      textposition = "outside",
      textfont = list(color = "#000000", size = 13),
      marker = list(colors = c("#d9534f", "#f0ad4e", "#5cb85c")),
      hovertemplate = "%{label}: %{value} visits (%{percent})<extra></extra>"
    ) %>%
      plotly::layout(
        title = list(text = "<b>How engaged are visits?</b>", font = list(size = 18)),
        paper_bgcolor = "#e8e8e8",
        plot_bgcolor = "#e8e8e8",
        showlegend = FALSE,
        margin = list(
          l = 60,
          r = 60,
          t = 70,
          b = 40
        ),
        font = list(color = "#000000")
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
  
  # ----------------------------------------------------------------------------
  # Filtered page views (for content vs non-content analysis)
  # ----------------------------------------------------------------------------
  filtered_page_views <- shiny::reactive({
    shiny::req(visit_page_views_data)
    
    df <- visit_page_views_data
    df$visit_date <- as.Date(df$visit_date)
    
    # Date filter
    start_date <- as.Date(input$visits_date_range[1])
    end_date <- as.Date(input$visits_date_range[2])
    shiny::req(start_date, end_date)
    start_date <- max(start_date, VISITS_EARLIEST_DATE)
    shiny::req(start_date <= end_date)
    
    df <- df %>%
      dplyr::filter(visit_date >= start_date, visit_date <= end_date)
    
    # Global filters
    filter_is_active <- function(x)
      ! is.null(x) && nzchar(x) && x != "All"
    if (filter_is_active(input$filter_country)) {
      df <- df %>% dplyr::filter(country_name == input$filter_country)
    }
    if (filter_is_active(input$filter_region)) {
      df <- df %>% dplyr::filter(region_name == input$filter_region)
    }
    if (filter_is_active(input$filter_user_type)) {
      df <- df %>% dplyr::filter(user_type == input$filter_user_type)
    }
    if (filter_is_active(input$filter_account_mgr)) {
      df <- df %>% dplyr::filter(account_manager_name == input$filter_account_mgr)
    }
    
    df
  })
  
  # ----------------------------------------------------------------------------
  # Engagement Quality: Content vs Non-Content
  # ----------------------------------------------------------------------------
  output$visits_content_chart <- plotly::renderPlotly({
    df <- filtered_page_views()
    shiny::req(nrow(df) > 0)
    
    content_data <- df %>%
      dplyr::mutate(page_type = dplyr::case_when(is_content == TRUE ~ "Course Content", TRUE ~ "Non-Content")) %>%
      dplyr::mutate(page_type = factor(page_type, levels = c("Course Content", "Non-Content"))) %>%
      dplyr::count(page_type, name = "count", .drop = FALSE)
    
    plot_ly(
      data = content_data,
      labels = ~ page_type,
      values = ~ count,
      type = "pie",
      hole = 0.5,
      sort = FALSE,
      textinfo = "label+percent",
      textposition = "outside",
      textfont = list(color = "#000000", size = 13),
      marker = list(colors = c("#5cb85c", "#adb5bd")),
      hovertemplate = "%{label}: %{value} page views (%{percent})<extra></extra>"
    ) %>%
      plotly::layout(
        title = list(text = "<b>Is activity focused on course content?</b>", font = list(size = 18)),
        paper_bgcolor = "#e8e8e8",
        plot_bgcolor = "#e8e8e8",
        showlegend = FALSE,
        margin = list(
          l = 60,
          r = 60,
          t = 70,
          b = 40
        ),
        font = list(color = "#000000")
      ) %>%
      plotly::config(displayModeBar = FALSE)
  })
}
