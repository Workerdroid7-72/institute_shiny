# ============================================================================
# SERVER: Certifications Tab
# ============================================================================

# --------------------------------------------------------------------------
# HELPER: integer-only y-axis breaks (for count charts)
# --------------------------------------------------------------------------
int_breaks <- function(limits) {
  breaks <- pretty(limits)
  int_breaks <- unique(round(breaks))
  if (length(int_breaks) < 2) {
    int_breaks <- seq(floor(limits[1]), ceiling(limits[2]))
  }
  int_breaks
}

server_certifications <- function(input, output, session) {
  # --------------------------------------------------------------------------
  # REACTIVE: Filtered user data (mirrors Overview's filtered_user_data)
  # --------------------------------------------------------------------------
  cert_user_data <- shiny::reactive({
    raw_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_platform_users_flat;")
    filtered <- raw_data

    if (!is.null(input$filter_country) && input$filter_country != "All") {
      filtered <- dplyr::filter(filtered, country_name == input$filter_country)
    }

    if (!is.null(input$filter_region) && input$filter_region != "All") {
      filtered <- dplyr::filter(filtered, region_name == input$filter_region)
    }

    if (!is.null(input$filter_user_type) && input$filter_user_type != "All") {
      filtered <- dplyr::filter(filtered, user_type == input$filter_user_type)
    }

    if (
      !is.null(input$filter_account_mgr) && input$filter_account_mgr != "All"
    ) {
      if (input$filter_account_mgr == "None / Company Staff") {
        filtered <- dplyr::filter(
          filtered,
          is.na(account_manager_name) |
            account_manager_name == "None / Company Staff"
        )
      } else {
        filtered <- dplyr::filter(
          filtered,
          account_manager_name == input$filter_account_mgr
        )
      }
    }

    return(filtered)
  })

  # --------------------------------------------------------------------------
  # KPI: Total Level 1 Certified
  # --------------------------------------------------------------------------
  output$cert_total <- shiny::renderText({
    data <- cert_user_data()
    formatC(
      sum(data$level_1_certified, na.rm = TRUE),
      format = "d",
      big.mark = ","
    )
  })

  # --------------------------------------------------------------------------
  # KPI: Conversion rate (% of Core 1 completers who became certified)
  # --------------------------------------------------------------------------
  output$cert_conversion <- shiny::renderText({
    data <- cert_user_data()
    core1_total <- sum(data$core_1_completed, na.rm = TRUE)
    certified_total <- sum(data$level_1_certified, na.rm = TRUE)
    rate <- if (core1_total > 0) certified_total / core1_total * 100 else 0
    paste0(formatC(rate, format = "f", digits = 1), "%")
  })

  # --------------------------------------------------------------------------
  # KPI: Median time to certify (Core 1 completion -> certification)
  # --------------------------------------------------------------------------
  output$cert_median_days <- shiny::renderText({
    data <- cert_user_data()
    certified <- data %>%
      dplyr::filter(level_1_certified == TRUE)

    if (nrow(certified) == 0) {
      return("—")
    }

    cert_date <- as.Date(certified$level_1_certified_date)
    core_date <- as.Date(certified$core_1_completed_date)
    days <- as.numeric(cert_date - core_date)
    days <- days[!is.na(days)]

    if (length(days) == 0) {
      return("—")
    }

    paste0(formatC(round(median(days)), format = "d", big.mark = ","), " days")
  })

  # --------------------------------------------------------------------------
  # CHART: Certifications over time (cumulative, by month)
  # --------------------------------------------------------------------------
  output$cert_over_time_chart <- plotly::renderPlotly({
    data <- cert_user_data()
    data <- dplyr::filter(data, !is.na(level_1_certified_date))
    shiny::req(nrow(data) > 0)

    # Month each user became certified
    data <- data %>%
      dplyr::mutate(
        month = as.Date(lubridate::floor_date(level_1_certified_date, "month"))
      )

    start_month <- min(data$month)
    end_month <- as.Date(lubridate::floor_date(lubridate::today(), "month"))

    # Full month grid so the cumulative line is continuous (no gaps)
    month_grid <- data.frame(
      month = seq.Date(start_month, end_month, by = "month")
    )

    monthly <- data %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(certifications = dplyr::n(), .groups = "drop")

    plot_data <- month_grid %>%
      dplyr::left_join(monthly, by = "month") %>%
      dplyr::mutate(certifications = dplyr::coalesce(certifications, 0L)) %>%
      dplyr::arrange(month) %>%
      dplyr::mutate(cumulative = cumsum(certifications))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = month, y = cumulative)) +
      ggplot2::geom_area(fill = "#0d6efd", alpha = 0.25) +
      ggplot2::geom_line(color = "#0d6efd", linewidth = 1.2) +
      ggplot2::geom_point(color = "#0d6efd", size = 2) +
      ggplot2::labs(
        title = "Certifications Over Time",
        subtitle = "Cumulative number of users who are Level 1 Certified",
        x = "Month",
        y = "Cumulative Certifications"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 12),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA)
      ) +
      ggplot2::scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y"
      ) +
      ggplot2::scale_y_continuous(breaks = int_breaks)

    plotly::ggplotly(p)
  })

  # --------------------------------------------------------------------------
  # CHART: Certified learners by selected dimension
  # --------------------------------------------------------------------------
  output$cert_by_dimension_chart <- plotly::renderPlotly({
    data <- cert_user_data()
    certified <- data %>%
      dplyr::filter(level_1_certified == TRUE)
    shiny::req(nrow(certified) > 0)

    dimension <- input$cert_dimension

    dim_label <- switch(
      dimension,
      "country_name" = "Country",
      "region_name" = "Region",
      "user_type" = "User Type",
      "account_manager_name" = "Account Manager"
    )

    certified$dim_value <- certified[[dimension]]
    certified$dim_value <- ifelse(
      is.na(certified$dim_value) | certified$dim_value == "",
      "Unknown",
      certified$dim_value
    )

    breakdown <- certified %>%
      dplyr::count(dim_value, name = "certified_count") %>%
      dplyr::arrange(dplyr::desc(certified_count))

    breakdown$dim_value <- factor(
      breakdown$dim_value,
      levels = rev(breakdown$dim_value)
    )

    p <- ggplot2::ggplot(
      breakdown,
      ggplot2::aes(x = dim_value, y = certified_count, fill = dim_value)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = paste0("Certified Learners by ", dim_label),
        subtitle = "Number of Level 1 certified users",
        x = dim_label,
        y = "Certified Users"
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
      ggplot2::scale_y_continuous(breaks = int_breaks)

    plotly::ggplotly(p)
  })

  # --------------------------------------------------------------------------
  # TABLE: Certified Learners Directory
  # --------------------------------------------------------------------------
  output$cert_directory_table <- DT::renderDT({
    data <- cert_user_data()
    certified <- data %>%
      dplyr::filter(level_1_certified == TRUE)
    shiny::req(nrow(certified) > 0)

    certified$core_date <- as.Date(certified$core_1_completed_date)
    certified$cert_date <- as.Date(certified$level_1_certified_date)
    certified$days_between <- as.numeric(
      certified$cert_date - certified$core_date
    )

    # Most recently certified first
    certified <- certified %>% dplyr::arrange(dplyr::desc(cert_date))

    display <- data.frame(
      "Name" = certified$full_name,
      "User Type" = certified$user_type,
      "Country" = certified$country_name,
      "Account Manager" = certified$account_manager_name,
      "Core 1 Completed" = format(certified$core_date, "%d %b %Y"),
      "Certified" = format(certified$cert_date, "%d %b %Y"),
      "Days Between" = certified$days_between,
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
}
