# ============================================================================
# SERVER: Device Characteristics Tab
# ============================================================================

server_device_characteristics <- function(input, output, session) {
  
  # ============================================================================
  # REACTIVE: Device data with filters
  # ============================================================================
  device_data <- shiny::reactive({
    raw_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_device_characteristics;")
    filtered <- raw_data
    
    # Country Filter
    if (!is.null(input$filter_country) && input$filter_country != "All") {
      filtered <- dplyr::filter(filtered, country_name == input$filter_country)
    }
    
    # Region Filter
    if (!is.null(input$filter_region) && input$filter_region != "All") {
      filtered <- dplyr::filter(filtered, region_name == input$filter_region)
    }
    
    # User Type Filter
    if (!is.null(input$filter_user_type) && input$filter_user_type != "All") {
      filtered <- dplyr::filter(filtered, user_type == input$filter_user_type)
    }
    
    # Account Manager Filter
    if (!is.null(input$filter_account_mgr) && input$filter_account_mgr != "All") {
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
  
  # ============================================================================
  # METRICS
  # ============================================================================
  
  output$metric_total_visits <- shiny::renderText({
    data <- device_data()
    formatC(nrow(data), format = "d", big.mark = ",")
  })
  
  output$metric_mobile_pct <- shiny::renderText({
    data <- device_data()
    if (nrow(data) == 0) return("0%")
    mobile_count <- sum(data$device_category == "Mobile", na.rm = TRUE)
    pct <- (mobile_count / nrow(data)) * 100
    paste0(round(pct, 1), "%")
  })
  
  output$metric_tablet_pct <- shiny::renderText({
    data <- device_data()
    if (nrow(data) == 0) return("0%")
    tablet_count <- sum(data$device_category == "Tablet", na.rm = TRUE)
    pct <- (tablet_count / nrow(data)) * 100
    paste0(round(pct, 1), "%")
  })
  
  output$metric_desktop_pct <- shiny::renderText({
    data <- device_data()
    if (nrow(data) == 0) return("0%")
    desktop_count <- sum(data$device_category == "Desktop", na.rm = TRUE)
    pct <- (desktop_count / nrow(data)) * 100
    paste0(round(pct, 1), "%")
  })
  
  output$metric_unique_users <- shiny::renderText({
    data <- device_data()
    unique_count <- length(unique(data$user_id))
    formatC(unique_count, format = "d", big.mark = ",")
  })
  
  # ============================================================================
  # SECTION 2: Browser Analysis
  # ============================================================================
  
  browser_summary <- shiny::reactive({
    data <- device_data()
    data %>%
      dplyr::group_by(visit_browser) %>%
      dplyr::summarise(
        visits = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        pct = round((visits / sum(visits)) * 100, 1)
      ) %>%
      dplyr::arrange(dplyr::desc(visits))
  })
  
  output$table_browsers <- DT::renderDT({
    data <- browser_summary()
    DT::datatable(
      data,
      colnames = c("Browser" = "visit_browser", "Visits" = "visits", "%" = "pct"),
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  output$chart_browsers <- plotly::renderPlotly({
    data <- browser_summary()
    shiny::req(nrow(data) > 0)
    
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = reorder(visit_browser, visits),
        y = visits,
        fill = visit_browser
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Visits by Browser",
        x = "Browser",
        y = "Number of Visits",
        fill = "Browser"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        legend.position = "none"
      )
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # SECTION 3: Device Size Analysis
  # ============================================================================
  
  device_size_summary <- shiny::reactive({
    data <- device_data()
    data %>%
      dplyr::group_by(visit_width, device_category) %>%
      dplyr::summarise(
        visits = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        pct = round((visits / sum(visits)) * 100, 1)
      ) %>%
      dplyr::arrange(dplyr::desc(visits))
  })
  
  output$table_device_sizes <- DT::renderDT({
    data <- device_size_summary()
    DT::datatable(
      data,
      colnames = c("Width (px)" = "visit_width", "Category" = "device_category", "Visits" = "visits", "%" = "pct"),
      options = list(
        pageLength = 10,
        dom = 't'
      ),
      rownames = FALSE
    )
  })
  
  output$chart_device_sizes <- plotly::renderPlotly({
    data <- device_size_summary()
    shiny::req(nrow(data) > 0)
    
    # Take top 15 for readability
    data <- utils::head(data, 15)
    
    p <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = reorder(paste0(visit_width, "px"), visits),
        y = visits,
        fill = device_category
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Top 15 Device Sizes",
        x = "Screen Width",
        y = "Number of Visits",
        fill = "Category"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      )
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # SECTION 4: Multi-Device Usage
  # ============================================================================
  
  output$chart_multi_device <- plotly::renderPlotly({
    data <- device_data()
    shiny::req(nrow(data) > 0)
    
    # Count distinct devices per user
    devices_per_user <- data %>%
      dplyr::group_by(user_id) %>%
      dplyr::summarise(
        device_count = dplyr::n_distinct(device_signature),
        .groups = "drop"
      )
    
    # Categorize into 1, 2, 3, 4, 5+
    device_usage <- devices_per_user %>%
      dplyr::mutate(
        device_category = dplyr::case_when(
          device_count >= 5 ~ "5+",
          TRUE ~ as.character(device_count)
        )
      ) %>%
      dplyr::group_by(device_category) %>%
      dplyr::summarise(
        users = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(device_category)
    
    p <- ggplot2::ggplot(
      device_usage,
      ggplot2::aes(
        x = factor(device_category, levels = c("1", "2", "3", "4", "5+")),
        y = users,
        fill = device_category
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::labs(
        title = "Users by Number of Devices Used",
        x = "Number of Distinct Devices",
        y = "Number of Users",
        fill = "Devices"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        legend.position = "none"
      )
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # SECTION 5: Device Category by User Type
  # ============================================================================
  
  output$chart_device_by_user_type <- plotly::renderPlotly({
    data <- device_data()
    shiny::req(nrow(data) > 0)
    
    # Calculate percentages
    device_by_type <- data %>%
      dplyr::group_by(user_type, device_category) %>%
      dplyr::summarise(
        visits = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::group_by(user_type) %>%
      dplyr::mutate(
        total = sum(visits),
        pct = (visits / total) * 100
      ) %>%
      dplyr::ungroup()
    
    p <- ggplot2::ggplot(
      device_by_type,
      ggplot2::aes(
        x = user_type,
        y = pct,
        fill = device_category
      )
    ) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      ggplot2::labs(
        title = "Device Usage by User Type",
        x = "User Type",
        y = "Percentage of Visits (%)",
        fill = "Device Category"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      )
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # SECTION 6: Device Trends Over Time
  # ============================================================================
  
  output$chart_device_trends <- plotly::renderPlotly({
    data <- device_data()
    shiny::req(nrow(data) > 0)
    
    # Convert timestamp to date and group by month
    monthly_data <- data %>%
      dplyr::mutate(
        visit_date = as.Date(visit_timestamp),
        month = as.Date(lubridate::floor_date(visit_date, "month"))
      ) %>%
      dplyr::group_by(month, device_category) %>%
      dplyr::summarise(
        visits = dplyr::n(),
        .groups = "drop"
      )
    
    p <- ggplot2::ggplot(
      monthly_data,
      ggplot2::aes(
        x = month,
        y = visits,
        color = device_category
      )
    ) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = "Device Usage Trends Over Time",
        x = "Month",
        y = "Number of Visits",
        color = "Device Category"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold")
      ) +
      ggplot2::scale_x_date(
        date_breaks = "1 month",
        date_labels = "%b %Y"
      )
    
    plotly::ggplotly(p)
  })
}