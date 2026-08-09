# ============================================================================
# SERVER: Overview Tab
# ============================================================================

server_overview <- function(input, output, session) {
  
  # ============================================================================
  # HELPER: Create standardized user table
  # ============================================================================
  create_user_table <- function(data) {
    display_data <- dplyr::select(
      data,
      "Name" = full_name,
      "Role" = user_type,
      "Country" = country_name,
      "Region" = region_name,
      "Account Manager" = account_manager_name,
      "Core 1" = core_1_completed,
      "Last Active" = last_active_date
    )
    
    display_data$"Core 1" <- ifelse(display_data$"Core 1" == TRUE, "Yes", "No")
    
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(width = '200px', targets = c(0, 4)),
          list(width = '100px', targets = c(1, 2, 3, 5))
        )
      ),
      rownames = FALSE,
      filter = "top"
    )
  }
  
  # ============================================================================
  # OBSERVER 1: Populate Country dropdown
  # ============================================================================
  shiny::observe({
    df <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT country_name FROM vw_platform_users_flat ORDER BY country_name;"
    )
    shiny::updateSelectInput(
      session,
      "filter_country",
      choices = c("All", unique(df$country_name)),
      selected = "All"
    )
  })
  
  # ============================================================================
  # OBSERVER 2: Update Region dropdown when Country changes
  # ============================================================================
  shiny::observe({
    selected_country <- input$filter_country
    
    if (is.null(selected_country) || selected_country == "All") {
      df <- DBI::dbGetQuery(
        con,
        "SELECT DISTINCT region_name FROM vw_platform_users_flat ORDER BY region_name;"
      )
    } else {
      df <- DBI::dbGetQuery(
        con,
        sprintf(
          "SELECT DISTINCT region_name FROM vw_platform_users_flat WHERE country_name = '%s' ORDER BY region_name;",
          selected_country
        )
      )
    }
    
    shiny::updateSelectInput(
      session,
      "filter_region",
      choices = c("All", unique(df$region_name)),
      selected = "All"
    )
  })
  
  # ============================================================================
  # OBSERVER 3: Populate User Type dropdown
  # ============================================================================
  shiny::observe({
    df <- DBI::dbGetQuery(con,
                          "SELECT DISTINCT user_type FROM vw_platform_users_flat ORDER BY user_type;")
    shiny::updateSelectInput(
      session,
      "filter_user_type",
      choices = c("All", unique(df$user_type)),
      selected = "All"
    )
  })
  
  # ============================================================================
  # OBSERVER 4: Update Account Manager dropdown
  # ============================================================================
  shiny::observe({
    selected_country <- input$filter_country
    selected_region <- input$filter_region
    
    if ((is.null(selected_country) || selected_country == "All") &&
        (is.null(selected_region) || selected_region == "All")) {
      query <- "
        SELECT DISTINCT am.fname || ' ' || am.lname AS account_manager_name
        FROM z_institute_users am
        WHERE am.user_id IN (SELECT DISTINCT account_manager FROM z_institute_users WHERE account_manager IS NOT NULL)
        ORDER BY account_manager_name;
      "
    } else if (!is.null(selected_country) &&
               selected_country != "All" &&
               (is.null(selected_region) ||
                selected_region == "All")) {
      query <- sprintf(
        "
        SELECT DISTINCT am.fname || ' ' || am.lname AS account_manager_name
        FROM z_institute_users am
        JOIN z_institute_lookup_region_manager rm ON am.user_id = rm.user_id
        JOIN z_institute_lookup_region r ON rm.region_id = r.region_id
        JOIN z_institute_lookup_country c ON r.country_id = c.country_id
        WHERE c.country_name = '%s'
        ORDER BY account_manager_name;
      ",
        selected_country
      )
    } else if (!is.null(selected_region) &&
               selected_region != "All") {
      query <- sprintf(
        "
        SELECT DISTINCT am.fname || ' ' || am.lname AS account_manager_name
        FROM z_institute_users am
        JOIN z_institute_lookup_region_manager rm ON am.user_id = rm.user_id
        JOIN z_institute_lookup_region r ON rm.region_id = r.region_id
        WHERE r.region_name = '%s'
        ORDER BY account_manager_name;
      ",
        selected_region
      )
    } else {
      query <- "SELECT DISTINCT 'None' AS account_manager_name;"
    }
    
    df <- DBI::dbGetQuery(con, query)
    am_choices <- c("All", unique(df$account_manager_name))
    if (!"None / Company Staff" %in% am_choices) {
      am_choices <- c(am_choices, "None / Company Staff")
    }
    
    shiny::updateSelectInput(session,
                             "filter_account_mgr",
                             choices = am_choices,
                             selected = "All")
  })
  
  # ============================================================================
  # REACTIVE: Filtered user data
  # ============================================================================
  filtered_user_data <- shiny::reactive({
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
  output$metric_total_users <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0) return("0")
    formatC(nrow(data), format = "d", big.mark = ",")
  })
  
  output$metric_active_users <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0) return("0")
    cutoff_date <- lubridate::today() - lubridate::days(30)
    active_count <- sum(data$last_active_date >= cutoff_date, na.rm = TRUE)
    formatC(active_count, format = "d", big.mark = ",")
  })
  
  output$metric_new_users <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0) return("0")
    cutoff_date <- lubridate::today() - lubridate::days(30)
    new_count <- sum(as.Date(data$date_registered) >= cutoff_date, na.rm = TRUE)
    formatC(new_count, format = "d", big.mark = ",")
  })
  
  output$metric_core1 <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0) return("0")
    core1_count <- sum(data$core_1_completed, na.rm = TRUE)
    formatC(core1_count, format = "d", big.mark = ",")
  })
  
  output$metric_inactive_pct <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0) return("0%")
    total_users <- nrow(data)
    inactive_count <- sum(data$is_truly_inactive == TRUE, na.rm = TRUE)
    inactive_pct <- (inactive_count / total_users) * 100
    paste0(round(inactive_pct, 1), "%")
  })
  
  output$metric_not_started <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0) return("0")
    not_started_count <- sum(data$not_started, na.rm = TRUE)
    formatC(not_started_count, format = "d", big.mark = ",")
  })
  
  # ============================================================================
  # CHARTS
  # ============================================================================
  output$chart_new_users_over_time <- plotly::renderPlotly({
    data <- filtered_user_data()
    shiny::req(nrow(data) > 0)
    
    monthly_data <- data %>%
      dplyr::mutate(month = as.Date(lubridate::floor_date(date_registered, "month"))) %>%
      dplyr::group_by(month, user_type) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    start_date <- min(monthly_data$month)
    end_date <- lubridate::floor_date(lubridate::today() + lubridate::days(31), "month")
    
    p <- ggplot2::ggplot(monthly_data,
                         ggplot2::aes(x = month, y = count, color = user_type)) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = "New User Registrations Over Time",
        x = "Month",
        y = "Number of New Users",
        color = "User Type"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA)
      ) +
      ggplot2::scale_x_date(
        limits = c(start_date, end_date),
        date_breaks = "1 month",
        date_labels = "%b %Y"
      )
    
    plotly::ggplotly(p)
  })
  
  output$chart_core1_over_time <- plotly::renderPlotly({
    data <- filtered_user_data()
    data <- dplyr::filter(data, !is.na(core_1_completed_date))
    shiny::req(nrow(data) > 0)
    
    monthly_data <- data %>%
      dplyr::mutate(month = as.Date(lubridate::floor_date(core_1_completed_date, "month"))) %>%
      dplyr::group_by(month, user_type) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    start_date <- min(monthly_data$month)
    end_date <- lubridate::floor_date(lubridate::today() + lubridate::days(31), "month")
    
    p <- ggplot2::ggplot(monthly_data,
                         ggplot2::aes(x = month, y = count, color = user_type)) +
      ggplot2::geom_line(size = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = "Core 1 Completions Over Time",
        x = "Month",
        y = "Number of Completions",
        color = "User Type"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::scale_x_date(
        limits = c(start_date, end_date),
        date_breaks = "1 month",
        date_labels = "%b %Y"
      ) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA)
      )
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # MODALS
  # ============================================================================
  shiny::observeEvent(input$show_total_users, {
    shiny::req(input$show_total_users > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: Total Users",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_total"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  shiny::observeEvent(input$show_active_users, {
    shiny::req(input$show_active_users > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: Active Users (30d)",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_active"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  shiny::observeEvent(input$show_new_users, {
    shiny::req(input$show_new_users > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: New Users (30d)",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_new"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  shiny::observeEvent(input$show_core1, {
    shiny::req(input$show_core1 > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: Core 1 Completed",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_core1"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  shiny::observeEvent(input$show_inactive, {
    shiny::req(input$show_inactive > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: Inactive Users",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_inactive"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  shiny::observeEvent(input$show_not_started, {
    shiny::req(input$show_not_started > 0)
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: Not Started",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_not_started"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  # ============================================================================
  # TABLE RENDERERS
  # ============================================================================
  output$user_table_total <- DT::renderDT({
    data <- filtered_user_data()
    create_user_table(data)
  })
  
  output$user_table_active <- DT::renderDT({
    data <- filtered_user_data()
    cutoff_date <- lubridate::today() - lubridate::days(30)
    metric_data <- dplyr::filter(data, last_active_date >= cutoff_date)
    create_user_table(metric_data)
  })
  
  output$user_table_new <- DT::renderDT({
    data <- filtered_user_data()
    cutoff_date <- lubridate::today() - lubridate::days(30)
    metric_data <- dplyr::filter(data, as.Date(date_registered) >= cutoff_date)
    create_user_table(metric_data)
  })
  
  output$user_table_core1 <- DT::renderDT({
    data <- filtered_user_data()
    metric_data <- dplyr::filter(data, core_1_completed == TRUE)
    create_user_table(metric_data)
  })
  
  output$user_table_inactive <- DT::renderDT({
    data <- filtered_user_data()
    metric_data <- dplyr::filter(data, is_truly_inactive == TRUE)
    create_user_table(metric_data)
  })
  
  output$user_table_not_started <- DT::renderDT({
    data <- filtered_user_data()
    metric_data <- dplyr::filter(data, not_started == TRUE)
    create_user_table(metric_data)
  })
}