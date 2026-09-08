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
  # HELPER: Format a period-over-period delta as HTML
  # ============================================================================
  format_delta_html <- function(
    current,
    previous,
    label = "vs previous 30 days"
  ) {
    if (is.null(previous) || is.na(previous) || previous == 0) {
      return("")
    }
    delta_pct <- ((current - previous) / previous) * 100

    if (abs(delta_pct) < 0.05) {
      return(sprintf(
        '<div title="%s" style="font-size: 0.7em; color: #6c757d; margin-top: 2px;">&#8212; no change</div>',
        label
      ))
    }

    if (delta_pct > 0) {
      arrow <- "&#9650;" # up
      color <- "#198754" # green
    } else {
      arrow <- "&#9660;" # down
      color <- "#dc3545" # red
    }

    sprintf(
      '<div title="%s" style="font-size: 0.7em; color: %s; margin-top: 2px;">%s %.1f%%</div>',
      label,
      color,
      arrow,
      abs(delta_pct)
    )
  }

  # Invisible spacer matching the delta line height, so cards without a delta
  # stay the same size as cards with one.
  delta_spacer_html <- '<div style="font-size: 0.7em; margin-top: 2px;">&nbsp;</div>'

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
    df <- DBI::dbGetQuery(
      con,
      "SELECT DISTINCT user_type FROM vw_platform_users_flat ORDER BY user_type;"
    )
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

    if (
      (is.null(selected_country) || selected_country == "All") &&
        (is.null(selected_region) || selected_region == "All")
    ) {
      query <- "
        SELECT DISTINCT am.fname || ' ' || am.lname AS account_manager_name
        FROM z_institute_users am
        WHERE am.user_id IN (SELECT DISTINCT account_manager FROM z_institute_users WHERE account_manager IS NOT NULL)
        ORDER BY account_manager_name;
      "
    } else if (
      !is.null(selected_country) &&
        selected_country != "All" &&
        (is.null(selected_region) ||
          selected_region == "All")
    ) {
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
    } else if (
      !is.null(selected_region) &&
        selected_region != "All"
    ) {
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

    shiny::updateSelectInput(
      session,
      "filter_account_mgr",
      choices = am_choices,
      selected = "All"
    )
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

  # ============================================================================
  # REACTIVE: User data for the country chart (ignores country filter so all
  # countries stay visible for comparison)
  # ============================================================================
  country_chart_data <- shiny::reactive({
    raw_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_platform_users_flat;")
    filtered <- raw_data

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

  # ============================================================================
  # METRICS (with deltas where a clean prior-period comparison exists)
  # ============================================================================
  output$metric_core1 <- shiny::renderUI({
    data <- filtered_user_data()
    current <- sum(data$core_1_completed, na.rm = TRUE)
    cutoff <- lubridate::today() - lubridate::days(30)
    previous <- sum(
      !is.na(data$core_1_completed_date) &
        as.Date(data$core_1_completed_date) < cutoff,
      na.rm = TRUE
    )
    shiny::tagList(
      formatC(current, format = "d", big.mark = ","),
      shiny::HTML(format_delta_html(
        current,
        previous,
        "growth over last 30 days"
      ))
    )
  })

  output$metric_total_users <- shiny::renderUI({
    data <- filtered_user_data()
    current <- nrow(data)
    cutoff <- lubridate::today() - lubridate::days(30)
    previous <- sum(as.Date(data$date_registered) < cutoff, na.rm = TRUE)
    shiny::tagList(
      formatC(current, format = "d", big.mark = ","),
      shiny::HTML(format_delta_html(
        current,
        previous,
        "growth over last 30 days"
      ))
    )
  })

  output$metric_new_users <- shiny::renderUI({
    data <- filtered_user_data()
    today <- lubridate::today()
    current_start <- today - lubridate::days(30)
    prev_start <- today - lubridate::days(60)
    current <- sum(as.Date(data$date_registered) >= current_start, na.rm = TRUE)
    previous <- sum(
      as.Date(data$date_registered) >= prev_start &
        as.Date(data$date_registered) < current_start,
      na.rm = TRUE
    )
    shiny::tagList(
      formatC(current, format = "d", big.mark = ","),
      shiny::HTML(format_delta_html(current, previous))
    )
  })

  output$metric_active_users <- shiny::renderUI({
    data <- filtered_user_data()
    cutoff_date <- lubridate::today() - lubridate::days(30)
    active_count <- sum(data$last_active_date >= cutoff_date, na.rm = TRUE)
    shiny::tagList(
      formatC(active_count, format = "d", big.mark = ","),
      shiny::HTML(delta_spacer_html)
    )
  })

  output$metric_not_started <- shiny::renderUI({
    data <- filtered_user_data()
    not_started_count <- sum(data$not_started, na.rm = TRUE)
    shiny::tagList(
      formatC(not_started_count, format = "d", big.mark = ","),
      shiny::HTML(delta_spacer_html)
    )
  })

  output$metric_inactive_pct <- shiny::renderUI({
    data <- filtered_user_data()
    total_users <- nrow(data)
    if (total_users == 0) {
      return(shiny::tagList("0%", shiny::HTML(delta_spacer_html)))
    }
    inactive_count <- sum(data$is_truly_inactive == TRUE, na.rm = TRUE)
    inactive_pct <- (inactive_count / total_users) * 100
    shiny::tagList(
      paste0(round(inactive_pct, 1), "%"),
      shiny::HTML(delta_spacer_html)
    )
  })

  # ============================================================================
  # KEY INSIGHTS PANEL
  # ============================================================================
  output$overview_insights <- shiny::renderUI({
    data <- filtered_user_data()
    geo_data <- country_chart_data()
    shiny::req(nrow(data) > 0)

    cutoff <- lubridate::today() - lubridate::days(30)

    # --- Core 1 headline ---
    total_users <- nrow(data)
    core1_total <- sum(data$core_1_completed, na.rm = TRUE)
    core1_pct <- if (total_users > 0) {
      round(core1_total / total_users * 100, 1)
    } else {
      0
    }
    core1_pct_display <- formatC(core1_pct, format = "f", digits = 1)
    core1_last30 <- sum(
      !is.na(data$core_1_completed_date) &
        as.Date(data$core_1_completed_date) >= cutoff,
      na.rm = TRUE
    )

    # --- Geographic reach + leader (global view) ---
    n_countries <- dplyr::n_distinct(geo_data$country_name, na.rm = TRUE)
    country_sizes <- geo_data %>%
      dplyr::count(country_name, name = "users") %>%
      dplyr::arrange(dplyr::desc(users))
    top_country <- if (nrow(country_sizes) > 0) {
      country_sizes$country_name[1]
    } else {
      "your top market"
    }
    top_country_users <- if (nrow(country_sizes) > 0) {
      country_sizes$users[1]
    } else {
      0
    }

    # --- Emerging market: most new users in last 30 days (global view) ---
    new_by_country <- geo_data %>%
      dplyr::filter(
        !is.na(date_registered),
        as.Date(date_registered) >= cutoff
      ) %>%
      dplyr::count(country_name, name = "new_users") %>%
      dplyr::arrange(dplyr::desc(new_users))
    has_emerging <- nrow(new_by_country) > 0

    # --- Activation gap ---
    not_started_count <- sum(data$not_started, na.rm = TRUE)

    # --- Inactivity ---
    inactive_count <- sum(data$is_truly_inactive == TRUE, na.rm = TRUE)

    # --- Build the bullets ---
    bullets <- character(0)

    bullets <- c(
      bullets,
      sprintf(
        paste0(
          "<strong>%s users have completed Core 1</strong> (%s%% of all registered users), ",
          "with %s more completing in the last 30 days."
        ),
        formatC(core1_total, format = "d", big.mark = ","),
        core1_pct_display,
        formatC(core1_last30, format = "d", big.mark = ",")
      )
    )

    bullets <- c(
      bullets,
      sprintf(
        "The platform is now active in <strong>%d %s</strong>. %s is the largest market with %s users.",
        n_countries,
        if (n_countries == 1) "country" else "countries",
        top_country,
        formatC(top_country_users, format = "d", big.mark = ",")
      )
    )

    if (has_emerging) {
      bullets <- c(
        bullets,
        sprintf(
          "In the last 30 days, <strong>%s</strong> added the most new users (%s).",
          new_by_country$country_name[1],
          formatC(new_by_country$new_users[1], format = "d", big.mark = ",")
        )
      )
    }

    bullets <- c(
      bullets,
      sprintf(
        paste0(
          "<strong>%s registered users haven't started Core 1 yet</strong> ",
          "— a ready-made audience for a re-engagement nudge."
        ),
        formatC(not_started_count, format = "d", big.mark = ",")
      )
    )

    bullets <- c(
      bullets,
      sprintf(
        "%s users have gone inactive.",
        formatC(inactive_count, format = "d", big.mark = ",")
      )
    )

    shiny::HTML(paste0(
      '<ul style="margin-bottom: 0; padding-left: 1.2em;">',
      paste0(
        '<li style="margin-bottom: 0.4em; line-height: 1.4;">',
        bullets,
        "</li>",
        collapse = ""
      ),
      "</ul>"
    ))
  })

  # ============================================================================
  # CHARTS
  # ============================================================================
  output$chart_new_users_over_time <- plotly::renderPlotly({
    data <- filtered_user_data()
    shiny::req(nrow(data) > 0)

    # Monthly registrations by user type (the stacked bars)
    monthly_data <- data %>%
      dplyr::mutate(
        month = as.Date(lubridate::floor_date(date_registered, "month"))
      ) %>%
      dplyr::group_by(month, user_type) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")

    # Monthly totals (the overlay line)
    monthly_totals <- monthly_data %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(total = sum(count), .groups = "drop")

    start_date <- min(monthly_data$month)
    end_date <- lubridate::floor_date(
      lubridate::today() + lubridate::days(31),
      "month"
    )

    p <- ggplot2::ggplot(
      monthly_data,
      ggplot2::aes(x = month, y = count, fill = user_type)
    ) +
      ggplot2::geom_col(width = 22) +
      ggplot2::geom_line(
        data = monthly_totals,
        ggplot2::aes(x = month, y = total),
        inherit.aes = FALSE,
        color = "#212529",
        linewidth = 1.3
      ) +
      ggplot2::geom_point(
        data = monthly_totals,
        ggplot2::aes(x = month, y = total),
        inherit.aes = FALSE,
        color = "#212529",
        size = 2.5
      ) +
      ggplot2::labs(
        title = "New User Registrations Over Time",
        subtitle = "Bars show registrations by user type; the line traces the monthly total",
        x = "Month",
        y = "Number of New Users",
        fill = "User Type"
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        plot.subtitle = ggplot2::element_text(size = 11),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "bottom"
      ) +
      ggplot2::scale_x_date(
        limits = c(start_date - 15, end_date),
        date_breaks = "1 month",
        date_labels = "%b %Y"
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

  output$chart_core1_over_time <- plotly::renderPlotly({
    data <- filtered_user_data()
    data <- dplyr::filter(data, !is.na(core_1_completed_date))
    shiny::req(nrow(data) > 0)

    # Month each user completed Core 1
    data <- data %>%
      dplyr::mutate(
        month = as.Date(lubridate::floor_date(core_1_completed_date, "month"))
      )

    start_month <- min(data$month)
    end_month <- as.Date(lubridate::floor_date(lubridate::today(), "month"))

    # Full month grid so the cumulative line is continuous (no gaps)
    month_grid <- data.frame(
      month = seq.Date(start_month, end_month, by = "month")
    )

    monthly <- data %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(completions = dplyr::n(), .groups = "drop")

    plot_data <- month_grid %>%
      dplyr::left_join(monthly, by = "month") %>%
      dplyr::mutate(completions = dplyr::coalesce(completions, 0L)) %>%
      dplyr::arrange(month) %>%
      dplyr::mutate(cumulative = cumsum(completions))

    p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = month, y = cumulative)) +
      ggplot2::geom_area(fill = "#0d6efd", alpha = 0.25) +
      ggplot2::geom_line(color = "#0d6efd", linewidth = 1.2) +
      ggplot2::geom_point(color = "#0d6efd", size = 2) +
      ggplot2::labs(
        title = "Core 1 Completions Over Time",
        subtitle = "Cumulative number of users who have completed Core 1",
        x = "Month",
        y = "Cumulative Completions"
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

  output$chart_country_adoption <- plotly::renderPlotly({
    data <- country_chart_data()
    shiny::req(nrow(data) > 0)

    metric <- input$country_chart_metric

    metric <- input$country_chart_metric

    if (metric == "core1") {
      data <- dplyr::filter(data, core_1_completed == TRUE)
      chart_title <- "Core 1 Completions by Country"
      y_label <- "Core 1 Completions"
    } else if (metric == "new30") {
      cutoff_date <- lubridate::today() - lubridate::days(30)
      data <- dplyr::filter(data, as.Date(date_registered) >= cutoff_date)
      chart_title <- "New Users by Country (Last 30 Days)"
      y_label <- "New Users (30d)"
    } else {
      chart_title <- "Platform Adoption by Country"
      y_label <- "Total Users"
    }

    shiny::req(nrow(data) > 0)

    country_counts <- data %>%
      dplyr::count(country_name, name = "count") %>%
      dplyr::arrange(dplyr::desc(count))

    country_counts$country_name <- factor(
      country_counts$country_name,
      levels = rev(country_counts$country_name)
    )

    p <- ggplot2::ggplot(
      country_counts,
      ggplot2::aes(x = country_name, y = count, fill = count)
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::scale_fill_gradient(low = "#6ea8fe", high = "#0d6efd") +
      ggplot2::labs(
        title = chart_title,
        x = "Country",
        y = y_label
      ) +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold"),
        axis.title.x = ggplot2::element_text(face = "bold"),
        axis.title.y = ggplot2::element_text(face = "bold"),
        plot.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        panel.background = ggplot2::element_rect(fill = "#e8e8e8", color = NA),
        legend.position = "none"
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
