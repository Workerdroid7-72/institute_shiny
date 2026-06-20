library(shiny)
library(bslib)
library(dplyr)
library(DBI)
library(plotly)

# Explicitly source the global environment file
source("global.R")

# ==============================================================================
# USER INTERFACE (UI)
# ==============================================================================

professional_theme <- bslib::bs_theme(
  version = 5,
  bg = "#2b3035",
  fg = "#f8f9fa",
  primary = "#0d6efd",
  base_font = bslib::font_google("Inter")
)

ui <- bslib::page_navbar(
  title = "Platform Engagement Dashboard",
  theme = professional_theme,
  id = "main_tabs",
  fillable = FALSE,
  
  # Sidebar with cascading filters
  sidebar = bslib::sidebar(
    title = "Filters",
    open = "always",
    shiny::selectInput(
      "filter_country",
      "Country",
      choices = c("Loading..." = ""),
      selected = ""
    ),
    shiny::selectInput(
      "filter_region",
      "Region",
      choices = c("Loading..." = ""),
      selected = ""
    ),
    shiny::selectInput(
      "filter_user_type",
      "User Type",
      choices = c("Loading..." = ""),
      selected = ""
    ),
    shiny::selectInput(
      "filter_account_mgr",
      "Account Manager",
      choices = c("Loading..." = ""),
      selected = ""
    )
  ),
  
  # Overview Tab
  bslib::nav_panel(
    title = "Overview",
    icon = shiny::icon("chart-line"),
    
    bslib::layout_column_wrap(
      width = 1 / 6,
      heights_equal = "all",
      
      # --- Total Users ---
      htmltools::div(
        style = "position: relative; width: 100%; height: 100%;",
        bslib::value_box(
          title = "Total Users",
          value = shiny::textOutput("metric_total_users"),
          showcase = shiny::icon("users"),
          theme = "primary"
        ),
        shiny::actionButton(
          "show_total_users",
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn-sm",
          style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
        )
      ),
      
      # --- Active Users ---
      htmltools::div(
        style = "position: relative; width: 100%; height: 100%;",
        bslib::value_box(
          title = "Active Users (30d)",
          value = shiny::textOutput("metric_active_users"),
          showcase = shiny::icon("user-check"),
          theme = "success"
        ),
        shiny::actionButton(
          "show_active_users",
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn-sm",
          style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
        )
      ),
      
      # --- New Users ---
      htmltools::div(
        style = "position: relative; width: 100%; height: 100%;",
        bslib::value_box(
          title = "New Users (30d)",
          value = shiny::textOutput("metric_new_users"),
          showcase = shiny::icon("user-plus"),
          theme = "info"
        ),
        shiny::actionButton(
          "show_new_users",
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn-sm",
          style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
        )
      ),
      
      # --- Core 1 Completed ---
      htmltools::div(
        style = "position: relative; width: 100%; height: 100%;",
        bslib::value_box(
          title = "Core 1 Completed",
          value = shiny::textOutput("metric_core1"),
          showcase = shiny::icon("graduation-cap"),
          theme = "warning"
        ),
        shiny::actionButton(
          "show_core1",
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn-sm",
          style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
        )
      ),
      
      # --- Inactive Users ---
      htmltools::div(
        style = "position: relative; width: 100%; height: 100%;",
        bslib::value_box(
          title = "Inactive Users (%)",
          value = shiny::textOutput("metric_inactive_pct"),
          showcase = shiny::icon("user-slash"),
          theme = "danger"
        ),
        shiny::actionButton(
          "show_inactive",
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn-sm",
          style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
        )
      ),
      
      # --- Not Started ---
      htmltools::div(
        style = "position: relative; width: 100%; height: 100%;",
        bslib::value_box(
          title = "Not Started",
          value = shiny::textOutput("metric_not_started"),
          showcase = shiny::icon("hourglass-start"),
          theme = "secondary"
        ),
        shiny::actionButton(
          "show_not_started",
          label = NULL,
          icon = shiny::icon("expand"),
          class = "btn-sm",
          style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
        )
      ),
      
      # --- Unknown Role
      # htmltools::div(
      #   style = "position: relative; width: 100%; height: 100%;",
      #   bslib::value_box(
      #     title = "Unknown Role",
      #     value = shiny::textOutput("metric_unknown_role"),
      #     showcase = shiny::icon("question-circle"),
      #     theme = "info"
      #   ),
      #   shiny::actionButton(
      #     "show_unknown_role",
      #     label = NULL,
      #     icon = shiny::icon("expand"),
      #     class = "btn-sm",
      #     style = "position: absolute; top: 8px; right: 8px; z-index: 100; opacity: 0.7;"
      #   )
      # )
    ),
    # Charts section - stacked vertically
    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "all",
      
      plotly::plotlyOutput("chart_new_users_over_time", height = "400px"),
      plotly::plotlyOutput("chart_core1_over_time", height = "400px")
    )
    
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  # ============================================================================
  # HELPER: Create standardized user table
  # ============================================================================
  create_user_table <- function(data) {
    # Select and rename columns
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
    
    # Convert TRUE/FALSE to Yes/No for better readability
    display_data$"Core 1" <- ifelse(display_data$"Core 1" == TRUE, "Yes", "No")
    
    # Create the datatable
    DT::datatable(
      display_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        columnDefs = list(
          list(width = '200px', targets = c(0, 4)),
          # Name and Account Manager columns
          list(width = '100px', targets = c(1, 2, 3, 5))  # Other columns
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
  # OBSERVER 4: Update Account Manager dropdown when Country/Region changes
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
    
    # Country Filter
    if (!is.null(input$filter_country) &&
        input$filter_country != "All") {
      filtered <- dplyr::filter(filtered, country_name == input$filter_country)
    }
    
    # Region Filter
    if (!is.null(input$filter_region) &&
        input$filter_region != "All") {
      filtered <- dplyr::filter(filtered, region_name == input$filter_region)
    }
    
    # User Type Filter
    if (!is.null(input$filter_user_type) &&
        input$filter_user_type != "All") {
      filtered <- dplyr::filter(filtered, user_type == input$filter_user_type)
    }
    
    # Account Manager Filter
    if (!is.null(input$filter_account_mgr) &&
        input$filter_account_mgr != "All") {
      filtered <- dplyr::filter(
        filtered,
        account_manager_name == input$filter_account_mgr |
          is.na(account_manager_name) |
          account_manager_name == "None / Company Staff"
      )
    }
    
    return(filtered)
  })
  
  # ============================================================================
  # METRIC 1: Total Users
  # ============================================================================
  output$metric_total_users <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0")
    formatC(nrow(data), format = "d", big.mark = ",")
  })
  
  # ============================================================================
  # METRIC 2: Active Users (30 days)
  # ============================================================================
  output$metric_active_users <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0")
    
    cutoff_date <- lubridate::today() - lubridate::days(30)
    active_count <- sum(data$last_active_date >= cutoff_date, na.rm = TRUE)
    formatC(active_count, format = "d", big.mark = ",")
  })
  
  # ============================================================================
  # METRIC 3: New Users (30 days)
  # ============================================================================
  output$metric_new_users <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0")
    
    cutoff_date <- lubridate::today() - lubridate::days(30)
    new_count <- sum(as.Date(data$date_registered) >= cutoff_date, na.rm = TRUE)
    formatC(new_count, format = "d", big.mark = ",")
  })
  
  # ============================================================================
  # METRIC 4: Core 1 Completed
  # ============================================================================
  output$metric_core1 <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0")
    
    core1_count <- sum(data$core_1_completed, na.rm = TRUE)
    formatC(core1_count, format = "d", big.mark = ",")
  })
  
  # ============================================================================
  # METRIC 5: Inactive Users (%)
  # ============================================================================
  output$metric_inactive_pct <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0%")
    
    total_users <- nrow(data)
    cutoff_date <- lubridate::today() - lubridate::days(30)
    active_count <- sum(data$last_active_date >= cutoff_date, na.rm = TRUE)
    inactive_count <- total_users - active_count
    inactive_pct <- (inactive_count / total_users) * 100
    
    paste0(round(inactive_pct, 1), "%")
  })
  
  # ============================================================================
  # METRIC 6: Not Started
  # ============================================================================
  output$metric_not_started <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0")
    
    not_started_count <- sum(data$not_started, na.rm = TRUE)
    formatC(not_started_count,
            format = "d",
            big.mark = ",")
  })
  
  
  # ============================================================================
  # METRIC 7: Unkown Role
  # ============================================================================
  output$metric_unknown_role <- shiny::renderText({
    data <- filtered_user_data()
    if (is.null(data) || nrow(data) == 0)
      return("0")
    unknown_role_count <- sum(data$user_type == "Unknown Role")
    formatC(unknown_role_count,
            format = "d",
            big.mark = ",")
    
  })
  
  
  # ============================================================================
  # CHART
  # ============================================================================
  output$chart_new_users_over_time <- plotly::renderPlotly({
    data <- filtered_user_data()
    shiny::req(nrow(data) > 0)
    
    monthly_data <- data %>%
      dplyr::mutate(month = as.Date(lubridate::floor_date(date_registered, "month"))) %>%
      dplyr::group_by(month, user_type) %>%                                  
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    # Calculate x-axis limits
    start_date <- min(monthly_data$month)
    end_date <- lubridate::floor_date(lubridate::today() + lubridate::days(31), "month")
    
    p <- ggplot2::ggplot(
      monthly_data,
      ggplot2::aes(x = month, y = count, color = user_type)
    ) +
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
      # NEW: Control x-axis date formatting and limits
      ggplot2::scale_x_date(
        limits = c(start_date, end_date),
        date_breaks = "1 month",
        date_labels = "%b %Y"
      )
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # CHART 2: Core 1 Completions Over Time
  # ============================================================================
  output$chart_core1_over_time <- plotly::renderPlotly({
    data <- filtered_user_data()
    
    # Only include users who have actually completed Core 1
    data <- dplyr::filter(data, !is.na(core_1_completed_date))
    shiny::req(nrow(data) > 0)
    
    monthly_data <- data %>%
      dplyr::mutate(month = as.Date(lubridate::floor_date(core_1_completed_date, "month"))) %>%  
      dplyr::group_by(month, user_type) %>%                                  
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    start_date <- min(monthly_data$month)
    end_date <- lubridate::floor_date(lubridate::today() + lubridate::days(31), "month")
    
    p <- ggplot2::ggplot(
      monthly_data,
      ggplot2::aes(x = month, y = count, color = user_type)
    ) +
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
  # MODAL: Separate observers for each button
  # ============================================================================
  
  # Total Users button
  shiny::observeEvent(input$show_total_users, {
    shiny::req(input$show_total_users > 0)
    
    data <- filtered_user_data()
    
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
  
  # Active Users button
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
  
  # New Users button
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
  
  # Core 1 button
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
  
  # Inactive Users button
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
  
  # Not Started button
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
  
  # Unknown Role button
  shiny::observeEvent(input$show_unknown_role, {
    shiny::req(input$show_unknown_role > 0)
    
    shiny::showModal(
      shiny::modalDialog(
        title = "Users: Unknown Role",
        size = "xl",
        easyClose = TRUE,
        DT::DTOutput("user_table_unknown_role"),
        footer = shiny::modalButton("Close")
      )
    )
  })
  
  # ============================================================================
  # RENDER: Separate table renderers for each modal
  # ============================================================================
  
  # Total Users table
  output$user_table_total <- DT::renderDT({
    data <- filtered_user_data()
    create_user_table(data)
  })
  
  # Active Users table
  output$user_table_active <- DT::renderDT({
    data <- filtered_user_data()
    cutoff_date <- lubridate::today() - lubridate::days(30)
    metric_data <- dplyr::filter(data, last_active_date >= cutoff_date)
    create_user_table(metric_data)
  })
  
  # New Users table
  output$user_table_new <- DT::renderDT({
    data <- filtered_user_data()
    cutoff_date <- lubridate::today() - lubridate::days(30)
    metric_data <- dplyr::filter(data, as.Date(date_registered) >= cutoff_date)
    create_user_table(metric_data)
  })
  
  # Core 1 table
  output$user_table_core1 <- DT::renderDT({
    data <- filtered_user_data()
    metric_data <- dplyr::filter(data, core_1_completed == TRUE)
    create_user_table(metric_data)
  })
  
  # Inactive Users table
  output$user_table_inactive <- DT::renderDT({
    data <- filtered_user_data()
    cutoff_date <- lubridate::today() - lubridate::days(30)
    metric_data <- dplyr::filter(data,
                                 last_active_date < cutoff_date |
                                   is.na(last_active_date))
    create_user_table(metric_data)
  })
  
  # Not Started table
  output$user_table_not_started <- DT::renderDT({
    data <- filtered_user_data()
    metric_data <- dplyr::filter(data, not_started == TRUE)
    create_user_table(metric_data)
  })
  
  # Unknown Role
  output$user_table_unknown_role <- DT::renderDT({
    data <- filtered_user_data()
    metric_data <- dplyr::filter(data, user_type == "Unknown Role")
    create_user_table(metric_data)
  })
  
  
}

shiny::shinyApp(ui = ui, server = server)