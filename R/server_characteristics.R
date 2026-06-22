# ============================================================================
# SERVER: User Characteristics Tab
# ============================================================================

server_characteristics <- function(input, output, session) {
  # ============================================================================
  # REACTIVE: User characteristics data (for demographics tab)
  # ============================================================================
  characteristics_data <- shiny::reactive({
    raw_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_user_characteristics;")
    filtered <- raw_data
    
    # Apply the same sidebar filters
    if (!is.null(input$filter_country) &&
        input$filter_country != "All") {
      filtered <- dplyr::filter(filtered, country_name == input$filter_country)
    }
    
    if (!is.null(input$filter_region) &&
        input$filter_region != "All") {
      filtered <- dplyr::filter(filtered, region_name == input$filter_region)
    }
    
    if (!is.null(input$filter_user_type) &&
        input$filter_user_type != "All") {
      filtered <- dplyr::filter(filtered, user_type == input$filter_user_type)
    }
    
    # if (!is.null(input$filter_account_mgr) && input$filter_account_mgr != "All") {
    #   filtered <- dplyr::filter(
    #     filtered,
    #     account_manager_name == input$filter_account_mgr |
    #       is.na(account_manager_name) |
    #       account_manager_name == "None / Company Staff"
    #   )
    # }
    # Account Manager Filter
    if (!is.null(input$filter_account_mgr) &&
        input$filter_account_mgr != "All") {
      if (input$filter_account_mgr == "None / Company Staff") {
        # Special case: show users without a dedicated account manager
        filtered <- dplyr::filter(
          filtered,
          is.na(account_manager_name) |
            account_manager_name == "None / Company Staff"
        )
      } else {
        # Normal case: show only users with this specific account manager
        filtered <- dplyr::filter(filtered,
                                  account_manager_name == input$filter_account_mgr)
      }
    }
    
    return(filtered)
  })
  
  # ============================================================================
  # CHART: User Type Distribution (Horizontal, Descending)
  # ============================================================================
  output$chart_user_type <- plotly::renderPlotly({
    data <- characteristics_data()
    shiny::req(nrow(data) > 0)
    
    # Count users by user_type
    user_type_counts <- data %>%
      dplyr::group_by(user_type) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    p <- ggplot2::ggplot(user_type_counts,
                         ggplot2::aes(
                           x = reorder(user_type, count),
                           # Reorder by count (ascending factor levels)
                           y = count,
                           fill = user_type
                         )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +  # Flips to horizontal bars
      ggplot2::labs(
        title = "Users by Role",
        x = "User Type",
        y = "Number of Users",
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
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # CHART: Work Experience Distribution (Horizontal, Descending)
  # ============================================================================
  output$chart_work_experience <- plotly::renderPlotly({
    data <- characteristics_data()
    shiny::req(nrow(data) > 0)
    
    # Count users by work_experience
    work_exp_counts <- data %>%
      dplyr::group_by(work_experience) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    p <- ggplot2::ggplot(work_exp_counts,
                         ggplot2::aes(
                           x = reorder(work_experience, count),
                           y = count,
                           fill = work_experience
                         )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Users by Work Experience",
        x = "Work Experience",
        y = "Number of Users",
        fill = "Work Experience"
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
    
    plotly::ggplotly(p)
  })
  
  # ============================================================================
  # CHART: Current Module Distribution (Ordered by Module Number)
  # ============================================================================
  output$chart_current_module <- plotly::renderPlotly({
    data <- characteristics_data()
    shiny::req(nrow(data) > 0)
    
    # Count users by current_module_cleaned, preserving sort order
    module_counts <- data %>%
      dplyr::group_by(current_module_cleaned, module_sort_order) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    p <- ggplot2::ggplot(module_counts,
                         ggplot2::aes(
                           x = reorder(current_module_cleaned, module_sort_order),
                           # Order by module number
                           y = count,
                           fill = current_module_cleaned
                         )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        title = "Users by Current Module",
        x = "Current Module",
        y = "Number of Users",
        fill = "Current Module"
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
    
    plotly::ggplotly(p)
  })
  
  
  # ============================================================================
  # CONDITIONAL UI: Qualification Section
  # ============================================================================
  output$ui_qualification_section <- renderUI({
    req(!is.null(input$filter_country))
    
    if (input$filter_country != "All") {
      shiny::tagList(
        shiny::h4("Users by Qualification"),
        plotly::plotlyOutput("chart_qualification", height = "400px")
      )
    } else {
      shiny::div(
        style = "padding: 25px; background-color: #e9ecef; border-left: 5px solid #495057; margin-top: 20px; margin-bottom: 20px; border-radius: 4px;",
        shiny::h4(
          "Users by Qualification",
          style = "margin-top: 0; color: #212529;"
        ),
        shiny::p(
          "Please select a specific country from the sidebar to view qualification distribution. Categories are country-specific and cannot be meaningfully aggregated.",
          style = "font-size: 15px; color: #495057; margin-bottom: 0; line-height: 1.6;"
        )
      )
    }
  })
  
  # ============================================================================
  # CHART: Qualification Distribution (Horizontal, Descending)
  # ============================================================================
  output$chart_qualification <- plotly::renderPlotly({
    data <- characteristics_data()
    shiny::req(nrow(data) > 0)
    
    qual_counts <- data %>%
      dplyr::group_by(qualification) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    p <- ggplot2::ggplot(qual_counts,
                         ggplot2::aes(
                           x = reorder(qualification, count),
                           y = count,
                           fill = qualification
                         )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Users by Qualification", x = "Qualification", y = "Number of Users") +
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
  # CONDITIONAL UI: Education Section
  # ============================================================================
  output$ui_education_section <- renderUI({
    req(!is.null(input$filter_country))
    
    if (input$filter_country != "All") {
      shiny::tagList(
        shiny::h4("Users by Education Level"),
        plotly::plotlyOutput("chart_education", height = "400px")
      )
    } else {
      shiny::div(
        style = "padding: 25px; background-color: #e9ecef; border-left: 5px solid #495057; margin-top: 20px; margin-bottom: 20px; border-radius: 4px;",
        shiny::h4(
          "Users by Education Level",
          style = "margin-top: 0; color: #212529;"
        ),
        shiny::p(
          "Please select a specific country from the sidebar to view education level distribution. Categories are country-specific and cannot be meaningfully aggregated.",
          style = "font-size: 15px; color: #495057; margin-bottom: 0; line-height: 1.6;"
        )
      )
    }
  })
  
  # ============================================================================
  # CHART: Education Distribution (Horizontal, Descending)
  # ============================================================================
  output$chart_education <- plotly::renderPlotly({
    data <- characteristics_data()
    shiny::req(nrow(data) > 0)
    
    edu_counts <- data %>%
      dplyr::group_by(education_level) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop")
    
    p <- ggplot2::ggplot(edu_counts,
                         ggplot2::aes(
                           x = reorder(education_level, count),
                           y = count,
                           fill = education_level
                         )) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::coord_flip() +
      ggplot2::labs(title = "Users by Education Level", x = "Education Level", y = "Number of Users") +
      ggplot2::theme_minimal() +
      tidyquant::theme_tq() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, face = "bold"),
        axis.title = ggplot2::element_text(face = "bold"),
        legend.position = "none"
      )
    
    plotly::ggplotly(p)
  })
  
  
  
}