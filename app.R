library(shiny)
library(bslib)
library(dplyr)
library(DBI)

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
  
  bslib::nav_panel(
    title = "Overview",
    icon = shiny::icon("chart-line"),
    
    sidebar = bslib::sidebar(
      title = "Filters",
      open = "always",
      shiny::selectInput("filter_region", "Region", choices = NULL, selected = "All"),
      shiny::selectInput("filter_user_type", "User Type", choices = NULL, selected = "All"),
      shiny::selectInput("filter_account_mgr", "Account Manager", choices = NULL, selected = "All")
    ),
    
    bslib::layout_column_wrap(
      width = 1/5,
      heights_equal = "all",
      
      bslib::value_box(
        title = "Total Users",
        value = shiny::textOutput("metric_total_users"),
        showcase = shiny::icon("users"),
        theme = "primary"
      ),
      
      # Placeholders for the next 4 metrics
      bslib::value_box(title = "Active Users (30d)", value = "Coming Soon", showcase = shiny::icon("user-check"), theme = "success"),
      bslib::value_box(title = "New Users (30d)", value = "Coming Soon", showcase = shiny::icon("user-plus"), theme = "info"),
      bslib::value_box(title = "Core 1 Completed", value = "Coming Soon", showcase = shiny::icon("graduation-cap"), theme = "warning"),
      bslib::value_box(title = "Inactive Users (%)", value = "Coming Soon", showcase = shiny::icon("user-slash"), theme = "danger")
    )
  )
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================

server <- function(input, output, session) {
  
  # 1. POPULATE DROPDOWNS
  shiny::observe({
    # Query the VIEW
    df <- DBI::dbGetQuery(con, "SELECT DISTINCT region_name, user_type, account_manager_name FROM vw_platform_users_flat ORDER BY region_name, user_type, account_manager_name;")
    
    # Explicitly set 'selected = "All"' to ensure it sticks after the choices update
    shiny::updateSelectInput(session, "filter_region", choices = c("All", unique(df$region_name)), selected = "All")
    shiny::updateSelectInput(session, "filter_user_type", choices = c("All", unique(df$user_type)), selected = "All")
    shiny::updateSelectInput(session, "filter_account_mgr", choices = c("All", unique(df$account_manager_name)), selected = "All")
  })
  
  # 2. REACTIVE DATA FILTERING
  filtered_user_data <- shiny::reactive({
    
    # Pull the flat data from the View
    raw_data <- DBI::dbGetQuery(con, "SELECT * FROM vw_platform_users_flat;")
    filtered <- raw_data
    
    # Region Filter (Safely check for NULL)
    if (!is.null(input$filter_region) && input$filter_region != "All") {
      filtered <- dplyr::filter(filtered, region_name == input$filter_region)
    }
    
    # User Type Filter
    if (!is.null(input$filter_user_type) && input$filter_user_type != "All") {
      filtered <- dplyr::filter(filtered, user_type == input$filter_user_type)
    }
    
    # Account Manager Filter
    if (!is.null(input$filter_account_mgr) && input$filter_account_mgr != "All") {
      filtered <- dplyr::filter(
        filtered, 
        account_manager_name == input$filter_account_mgr | 
          is.na(account_manager_name) | 
          account_manager_name == "None / Company Staff"
      )
    }
    
    return(filtered)
  })
  
  # 3. RENDER METRICS
  output$metric_total_users <- shiny::renderText({
    data <- filtered_user_data()
    
    # If data is somehow empty or NULL, return "0" instead of blank
    if (is.null(data) || nrow(data) == 0) {
      return("0")
    }
    
    # Format with commas for readability (e.g., 1,234)
    formatC(nrow(data), format = "d", big.mark = ",")
  })
  
}

shiny::shinyApp(ui = ui, server = server)