# ============================================================================
# UI: Device Characteristics Tab
# ============================================================================

ui_device_characteristics <- bslib::nav_panel(
  title = "Device Characteristics",
  icon = shiny::icon("mobile-screen"),
  
  # Section 1: Big Picture Metrics
  bslib::layout_column_wrap(
    width = 1 / 5,
    heights_equal = "all",
    
    bslib::value_box(
      title = "Total Visits",
      value = shiny::textOutput("metric_total_visits"),
      showcase = shiny::icon("eye"),
      theme = "primary"
    ),
    
    bslib::value_box(
      title = "Mobile (%)",
      value = shiny::textOutput("metric_mobile_pct"),
      showcase = shiny::icon("mobile-screen-button"),
      theme = "info"
    ),
    
    bslib::value_box(
      title = "Tablet (%)",
      value = shiny::textOutput("metric_tablet_pct"),
      showcase = shiny::icon("tablet-screen-button"),
      theme = "warning"
    ),
    
    bslib::value_box(
      title = "Desktop (%)",
      value = shiny::textOutput("metric_desktop_pct"),
      showcase = shiny::icon("desktop"),
      theme = "success"
    ),
    
    bslib::value_box(
      title = "Unique Users",
      value = shiny::textOutput("metric_unique_users"),
      showcase = shiny::icon("users"),
      theme = "secondary"
    )
  ),
  
  # Section 2: Browser Analysis
  shiny::h4("Browser Analysis", style = "margin-top: 40px; margin-bottom: 20px;"),
  bslib::layout_column_wrap(
    width = 1 / 2,
    heights_equal = "all",
    
    DT::DTOutput("table_browsers"),
    plotly::plotlyOutput("chart_browsers", height = "400px")
  ),
  
  # Section 3: Device Size Analysis
  shiny::h4("Device Size Analysis", style = "margin-top: 40px; margin-bottom: 20px;"),
  bslib::layout_column_wrap(
    width = 1 / 2,
    heights_equal = "all",
    
    DT::DTOutput("table_device_sizes"),
    plotly::plotlyOutput("chart_device_sizes", height = "400px")
  ),
  
  # Section 4: Multi-Device Usage
  shiny::h4("Multi-Device Usage", style = "margin-top: 70px; margin-bottom: 20px;"),
  plotly::plotlyOutput("chart_multi_device", height = "400px"),
  
  # Section 5: Device Category by User Type
  shiny::h4("Device Usage by User Type", style = "margin-top: 40px; margin-bottom: 20px;"),
  plotly::plotlyOutput("chart_device_by_user_type", height = "400px"),
  
  # Section 6: Device Trends Over Time
  shiny::h4("Device Trends Over Time", style = "margin-top: 40px; margin-bottom: 20px;"),
  plotly::plotlyOutput("chart_device_trends", height = "400px")
)