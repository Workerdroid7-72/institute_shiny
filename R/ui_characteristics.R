# ============================================================================
# UI: User Characteristics Tab
# ============================================================================

ui_characteristics <- bslib::nav_panel(
  title = "User Characteristics",
  icon = shiny::icon("users-gear"),
  
  # Chart: User Type Distribution
  shiny::h4("Users by Role"),
  plotly::plotlyOutput("chart_user_type", height = "400px"),
  
  # Chart: Work Experience Distribution
  shiny::h4("Users by Work Experience"),
  plotly::plotlyOutput("chart_work_experience", height = "400px"),
  
  # Chart: Current Module Distribution
  shiny::h4("Users by Current Module"),
  plotly::plotlyOutput("chart_current_module", height = "400px"),
  
  # Dynamic Qualification Section
  uiOutput("ui_qualification_section"),
  
  # Dynamic Education Section
  uiOutput("ui_education_section")
  
 
)