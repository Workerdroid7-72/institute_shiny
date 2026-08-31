library(shiny)
library(bslib)
library(dplyr)
library(DBI)
library(plotly)

source("global.R")
source("R/ui_overview.R")
source("R/server_overview.R")
source("R/ui_characteristics.R")
source("R/server_characteristics.R")
source("R/ui_device_characteristics.R")
source("R/server_device_characteristics.R")
source("R/ui_visits.R")
source("R/server_visits.R")




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

dashboard_ui <- bslib::page_navbar(
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
  ui_overview,
  # NEW: Characteristics tab
  ui_characteristics,
  ui_device_characteristics,
  ui_visits
  
)


# ==============================================================================
# SERVER LOGIC
# ==============================================================================

dashboard_server <- function(input, output, session) {
  # Overview tab
  server_overview(input, output, session)
  #characteristics tab
  server_characteristics(input, output, session)
  #device tab
  server_device_characteristics(input, output, session)
  #visits tab
  server_visits(input, output, session)
  
  
}



# ==============================================================================
# LOGIN UI
# ==============================================================================
login_ui <- shiny::div(
  style = "
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    min-height: 100vh;
    margin: -1rem;
    padding: 1rem;
    display: flex;
    align-items: center;
    justify-content: center;
  ",
  
  shiny::tags$style(shiny::HTML("
    /* Password input styling */
    #login_password {
      background-color: white !important;
      color: #333 !important;
      border: 1px solid #ced4da !important;
    }
    #login_password:focus {
      background-color: white !important;
      color: #333 !important;
      border-color: #80bdff !important;
      box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25) !important;
    }
    
    /* Form labels - make them dark and readable */
    .form-label, label {
      color: #333 !important;
      font-weight: 500 !important;
    }
    
    /* Error message - red and bold */
    .login-error {
      color: #dc3545 !important;
      font-weight: 600 !important;
      text-align: center;
      margin-top: 15px;
      font-size: 14px;
    }
  ")),
  
  shiny::div(
    style = "
      max-width: 400px;
      width: 100%;
      padding: 40px;
      background: white;
      border-radius: 10px;
      box-shadow: 0 10px 40px rgba(0,0,0,0.2);
    ",
    shiny::h2(
      "Platform Engagement Dashboard",
      style = "text-align: center; color: #333; margin-bottom: 30px; font-size: 24px; font-weight: 600;"
    ),
    shiny::passwordInput("login_password", "Password:", width = "100%"),
    shiny::actionButton(
      "login_btn", 
      "Login", 
      class = "btn-primary", 
      style = "width: 100%; margin-top: 15px;"
    ),
    shiny::uiOutput("login_error")
  )
)
# ==============================================================================
# WRAPPER UI — decides which UI to show
# ==============================================================================
ui <- bslib::page_fluid(
  theme = professional_theme,
  shiny::uiOutput("app_ui")
)

# ==============================================================================
# WRAPPER SERVER — handles login, then delegates to dashboard_server
# ==============================================================================
server <- function(input, output, session) {
  
  # Track authentication state
  authenticated <- shiny::reactiveVal(FALSE)
  
  # Render the appropriate UI
  output$app_ui <- shiny::renderUI({
    if (authenticated()) {
      dashboard_ui
    } else {
      login_ui
    }
  })
  
  # Handle login
  shiny::observeEvent(input$login_btn, {
    # Clear error message first
    output$login_error <- shiny::renderUI(NULL)
    
    if (isTRUE(nchar(input$login_password) > 0) && 
        input$login_password == APP_PASSWORD) {
      authenticated(TRUE)
    } else {
      output$login_error <- shiny::renderUI({
        shiny::div("Incorrect password. Please try again.", class = "login-error")
      })
    }
  })
  
  # CRITICAL: Call dashboard_server ONLY AFTER the UI is rendered
  # onFlushed ensures the dashboard inputs exist in the DOM
  shiny::observeEvent(authenticated(), {
    if (authenticated()) {
      session$onFlushed(function() {
        dashboard_server(input, output, session)
      }, once = TRUE)
    }
  }, once = TRUE, ignoreInit = TRUE)
}

shiny::shinyApp(ui = ui, server = server)

