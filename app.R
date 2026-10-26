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
source("R/ui_electives.R")
source("R/server_electives.R")
source("R/ui_certifications.R")
source("R/server_certifications.R")
source("R/ui_leaderboard.R")
source("R/server_leaderboard.R")


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

  header = shiny::tagList(
    shiny::tags$head(
      # --- YOUR EXISTING CSS ---
      shiny::tags$style(shiny::HTML(
        "
        #app-loading-overlay {
          display: none;
          position: fixed;
          inset: 0;
          background: rgba(255, 255, 255, 0.65);
          z-index: 9999;
          align-items: center;
          justify-content: center;
          opacity: 0;
        }
        .shiny-busy #app-loading-overlay {
          display: flex;
          animation: appOverlayIn 0s forwards 0.5s;
        }
        @keyframes appOverlayIn { to { opacity: 1; } }
      "
      )),

      # --- NUCLEAR SCROLL FIX: MutationObserver ---
      shiny::tags$script(shiny::HTML(
        "
        $(document).ready(function() {
          const observer = new MutationObserver(function(mutations) {
            mutations.forEach(function(mutation) {
              if (mutation.type === 'attributes' && mutation.attributeName === 'style') {
                const body = document.body;
                // If modal-open class is gone, force-remove overflow:hidden
                if (!body.classList.contains('modal-open')) {
                  body.style.overflow = '';
                  body.style.paddingRight = '';
                }
              }
            });
          });
          
          // Watch the body's style attribute for any changes
          observer.observe(document.body, { 
            attributes: true, 
            attributeFilter: ['style', 'class'] 
          });
        });
      "
      ))
    ),

    # --- YOUR EXISTING LOADING OVERLAY ---
    shiny::tags$div(
      id = "app-loading-overlay",
      shiny::tags$div(
        class = "d-flex flex-column align-items-center",
        shiny::tags$div(
          class = "spinner-border text-primary",
          style = "width: 3rem; height: 3rem;",
          role = "status"
        ),
        shiny::tags$span(class = "mt-2 text-muted", "Loading data...")
      )
    )
  ),

  # Tabs
  ui_overview,
  ui_certifications,
  ui_leaderboard,
  ui_visits,
  ui_electives,
  ui_characteristics,
  ui_device_characteristics,
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
  #electives tab
  server_electives(input, output, session)
  #certifications tab
  server_certifications(input, output, session)
  #leaderboard tab
  server_leaderboard(input, output, session)
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

 

  shiny::tags$style(shiny::HTML(
    "
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
  "
  )),

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
    
    # --- REMEMBER ME CHECKBOX ---
    shiny::div(
      style = "margin: 15px 0;",
      shiny::checkboxInput(
        "remember_me",
        label = shiny::tags$span(
          "Remember me for 7 days",
          style = "color: #333; font-size: 14px; font-weight: 500;"
        ),
        value = FALSE,
        width = "100%"
      )
    ),
    
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
  
  # --- COOKIE CHECK SCRIPT (SIMPLIFIED) ---
  shiny::tags$head(
    shiny::tags$script(shiny::HTML(
      "
      // Run immediately, no waiting for jQuery or Shiny
      (function() {
        console.log('=== COOKIE CHECK SCRIPT RUNNING ===');
        console.log('Current cookies:', document.cookie);
        
        // Check for auth cookie
        var cookies = document.cookie.split(';');
        for (var i = 0; i < cookies.length; i++) {
          var cookie = cookies[i].trim();
          if (cookie.startsWith('dash_auth=')) {
            var value = cookie.substring('dash_auth='.length);
            console.log('Found dash_auth cookie with value:', value);
            if (value === 'verified') {
              console.log('✅ AUTH COOKIE VALID - will auto-login');
              // Store flag for when Shiny connects
              window.__dashAuthValid = true;
            }
          }
        }
      })();
      
      // When Shiny connects, send the flag
      if (typeof Shiny !== 'undefined') {
        Shiny.addCustomMessageHandler('set_auth_cookie', function(message) {
          console.log('Setting cookie for', message.days, 'days');
          var date = new Date();
          date.setTime(date.getTime() + (message.days * 24 * 60 * 60 * 1000));
          var expires = 'expires=' + date.toUTCString();
          document.cookie = 'dash_auth=verified; ' + expires + '; path=/';
          console.log('Cookie set:', document.cookie);
        });
        
        // Check if we already have the flag and send it
        if (window.__dashAuthValid === true) {
          setTimeout(function() {
            console.log('✅ Sending cookie_auth_valid to Shiny');
            Shiny.setInputValue('cookie_auth_valid', true, {priority: 'event'});
          }, 100);
        }
      }
      "
    ))
  ),
  
  shiny::uiOutput("app_ui")
)

# ==============================================================================
# WRAPPER SERVER — handles login, then delegates to dashboard_server
# ==============================================================================
server <- function(input, output, session) {
  # Track authentication state
  authenticated <- shiny::reactiveVal(FALSE)

  # 1. Handle cookie-based auto-login
  shiny::observeEvent(
    input$cookie_auth_valid,
    {
      cat(
        "🔍 DEBUG: cookie_auth_valid triggered with value:",
        input$cookie_auth_valid,
        "\n"
      )
      if (isTRUE(input$cookie_auth_valid)) {
        cat("✅ DEBUG: Setting authenticated to TRUE via cookie\n")
        authenticated(TRUE)
      }
    },
    ignoreInit = TRUE
  )

  # 2. Render the appropriate UI
  output$app_ui <- shiny::renderUI({
    if (authenticated()) {
      dashboard_ui
    } else {
      login_ui
    }
  })

  # 3. Handle login
  shiny::observeEvent(input$login_btn, {
    cat("🔍 DEBUG: login_btn clicked.\n")
    cat("   - Password length:", nchar(input$login_password), "\n")
    cat("   - Remember me value:", input$remember_me, "\n")

    output$login_error <- shiny::renderUI(NULL)

    if (
      isTRUE(nchar(input$login_password) > 0) &&
        input$login_password == APP_PASSWORD
    ) {
      authenticated(TRUE)

      # If "Remember me" is checked, set the cookie for 7 days
      if (isTRUE(input$remember_me)) {
        cat("✅ DEBUG: Sending custom message to set cookie for 7 days\n")
        session$sendCustomMessage("set_auth_cookie", list(days = 7))
      } else {
        cat("⚠️ DEBUG: Remember me is FALSE or NULL\n")
      }
    } else {
      output$login_error <- shiny::renderUI({
        shiny::div(
          "Incorrect password. Please try again.",
          class = "login-error"
        )
      })
    }
  })

  # 4. Handle logout
  shiny::observeEvent(input$logout_btn, {
    cat("🔍 DEBUG: Logout clicked. Clearing cookie.\n")
    authenticated(FALSE)
    # Clear the cookie by setting its expiration to the past (-1 day)
    session$sendCustomMessage("set_auth_cookie", list(days = -1))
  })

  # 5. CRITICAL: Call dashboard_server ONLY AFTER the UI is rendered
  shiny::observeEvent(
    authenticated(),
    {
      if (authenticated()) {
        session$onFlushed(
          function() {
            dashboard_server(input, output, session)
          },
          once = TRUE
        )
      }
    },
    once = TRUE,
    ignoreInit = TRUE
  )
}
shiny::shinyApp(ui = ui, server = server)
