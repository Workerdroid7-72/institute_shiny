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
    ),
    # --- REFRESH + LOGOUT BUTTONS (FIXED VISIBILITY) ---
    shiny::div(
      style = "position: fixed; right: 20px; top: 12px; z-index: 1050; display: flex; gap: 8px;",
      shiny::actionButton(
        "refresh_btn",
        "Refresh data",
        icon = shiny::icon("rotate"),
        class = "btn-sm",
        style = "background-color: rgba(255, 255, 255, 0.15); border: 1px solid rgba(255, 255, 255, 0.4); color: #ffffff;"
      ),
      shiny::actionButton(
        "logout_btn",
        "Logout",
        icon = shiny::icon("right-from-bracket"),
        class = "btn-sm",
        style = "background-color: rgba(255, 255, 255, 0.15); border: 1px solid rgba(255, 255, 255, 0.4); color: #ffffff;"
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
  # Refresh button: drop the shared view cache and bump this session's counter
  # so every reactive using view_data() re-fetches.
  session$userData$data_version <- shiny::reactiveVal(0)
  shiny::observeEvent(input$refresh_btn, {
    refresh_views()
    session$userData$data_version(session$userData$data_version() + 1)
    shiny::showNotification("Data refreshed.", type = "message", duration = 3)
  })

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

  # Pressing Enter in the password box clicks Login. Delegated handler because
  # the form is created by renderUI after page load; 'change' makes Shiny send
  # the typed password immediately instead of after its ~250 ms debounce.
  shiny::tags$script(shiny::HTML(
    "
    $(document).on('keydown', '#login_password', function(e) {
      if (e.key === 'Enter') {
        $(this).trigger('change');
        $('#login_btn').click();
      }
    });
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

  # --- AUTH COOKIE SCRIPT ---
  # The cookie holds a server-signed token (see make_auth_token in global.R).
  # The browser only stores and returns it; the server decides if it is valid.
  shiny::tags$head(
    shiny::tags$script(shiny::HTML(
      "
      $(document).on('shiny:connected', function() {
        Shiny.addCustomMessageHandler('set_auth_cookie', function(message) {
          var date = new Date();
          date.setTime(date.getTime() + (message.days * 24 * 60 * 60 * 1000));
          document.cookie = 'dash_auth=' + (message.token || '') +
            '; expires=' + date.toUTCString() +
            '; path=/; SameSite=Lax' +
            (location.protocol === 'https:' ? '; Secure' : '');
        });

        var match = document.cookie.match(/(?:^|;\\s*)dash_auth=([^;]+)/);
        if (match) {
          Shiny.setInputValue('auth_token', match[1], {priority: 'event'});
        }
      });
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
    input$auth_token,
    {
      if (verify_auth_token(input$auth_token)) {
        authenticated(TRUE)
      }
    },
    # The browser sends the cookie token as part of the session's initial
    # inputs, so it must NOT be skipped as "init" (a missing cookie is NULL,
    # which observeEvent already ignores).
    ignoreInit = FALSE
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
    output$login_error <- shiny::renderUI(NULL)

    if (
      nzchar(APP_PASSWORD) &&
        isTRUE(nchar(input$login_password) > 0) &&
        identical(input$login_password, APP_PASSWORD)
    ) {
      authenticated(TRUE)

      # If "Remember me" is checked, issue a signed token valid for 7 days
      if (isTRUE(input$remember_me)) {
        session$sendCustomMessage(
          "set_auth_cookie",
          list(days = 7, token = make_auth_token(7))
        )
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
    authenticated(FALSE)
    # Clear the cookie by setting its expiration to the past (-1 day)
    session$sendCustomMessage("set_auth_cookie", list(days = -1, token = ""))
    # Reload so the next login starts a fresh session (no stale filters and the
    # dashboard server starts again); the cookie is already cleared above.
    session$reload()
  })

  # 5. CRITICAL: Call dashboard_server ONLY AFTER the UI is rendered
  # A flag (not once/ignoreInit) so this also works when `authenticated` is
  # already TRUE at page load (cookie login) as well as when it turns TRUE
  # after a password login.
  dashboard_started <- FALSE
  shiny::observeEvent(authenticated(), {
    if (authenticated() && !dashboard_started) {
      dashboard_started <<- TRUE
      session$onFlushed(
        function() {
          dashboard_server(input, output, session)
        },
        once = TRUE
      )
    }
  })
}
shiny::shinyApp(ui = ui, server = server)
