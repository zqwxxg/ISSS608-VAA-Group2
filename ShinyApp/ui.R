library(shiny)
library(shinyjs)
library(bslib)
library(tmap)

### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . ..
### Colors                                                                  ####

#134178 blue
#f0f5fa light blue
#071f4d dark blue
#dad9d6 grey
#f4f3f2 light grey
#96816d brown

my_theme <- bs_theme(
  version = 5,                   # Bootstrap 5
  bootswatch = "flatly",         # Preset theme
  bg = "#f0f5fa",                # Background color
  fg = "#134178",                # Foreground (text) color
  primary = "#071f4d",           # Primary color (nav links, active tabs)
  secondary = "#96816d",         # Secondary (accents, cards)
  success = "#B2B2B2",           # Custom success color (maybe for neutral feel)
  
  base_font = font_google("Raleway"),       # Body text
  heading_font = font_google("Montserrat")  # Headings
)

interpolation_sidebar <- function() {
  div(
  
    radioButtons(
      inputId = "interpolation-granularity",
      label = "Select Granularity:",
      choices = c("Daily", "Monthly", "Yearly"),
      selected = "Monthly",
      inline = TRUE
    ),
    
    uiOutput("interpolation-date_picker"),
    
    actionButton("submit_interpolation", "Submit", icon = icon("cloud-rain"), class = "btn-primary", style = "width: 100%")
  )
}

ui <- (
  page_navbar(title = "RainSense",
              theme = my_theme,
              id = "main_navbar",
              
              header = tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
              ),
              
              # ---------- Home Panel ----------
              nav_panel(
                "Home",
                div(class = "landing-section",
                    h1("Welcome to RainSense"),
                    p("RainSense is an interactive platform designed to help you explore rainfall variability and forecasting patterns across Singapore. Dive into our visual and analytical tools to uncover insights from historical and projected rainfall data."),
                    br(),
                    layout_columns(
                      col_widths = c(6, 6),
                      
                      div(
                        p(tags$b("Confirmatory Analysis: "),
                          "Compare daily rainfall across different monsoon seasons and uncover trends in extreme rainfall events through statistical analysis."
                        ),
                        br(),
                        p(tags$b("Spatial Analysis: "),
                          "Visualize rainfall distribution across regions and identify spatial patterns that may influence flood risks or drought zones."
                        ),
                        br(),
                        p(tags$b("Forecasting: "),
                          "Detect rainfall anomalies and generate future rainfall forecasts using data-driven models, helping you anticipate wet or dry periods ahead of time."
                        )
                        ),

                      div(
                        # tmapOutput("station_map")
                        )
                      )
                    )
                ),
              
              # ---------- Spatial Analysis Tab ----------
              nav_panel(
                title = "Spatial Analysis",
                id = "spatial_panel",
                navset_card_tab(
                  nav_panel(
                  "Interpolation",
                  page_fillable(
                    layout_sidebar(
                      sidebar = interpolation_sidebar(),
                      tmapOutput("rain_map"),
                    ))
                  ),

                  nav_panel(
                  "Clustering", "test")
                  )
                )
              )
)
# ui <- (
#   navbarPage(title = "RainSense",
#              fluid = TRUE,
#              collapsible = TRUE,
#              header = tags$head (
#                tags$script(HTML("$(document).on('click', '#toggleSidebar', function () {$('.custom-sidebar').toggleClass('collapsed');});"))
#                ),
#              theme = "styles.css",
#                    
#              # ----------------------------------
#              # tab panel 1 - Home
#              tabPanel("Home",
#                       div(class = "landing-section",
#                           div(style = "padding-bottom: 30px;",
#                             fluidRow(
#                               column(12,
#                                      h1("Welcome to RainSense"),
#                                      p("RainSense is an interactive platform designed to help you explore rainfall variability and forecasting patterns across Singapore. Dive into our visual and analytical tools to uncover insights from historical and projected rainfall data.")
#                                      )
#                               )
#                             ),
#                           fluidRow(
#                             column(6,
#                                    p(tags$b("Confirmatory Analysis"),
#                                      "Compare daily rainfall across different monsoon seasons and uncover trends in extreme rainfall events through statistical analysis."
#                                    ),
#                                    
#                                    br(),
#                                    
#                                    p(tags$b("Spatial Analysis: "),
#                                      "Visualize rainfall distribution across regions and identify spatial patterns that may influence flood risks or drought zones."
#                                    ),
#                                    
#                                    br(),
#                                    
#                                    p(tags$b("Forecasting: "),
#                                      "Detect rainfall anomalies and generate future rainfall forecasts using data-driven models, helping you anticipate wet or dry periods ahead of time."
#                                    )
#                                    ),
#                             # column(6,
#                             #        tmapOutput("station_map")
#                             #        )
#                             )
#                           )
#                       ),
#              
#              # ----------------------------------
#              # tab panel 3 - Spatial Analysis
#              tabPanel("Spatial Analysis",
#                       div(class = "page-container",
#                           fluidRow(
#                             column(3,
#                                    div(id = "sidebar", class = "custom-sidebar",
#                                        # Toggle button at the top of the sidebar
#                                        tags$button(id = "toggleSidebar", class = "toggle-btn", icon("bars")),
#                                        tags$div(id = "sidebarContent",
#                                                 h4("Rainfall Analysis"),
#                                                 selectInput("granularity", "Select Granularity:", choices = c("Monthly", "Daily")),
#                                                 uiOutput("date_selector")
#                                        )
#                                    )
#                             )
#                           )
#                             
#                           
#                        #??? Main map output
#                        # column(9,
#                        #   tmapOutput("rain_map", height = "700px")
#                        #   )
#                        )
#                       )
#              )
# )
  