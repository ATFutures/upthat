#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import leaflet
#' @export
shinyAppUI = navbarPage("Urban Planning and Transport Health Assessment Tool (upthat)",
  tabPanel("Maps",
    column(12, shiny::htmlOutput("app_info")),
    column(width = 3,
           selectInput("city", label = "City",  choices = c("Accra", "Kathmandu", "Bristol", "NYC")),
           selectInput("mode", label = "Mode of transport",  choices = c("Walk", "Cycle", "Ebike", "Escooter", "Fly")),
           selectInput("layer", label = "Layer",  choices = c("pedestrian flow", "exposure")),
           sliderInput("bus", "Number of bus stops added per 1,000", min = 0, max = 10, value = 1),
           sliderInput("obs", "Investment (US $ millions):", min = 0, max = 50, value = 0.5, step = 0.1)
    ),
    column(width = 9,
           mapdeck::mapdeckOutput ("mymap")
    ),
    p(),
    actionButton("recalc", "Zoom to city extent")
  ),
  tabPanel("Scenarios",
    column(width=3,
           selectInput("city", label = "City",  choices = c("Accra", "Kathmandu", "Bristol", "NYC")),
           br(),
           sliderInput("bus", "Number of bus stops added per 1,000", value = 1, min = 1, max = 10)
    ),
    column(width=9,
           selectInput("other", label = "Other",  choices = c("Accra", "Kathmandu", "Bristol", "NYC"))
    )
  )
)
