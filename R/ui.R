#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny
#' @import leaflet
#' @export
shinyAppUI <- fluidPage(
  shiny::titlePanel("Urban Planning and Transport Health Assessment Tool (upthat)"),
  column(12, shiny::htmlOutput("app_info")),
  column(width = 3,
         selectInput("city", label = "City",  choices = c("Accra", "Kathmandu", "Bristol", "NYC")),
         selectInput("mode", label = "Mode of transport",  choices = c("Walk", "Cycle", "Ebike", "Escooter", "Fly")),
         selectInput("scen", label = "Scenario",  choices = c("baseline", "cardiet")),
         sliderInput("rem", "Re-purposing of car parking spaces (% spaces removed)", min = 0, max = 100, value = 5),
         sliderInput("obs", "Investment (US $ millions):", min = 0, max = 50, value = 0.5, step = 0.1)
  ),
  column(width = 9,
         #tmap::tmapOutput("mymap")
         #leaflet::leafletOutput("mymap")
         mapdeck::mapdeckOutput ("mymap")
  ),
  p(),
  actionButton("recalc", "Recalculate scenario")
)
