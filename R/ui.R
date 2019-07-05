#' Shiny app server object
#'
#' @importFrom graphics hist
#' @import shiny

# create the shiny application user interface
shinyAppUI <- fluidPage(
  shiny::titlePanel("Welcome to upthat, the Urban Planning and Transport Health Assessment Tool"),
  column(width = 3, 
         selectInput("mode", label = "Mode of transport",  choices = c("Walk", "Cycle")),
         sliderInput("rem", "Re-purposing of car parking spaces (% spaces removed)", min = 0, max = 100, value = 5),
         sliderInput("obs", "Investment (US $ millions):", min = 0, max = 50, value = 0.5, step = 0.1)
  ),
  column(width = 9, 
         leafletOutput("mymap")
  ),
  p(),
  actionButton("recalc", "Recalculate scenario")
)
