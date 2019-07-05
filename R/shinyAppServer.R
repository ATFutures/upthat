#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

#' @import leaflet
# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {
  
  library(leaflet)
  
  net = sf::read_sf("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson")
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2, rnorm(40) + 6)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolylines(data = net)
  })
}
