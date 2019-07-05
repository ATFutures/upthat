#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

#' @import leaflet
# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {
  
  
  library(leaflet)
  
  net = sf::read_sf("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson")
  n = names(net)
  n_flow = n[grepl(pattern = "flow_", x = n)]
  
  output$mymap <- renderLeaflet({
    n_to_show = n_flow[grepl(pattern = input$mode, n_flow, ignore.case = TRUE)]
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolylines(data = net, weight = round(net[[n_to_show]] / mean(net[[n_to_show]]) * 5))
  })
}
