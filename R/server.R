#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

#' @import leaflet
# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {
  # if(!file.exists("net.geojson")) download.file("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson", "net.geojson")
  # net = sf::read_sf("net.geojson")
  net = sf::read_sf("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson")
  n = names(net)
  n_flow = n[grepl(pattern = "flow_", x = n)]
  
  
  # map %>%
  #   addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
  #               color = ~qpal(gdp_md_est)
  #   ) %>%
  #   addLegend(pal = qpal, values = ~gdp_md_est, opacity = 1)
  
  output$mymap <- renderLeaflet({
    n_to_show = n_flow[grepl(pattern = input$mode, n_flow, ignore.case = TRUE)]
    qpal = colorBin("RdYlBu", net[[n_to_show]], n = 5)
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolylines(data = net, weight = round(net[[n_to_show]] / mean(net[[n_to_show]]) * 5), color = ~qpal(net[[n_to_show]])) %>% 
      addLegend(pal = qpal, values = net[[n_to_show]], opacity = 1)
  })
}
