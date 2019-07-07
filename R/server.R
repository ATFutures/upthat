#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny

#' @import leaflet
#' @import tmap
# Define server logic required to draw a histogram
shinyAppServer <- function(input, output, session) {

  library(tmap)


  # if(!file.exists("net.geojson")) download.file("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson", "net.geojson")
  # net = sf::read_sf("net.geojson")
  net = sf::read_sf("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson")
  # net = readRDS(system.file("net.Rds", package = "upthat"))
  n = names(net)
  n_flow = n[grepl(pattern = "flow_", x = n)]

  # map %>%
  #   addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
  #               color = ~qpal(gdp_md_est)
  #   ) %>%
  #   addLegend(pal = qpal, values = ~gdp_md_est, opacity = 1)

  repo_sha = system("git rev-parse --short HEAD", intern = TRUE)
  output$app_info = renderText(paste('Version', a(repo_sha, href = paste0("https://github.com/npct/pct-shiny/tree/", repo_sha), target = '_blank'),
                          'released under the', a('GNU Affero GPL', href = "https://www.gnu.org/licenses/agpl-3.0.en.html", target = '_blank'), 'and funded by the',
                          a('WHO', href = "https://www.who.int/", target = "_blank")))

  mytmap <- tmap::renderTmap({
    qtm(net)
  })

  mymap <- renderLeaflet({
    n_to_show = n_flow[grepl(pattern = input$mode, n_flow, ignore.case = TRUE)]
    qpal = colorBin("RdYlBu", net[[n_to_show]], n = 5)
    leaflet() %>%
      addProviderTiles(leaflet::providers$OpenStreetMap.BlackAndWhite, options = providerTileOptions(noWrap = TRUE)) %>%
      addPolylines(data = net, weight = round(net[[n_to_show]] / mean(net[[n_to_show]]) * 5), color = ~qpal(net[[n_to_show]])) %>%
      addLegend(pal = qpal, values = net[[n_to_show]], opacity = 1)
  })

  observe({
    if(input$pkg == "tmap") {
      output$mymap = mytmap
    } else {
      output$mymap = mymap
    }
  })
}
