# Aim: create simple leaflet shiny app for mvp

# if(!require(remotes)) {
#   install.packages("remotes")
# }
# remotes::install_cran("leaflet")
library(shiny)
library(leaflet)

net = sf::read_sf("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson")
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  shiny::titlePanel("Welcome to the ATT (name and contents are work in progress)"),
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2, rnorm(40) + 6)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addPolylines(data = net)
  })
}

shinyApp(ui, server)