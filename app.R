# Aim: create simple leaflet shiny app for mvp

# if(!require(remotes)) {
#   install.packages("remotes")
# }
# remotes::install_cran("leaflet")
library(shiny)
library(leaflet)
devtools::install_github("atfutures-labs/upthat")

net = sf::read_sf("https://github.com/ATFutures/who3/releases/download/0.0.1/net.geojson")

ui <- fluidPage(
  shiny::titlePanel("Welcome to the ATT (name and contents are work in progress)"),
  column(width = 3, 
         selectInput("mode", label = "Mode of transport",  choices = c("Walk", "Cycle")),
         sliderInput("rem", "Re-purposing of car parking spaces", min = 0, max = 100, value = 5),
         sliderInput("obs", "Investment (US $ millions):", min = 0, max = 50, value = 0.5, step = 0.1)
         ),
  column(width = 9, 
         leafletOutput("mymap")
  ),
  p(),
  actionButton("recalc", "Recalculate scenario")
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