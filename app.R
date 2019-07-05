library(upthat)
source("R/server.R")
source("R/ui.R")
shiny::shinyApp(ui = shinyAppUI, server = shinyAppServer)
