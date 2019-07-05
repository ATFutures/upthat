if(file.exists("R/server.R")) {
  source("R/server.R")
  source("R/ui.R")
  shiny::shinyApp(ui = shinyAppUI, server = shinyAppServer)
}  else {
  upthat::runUpthat()
}