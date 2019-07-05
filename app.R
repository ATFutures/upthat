library(upthat)
if(file.exists("R/server.R")) {
  source("R/server.R")
  source("R/ui.R")
  shiny::shinyApp(ui = shinyAppUI, server = shinyAppServer)
}  else {
  download.file("https://github.com/atfutures-labs/upthat/archive/master.zip", "master.zip")
  unzip("master.zip", exdir = ".")
  source("upthat-master/R/server.R")
  source("upthat-master/R/ui.R")
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
