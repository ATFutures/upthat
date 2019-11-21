#' launches the Urban Planning and Transport Health Assessment Tool app
#'
# wrapper for shiny::shinyApp()
#' @return shiny application object
#'
#' @import shiny
#' @export
runUpthat = function() {
  shinyApp(ui = shinyAppUI, server = shinyAppServer)
}
