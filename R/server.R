#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session The shiny session

#' @import leaflet
#' @import tmap
#' @export
# Define server logic required to draw a histogram
shinyAppServer = function(input, output, session) {

  if(is.na(mapdeck::mapdeck_tokens()[[1]][[1]])) {
    message("No mapdeck token found on system. Trying environment variable MAPBOX")
    mapdeck::set_token(token = Sys.getenv("MAPBOX"))
  }

  repo_sha = system("git rev-parse --short HEAD", intern = TRUE)
  output$app_info = renderText(
    paste(
      'Warning: this is not a stable version. Please do not distribute. Version'
      ,
      a(
        repo_sha,
        href = paste0("https://github.com/atfutures/upthat/tree/", repo_sha),
        target = '_blank'
      ),
      'released under the',
      a('GNU Affero GPL', href = "https://www.gnu.org/licenses/agpl-3.0.en.html", target = '_blank'),
      'and funded by the',
      a('WHO', href = "https://www.who.int/", target = "_blank")
    )
  )

  if ("net" %in% ls ()) rm (net)
  net = readRDS(system.file("net-kathmandu.Rds", package = "upthat"))
  rds_files_available = list.files(path = "inst", pattern = ".Rds", full.names = TRUE)

  output$mymap = mapdeck::renderMapdeck({
    mapdeck::mapdeck(style = "mapbox://styles/mapbox/light-v10")
  })


  observeEvent({input$city}, {

    matching_file = rds_files_available[grepl(pattern = input$city, x = rds_files_available, ignore.case = TRUE)]
    if (length(matching_file) == 1) {
      if ("net" %in% ls ()) rm (net)
      net = readRDS(matching_file)
    } else {
      message(length(matching_file),  " files found")
    }

    net$width <- 20 * net$flow / max (net$flow, na.rm = TRUE)
    mapdeck::mapdeck_update(map_id = "mymap") %>%
      mapdeck::clear_path(layer_id = "mylayer") %>%
      mapdeck::add_path(palette = "inferno", # see colourvalues::color_palettes()
                        net,
                        stroke_colour = "flow",
                        stroke_width = "width",
                        legend = TRUE,
                        layer_id = "mylayer")
  })

}
