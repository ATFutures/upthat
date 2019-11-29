#' Shiny app server function
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @param session The shiny session

#' @export
# Define server logic required to draw a histogram
shinyAppServer = function(input, output, session) {
  if (is.na(mapdeck::mapdeck_tokens()[[1]][[1]])) {
    message("No mapdeck token found on system. Trying environment variable MAPBOX")
    mapdeck::set_token(token = Sys.getenv("MAPBOX"))
  }

  repo_sha = system("git rev-parse --short HEAD", intern = TRUE)
  output$app_info = renderText(
    paste(
      "Warning: this is not a stable version. Please do not distribute. Version",
      a(
        repo_sha,
        href = paste0("https://github.com/atfutures/upthat/tree/", repo_sha),
        target = "_blank"
      ),
      "released under the",
      a("GNU Affero GPL", href = "https://www.gnu.org/licenses/agpl-3.0.en.html", target = "_blank"),
      "and funded by the",
      a("WHO", href = "https://www.who.int/", target = "_blank")
    )
  )

  rds_files_available = c(
    list.files(pattern = "net-", full.names = TRUE),
    list.files(path = "inst", pattern = "net-", full.names = TRUE)
  )
  message(
    "Found these Rds files: ",
    paste0(rds_files_available, collapse = ", ")
  )

  ggplot2::theme_set (ggplot2::theme_minimal ())

  output$mymap = mapdeck::renderMapdeck({
    mapdeck::mapdeck(style = "mapbox://styles/mapbox/light-v10")
  })

  observeEvent(
    {
      input$city
    },
    {
      matching_file = rds_files_available[grepl(pattern = input$city, x = rds_files_available, ignore.case = TRUE)]
      if (length(matching_file) > 1) {
        mmsg = paste(matching_file, collapse = ", ")
        message(length(matching_file), " files found:", mmsg, "selecting the first")
        matching_file = matching_file[1]
      }
      if (length(matching_file) < 1) { # if there are no matches
        matching_file = system.file("net.Rds", package = "upthat")
      }
      message("Reading this matching file: ", matching_file)
      net <<- readRDS(matching_file)
      net$layer = net$flow
      plot_map(net, input$layer, update_view = TRUE)
    }
  )

  observeEvent(
    {
      input$layer
    },
    {
      if (input$layer == "pedestrian flow") {
        net$layer = net$flow
      } else if (input$layer == "exposure") {
        if ("exposure" %in% names(net)) {
          net$layer = net$exposure
        } else {
          net$layer = net$flow
        }
      }
      plot_map(net, input$layer, update_view = FALSE)
    }
  )

  observeEvent(
    eventExpr = {input$recalc},
    {
      if (input$layer == "pedestrian flow") {
        net$layer = net$flow
      } else if (input$layer == "exposure") {
        if ("exposure" %in% names(net)) {
          net$layer = net$exposure
        } else {
          net$layer = net$flow
        }
      }
      plot_map(net, input$layer, update_view = TRUE)
    }
    )

  x <- reactive ({
    g <- plot_chart (city = input$city_sc)
    return (g)
  })
  output$plot = renderPlot ({
    print (x ())
  })

}

plot_map = function(net, leg_title, update_view = FALSE) {
  net$width = 100 * net$layer / max(net$layer, na.rm = TRUE)
  cols = rgb(colourvalues::get_palette("inferno"), maxColorValue = 255)
  variables = seq(min(net$layer), max(net$layer), length.out = 5)
  if (variables [1] < 1e-6) {
    variables [1] = 0
  }
  variables = signif(variables, digits = 2)
  index = seq(1, length(cols), length.out = length(variables))
  leg = mapdeck::legend_element(
    variables = variables,
    colours = cols [index],
    colour_type = "fill",
    variable_type = "gradient",
    title = leg_title
  )
  mapdeck::mapdeck_update(map_id = "mymap") %>%
    mapdeck::add_path(
      palette = "inferno", # see colourvalues::color_palettes()
      net,
      stroke_colour = "layer",
      stroke_width = "width",
      stroke_opacity = "layer",
      legend = leg,
      update_view = update_view,
      layer_id = "mylayer"
    )
}

plot_chart = function (city) {
    x <- calc_exposure (city = city, has_tram = FALSE)
    x$mortality_reduction <- x$d_mortality - x$exposure
    ggplot2::ggplot (x, ggplot2::aes (x = bus_stops_per_1000,
                                           y = mortality_reduction)) +
        ggplot2::geom_point () + 
        ggplot2::geom_smooth (method = "lm") +
        ggplot2::theme (axis.title.y = ggplot2::element_text (angle = 90))
}
