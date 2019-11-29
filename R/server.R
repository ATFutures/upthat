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
      plot_layer(net, input$layer, update_view = TRUE)
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
      plot_layer(net, input$layer, update_view = FALSE)
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
      plot_layer(net, input$layer, update_view = TRUE)
    }
    )
}

plot_layer = function(net, leg_title, update_view = FALSE) {
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

# Average global mortality from WHO database 
get_mortality <- function () {
    #x <- read.csv ("../who3/health-econ/who-mortality.csv")
    #names (x) <- c ("country", "population", "deaths", "remove")
    #x$remove <- NULL
    #x <- x [!is.na (x$deaths), ]
    #mortality <- mean (x$deaths / x$population)
    0.007425708
}

# Mode shift response based entirely on Accra walking statistics
mode_shift_response <- function (mode_incr = 0.01, city_pop, mortality) {
    # Accra data for distance walked to market
    d_market <- c (0.5, 1.5, 2.5, 4.5, 8.5)
    p_market <- c (0.273, 0.212, 0.061, 0.424, 0.03)
    d_market <- sum (d_market * p_market)

    # Accra data for distance walked to trotro
    d_tro <- c (0.25, 0.75, 1.5, 3.5, 7.5)
    p_tro <- c (0.832, 0.119, 0.023, 0.005, 0.022)
    d_tro <- sum (d_tro * p_tro)

    # Estimate of distance walked to work based on relative frequencies of trips
    # # to market and trotro
    d_work <- (0.474 * d_market + 0.19 * d_tro) / (0.474 + 0.19)

    # Accra data for numbers of weekly walking trips
    n_walk <- c (5, 15.5, 25.5, 35.5, 50.5, 80.5)
    p_walk <- c (0.64, 0.204, 0.062, 0.029, 0.034, 0.007)
    n_walk <- 7 * sum (n_walk * p_walk)

    # Reference weekly walking distance
    d_walk_ref <- (n_walk - 5) * d_tro + 2.5 * 0.474 * d_work + 2.5 * d_market

    # Change in daily distance walked to work in response to mode_incr for
    # walking
    d_work <- ((0.474 * (1 + mode_incr)) * d_market +
        (0.19 * (1 + mode_incr)) * d_tro) / (0.474 + 0.19)
    # Change in daily distance walked in general in response to mode_incr for
    # walking
    d_walk <- (n_walk - 5) * (1 + mode_incr) * d_tro +
        2.5 * 0.474 * (1 + mode_incr) * d_work +
        2.5 * (1 + mode_incr) * d_market

    # Overall risk ratio for change in distance walked:
    rr <- 0.114 * d_walk / d_walk_ref - 0.114
    mortality_here <- city_pop * mortality * rr
    data.frame (mode_shift = mode_incr,
                dist_ref = d_walk_ref,
                dist = d_walk,
                increase = d_walk / d_walk_ref - 1,
                rr = rr,
                d_mortality = mortality_here)
}

get_scenario_results <- function (city = "Accra", has_tram = FALSE) {
    nm <- "scenario-results-table.csv"
    f <- system.file (nm, package = "upthat")
    if (f == "") {
        u <- paste0 ("https://github.com/ATFutures/upthat/releases/",
                     "download/0.0.2/", nm)
        path <- dirname (system.file ("net.Rds", package = "upthat"))
        download.file (u, destfile = file.path (path, nm))
        f <- system.file (nm, package = "upthat")
    }
    mode_shift <- read.csv (f)
    mode_shift [tolower (mode_shift$City) == tolower (city) &
                mode_shift$has_tram == has_tram, ]
}

get_population <- function (city) {
    switch (city,
            "Accra" = 2.27e6,
            "Kathmandu" = 1.74e6)
}


calc_exposure <- function (city = "Accra", has_tram = FALSE) {
    # Assume fixed PM2.5 values as for Accra
    pm25bg <- 35
    pm25max <- 50

    mode_shift <- get_scenario_results (city = city, has_tram = has_tram)

    # pm25bg modified by reduction in car usage due to mode shift:
    car_red <- mode_shift$car / 100
    pm25bg_mod <- pm25bg + car_red * (pm25max - pm25bg) / pm25bg
    # presume average walking concentrations half way to max value:
    pm25walk <- (pm25max + pm25bg_mod) / 2

    response <- mode_shift_response (mode_incr = mode_shift$walking / 100,
                                     city_pop = get_population (city),
                                     mortality = get_mortality ())
                                     
    # average weekly concentration for reference case:
    walk_time <- response$dist_ref / 5.3
    non_walk_time <- 24 * 7 - walk_time
    pm25_ref <- (pm25bg_mod * non_walk_time + pm25walk * walk_time) / (24 * 7)

    # modified average weekly concentration for scenario
    walk_time <- response$dist / 5.3
    non_walk_time <- 24 * 7 - walk_time
    pm25_scenario <- (pm25bg_mod * non_walk_time + pm25walk * walk_time) / (24 * 7)
    # capped at 50, but all well below here, so can be left as is

    d_exposure <- pm25_scenario - pm25_ref
    response$exposure <- get_population (city) *
        get_mortality () * d_exposure * 0.07 / 10
    return (response)
}
