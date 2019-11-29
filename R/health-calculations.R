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
        utils::download.file (u, destfile = file.path (path, nm))
        f <- system.file (nm, package = "upthat")
    }
    mode_shift <- utils::read.csv (f)
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

    mode_shift <- cbind (mode_shift, response [, -1])
                                     
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
    mode_shift$exposure <- get_population (city) *
        get_mortality () * d_exposure * 0.07 / 10
    return (mode_shift)
}
