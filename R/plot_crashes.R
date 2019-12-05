plot_crashes = function(map, crashes) {
  map %>%
    mapdeck::add_sf(
      data = crashes,
      radius_max_pixels = 20,
      radius_min_pixels = 5,
      fill_colour = "crash_severity",
      legend = TRUE,
      palette = "rdylbu"
    )}
