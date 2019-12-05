
city_name = "accra"
crashes_file_name = paste0("crashes-simulated-", city_name, ".Rds")

serious_levels = c("Slight", "Serious", "Fatal")
numeric_to_levels = function(n) serious_levels[n]

crashes = readRDS(crashes_file_name)
head(colourvalues::color_values(crashes$crash_severity))
crashes$severity = factor(
  crashes$crash_severity,
  levels = serious_levels
  )
crashes$crashes_numeric = as.numeric(crashes$severity)
numeric_to_levels(crashes$crashes_numeric)


library(mapdeck)

mapdeck(style = 'mapbox://styles/mapbox/dark-v9', pitch = 45 ) %>%
  mapdeck::add_sf(
    data = crashes,
    radius_max_pixels = 20,
    radius_min_pixels = 5,
    fill_colour = "crash_severity",
    legend = TRUE,
    palette = "rdylbu"
  )

# as a function to add on to an existing map  -----------------------------

map = mapdeck(style = 'mapbox://styles/mapbox/dark-v9', pitch = 45 )

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

map %>% plot_crashes(crashes = crashes)

l1 = legend_element(
  variables = serious_levels
  , colours = c("#00FF00FF", "#FF0000FF")
  , colour_type = "fill"
  , variable_type = "category"
)
js = mapdeck_legend(l1)


