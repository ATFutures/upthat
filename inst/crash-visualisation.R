
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

l1 = legend_element(
  variables = serious_levels
  , colours = c("#00FF00FF", "#FF0000FF")
  , colour_type = "fill"
  , variable_type = "category"
)
js = mapdeck_legend(l1)




mapdeck(style = 'mapbox://styles/mapbox/dark-v9', pitch = 45 ) %>%
  mapdeck::add_sf(
    data = crashes,
    radius_max_pixels = 20,
    radius_min_pixels = 5,
    fill_colour = "crash_severity",
    legend = TRUE,
    palette = "rdylbu"
  )
