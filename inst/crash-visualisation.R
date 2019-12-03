
city_name = "accra"
crashes_file_name = paste0("crashes-simulated-", city_name, ".Rds")

crashes = readRDS(crashes_file_name)
head(colourvalues::color_values(crashes$crash_severity))
crashes$severity = factor(
  crashes$crash_severity,
  levels = c("Slight", "Serious", "Fatal")
  )
crashes$crashes_numeric = as.numeric(crashes$severity)


library(mapdeck)
mapdeck(style = 'mapbox://styles/mapbox/dark-v9', pitch = 45 ) %>%
  mapdeck::add_sf(
    data = crashes,
    radius_max_pixels = 20,
    radius_min_pixels = 5,
    fill_colour = "crashes_numeric"
  )

