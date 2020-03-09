
# packages ----------------------------------------------------------------

remotes::install_github("zonebuilders/zonebuilder")
remotes::install_github("ITSLeeds/od")

library(zonebuilder)
library(osmdata)


# basic parameters --------------------------------------------------------

# idea: fundamental attributes per city

city_name = "accra"
has_od_data = FALSE
has_zone_data = FALSE


# get city data -----------------------------------------------------------

ne_cities = rnaturalearth::ne_download(type = "populated_places", returnclass = "sf", scale = 50)
nrow(ne_cities)
sum(ne_cities$POP_MAX) / 7e9 # around 20% of world's population

# for smaller cities
# ne_cities_10 = rnaturalearth::ne_download(type = "populated_places", returnclass = "sf", scale = 10)
# nrow(ne_cities_10)
# sum(ne_cities_10$POP_MAX, na.rm = TRUE) / 7e9 # around 33% of world's population

cities_match = grepl(pattern = city_name, x = ne_cities$NAME, ignore.case = TRUE)
n_city_name_in_city_data = sum(cities_match)

if(n_city_name_in_city_data == 1) {
  message("The city is in the city database" )
  city = ne_cities[cities_match, ]
}

region = osmdata::getbb(place_name = city_name, format_out = "sf_polygon")
if(is.null(region)) {
  message("Could not find boundary data from OSM. Getting data from another source.")
  if(city_name == "accra") {
    region = sf::read_sf("~/atfutures/who-data/accra/AccraMetroAdmin/AccraMetroAdmin2.shp")
  }
  # mapview::mapview(region)
}

city_centre = tmaptools::geocode_OSM(q = city_name, as.sf = TRUE)
mapview::mapview(region) + mapview::mapview(city_centre)
if(!has_zone_data) {
  city_zones = zonebuilder::zb_zone(x = city_centre, area = region)["label"]
  plot(city_zones)
  plot(city_zone_centroids, add = TRUE, col = "grey")
}

city_zone_centroids$distance_to_centre = as.numeric(sf::st_distance(
  city_zone_centroids, city_centre)[, 1])
city_zones$distance_to_centre = city_zone_centroids$distance_to_centre
plot(city_zones["distance_to_centre"])


# generate od data --------------------------------------------------------

od_region = od::points_to_od(city_zone_centroids, interzone_only = TRUE)
nrow(od_region) # 1560
od_region = od::od_oneway(od_region)
nrow(od_region) # 780
head(od_region)
od_sf = od::od_to_sf(x = od_region, z = city_zone_centroids)
plot(od_sf)


# generate distance data --------------------------------------------------

od_sf$distance_to_centre = city_zone_centroids$distance_to_centre[
  match(od_sf$d, city_zone_centroids$label)
]
plot(od_sf["distance_to_centre"])
plot(od_sf["distance_to_centre"][od_sf$distance_to_centre < 5000, ])

