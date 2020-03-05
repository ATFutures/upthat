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

region = getbb(place_name = city_name, format_out = "sf_polygon")
