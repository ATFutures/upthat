
# packages ----------------------------------------------------------------

remotes::install_github("zonebuilders/zonebuilder")
remotes::install_github("ITSLeeds/od")

library(zonebuilder)
library(osmdata)
library(tmap)
tmap_mode("view")

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
}
# filter out tiny zones
city_zones$area = as.numeric(sf::st_area(city_zones))
plot(city_zones$area)
# area_zone_1 = city_zones$area[2] # removes all but 'full' zones
area_zone_1 = 1e6 # removes all but 'full' zones
summary(city_zones$area)
city_zones = city_zones[city_zones$area >= area_zone_1, ]
mapview::mapview(city_zones)
# get centroids
city_zone_centroids = sf::st_centroid(city_zones)  # simple approach

library(geofabrik)
mapview::mapview(geofabrik_zones)
regional_network = geofabrik::get_geofabrik(name = city_zones)
city_network = regional_network[city_zones, ]
city_transport_network = city_network[!is.na(city_network$highway), ]
mapview::mapview(city_transport_network) # yes, that's the navigable network!

city_zone_centroids$distance_to_centre = as.numeric(
  sf::st_distance(city_zone_centroids, city_centre)[, 1]
  ) / 1000
city_zones$distance_to_centre = city_zone_centroids$distance_to_centre
plot(city_zones["distance_to_centre"])


# generate od data --------------------------------------------------------

od_region = od::points_to_od(city_zone_centroids, interzone_only = TRUE)
nrow(od_region) # 1560
od_region = od::od_oneway(od_region)
nrow(od_region) # 780
head(od_region)


# switch origin and destination order so destination is closer to centre
od_region = od_region[2:1]
names(od_region) = c("o", "d")
# od_sf = od::od_to_sf(x = od_region, z = city_zone_centroids)
# use sf version for correct crs
od_sf = od::od_to_sf(x = od_region, z = city_zone_centroids, package = "sf")
# with offsets
od_sf = od::od_to_sf_network(
  x = od_region,
  z = city_zones,
  package = "sf",
  network = city_network
)
sf::st_crs(od_sf)
plot(od_sf)

# generate distance data --------------------------------------------------

od_sf$distance = as.numeric(sf::st_length(od_sf)) / 1000
od_sf$distance_to_centre_d = city_zone_centroids$distance_to_centre[
  match(od_sf$d, city_zone_centroids$label)
]
od_sf$distance_to_centre_o = city_zone_centroids$distance_to_centre[
  match(od_sf$o, city_zone_centroids$label)
]

plot(od_sf)
plot(od_sf["distance"])
plot(od_sf["distance_to_centre_o"])
plot(od_sf["distance_to_centre_o"][od_sf$distance_to_centre_o < 5, ])
summary(od_sf$o <= od_sf$d) # point closes to centre is always 'destination'


# estimate od flow --------------------------------------------------------

# piggyback::pb_list()
# piggyback::pb_download_url("pred-m9-bristol-sim-test.Rds")
# m = readRDS(url("https://github.com/ATFutures/upthat/releases/download/0.0.3.1/pred-m9-bristol-sim-test.Rds"))
# m = readRDS("pred-m9-bristol-sim-test.Rds")
od_sf$all_predicted = predict(m, od_sf)
od_sf = od_sf[order(od_sf$all_predicted), ]
plot(od_sf["all_predicted"], lwd = od_sf$all_predicted / mean(od_sf$all_predicted) / 3 )


# routing -----------------------------------------------------------------

remotes::install_github("GIScience/openrouteservice-r")
library(openrouteservice)

min_n_flows = 500
summary(od_sf$all_predicted) # most flows have less than 100 estimated trips
od_sf_top = od_sf[od_sf$all_predicted >= min_n_flows, ]
plot(od_sf_top["all_predicted"])
nrow(od_sf_top) # 71
od_sf_top_o = sf::st_coordinates(lwgeom::st_startpoint(od_sf_top$geometry))
od_sf_top_d = sf::st_coordinates(lwgeom::st_endpoint(od_sf_top$geometry))
od_sf_top_coordinates = as.matrix(data.frame(
  od_sf_top_o,
  od_sf_top_d
))
head(od_sf_top_coordinates)
x = ors_directions(list(od_sf_top_o[1, ], od_sf_top_d[1, ]), output = "sf")
mapview::mapview(x)
x_list = pbapply::pblapply(X = 1:nrow(od_sf_top), FUN = function(i) {
  message(i)
  ors_directions(list(od_sf_top_o[i, ], od_sf_top_d[i, ]), output = "sf",
                 profile = "cycling-regular")
})
od_routes = do.call(rbind, x_list)
mapview::mapview(od_routes)
od_routes$summary

od_sf_top_routes = sf::st_sf(
  sf::st_drop_geometry(od_sf_top),
  geometry = od_routes$geometry
)
mapview::mapview(od_sf_top_routes)
od_sf_top_routes$length = sf::st_length(od_sf_top_routes) %>% as.numeric()
od_sf_top_routes$go_dutch = pct::uptake_pct_godutch(
  distance = od_sf_top_routes$length,
  gradient = rep(0, nrow(od_sf_top_routes))
) * od_sf_top_routes$all_predicted

rnet = stplanr::overline(od_sf_top_routes, "go_dutch")
tm_shape(rnet) +
  tm_lines(lwd = "go_dutch", scale = 9, col = "go_dutch",
           palette = "RdYlBu")

# tests with ors
x = ors_directions(list(od_sf_top_o[1, ], od_sf_top_d[1, ]))
x$features[[1]]$properties$segments[[1]]$duration / 60 # 15 minutes
x$features[[1]]$properties$segments[[1]]$steps[[1]]$type # 15 minutes
x_sf = sf::st_sf(
  data.frame(duration = x$features[[1]]$properties$segments[[1]]$duration),
  geometry = sf::st_sfc(sf::st_linestring(x$features[[1]]$geometry$coordinates)),
  crs = 4326)
mapview::mapview(x_sf)
# x = stplanr::route(l = od_sf_top[1, ], route_fun = ors_directions, output = "sf")



# testing the googleway pkg
library(googleway)
key = Sys.getenv("GOOGLEDIST")
df = google_directions(origin = "Melbourne, Australia",
                        destination = "Sydney, Australia",
                        key = key,
                        mode = "driving",
                        simplify = TRUE)
names(df)
class(df)
df$geocoded_waypoints
df$routes
access_result(df, result = "elev_location")
googleway::elevation()
df
googleway::direction_polyline(res = df)
directions_sf = googlePolylines::decode(polylines = .Last.value)
class(directions_sf)


# tests with network data -------------------------------------------------


# mapview::mapview(city_transport_network) # shows the network
city_transport_network = city_transport_network[
  !grepl(pattern = '"boundary"=>"administrative"', city_transport_network$other_tags),
]
summary(as.factor(city_transport_network$highway))
city_network_no_highway = city_transport_network[is.na(city_transport_network$highway), ]
mapview::mapview(city_network_no_highway) # not really navigable ways
plot(city_zone_centroids, add = TRUE, col = "grey")

# get centroids sampled on network
network = city_transport_network
zone = city_zones[1, ]
n = 1
i = 1
sample_in_zone = function(network, zone, n, i = 1) {
  network_sample = sf::st_intersection(network, zone[i, ])
  res = sf::st_sample(x = network_sample, size = n)
  res[!sf::st_is_empty(res)]
}
sample_from_zone_1 = sample_in_zone(network = city_transport_network, zone = zone, n = 1)
mapview::mapview(sample_from_zone_1)
centroids_on_network_list = lapply(
  1:nrow(city_zones),
  sample_in_zone,
  network = city_transport_network,
  zone = city_zones,
  n = 1
)
centroids_on_network = do.call(c, centroids_on_network_list)
mapview::mapview(city_zones) +
  mapview::mapview(centroids_on_network) +
  mapview::mapview(city_zone_centroids)
