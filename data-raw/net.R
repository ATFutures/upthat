## code to prepare `net` dataset goes here

# Aim: get minimum cycle network in city

# examaple:
net = readRDS(system.file("net.Rds", package = "upthat"))
names(net)
head(net)

# Parameters
city_name = "Accra"
city_codename = "accra"
size_net = 2000
osm_variables_to_keep = c("osm_id", "name", "highway", "access", "bicycle", "oneway.bicycle", "cycleway", "foot", "maxspeed", "oneway")
schema_components = c("mode", "scenario", "purpose") # could add frequency/time of day
modes = c("cycle", "walk")
scenarios = c("baseline", "cardiet")
purpose = c("all")
# flow_variables = c("walk_baseline_all", "cycle_baseline_all")
flow_variables = apply(expand.grid(modes, scenarios, purpose), 1, paste, collapse="_")


library(osmdata)

lonlat2UTM = function(lonlat) {
  utm = (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

city_centre_coords = stplanr::geo_code(city_name)
city_centre_buffer = city_centre_coords %>% sf::st_point() %>% sf::st_sfc(crs = 4326) %>%
  sf::st_transform(crs = lonlat2UTM(city_centre_coords)) %>%
  sf::st_buffer(dist = size_net) %>%
  sf::st_transform(4326)

# mapview::mapview(city_centre_buffer)

osm = opq(city_name) %>%
  add_osm_feature("highway") %>%
  osmdata_sf()

# sf:::plot.sf(osm$osm_lines)
# mapview::mapview(osm$osm_lines)

vars_to_keep = names(osm$osm_lines)[names(osm$osm_lines) %in% osm_variables_to_keep]
net = osm$osm_lines[city_centre_buffer, vars_to_keep]
mapview::mapview(net)
net_spln = stplanr::SpatialLinesNetwork(net)
f = igraph::edge.betweenness(net_spln@g)
net$flow = f
plot(net["flow"])
for(i in flow_variables) {
  net[[i]] = runif(nrow(net), min = 0, max = max(f) / 3)
}
plot(net[grepl(pattern = "_all", x = names(net))])
saveRDS(net, paste0("net-", city_codename, ".Rds"))

# sf::write_sf(net, "net.geojson")
# piggyback::pb_upload("net.geojson")

# getting city centre via centroid: unreliable
# b = getbb(city_name, format_out = "sf_polygon")
# b_projected = sf::st_transform(b, lonlat2UTM(sf::st_coordinates(b)))
# mapview::mapview(b_projected)
# b_point = sf::st_centroid(b)

# usethis::use_data("DATASET")
