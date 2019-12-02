# Aim: generate synthetic crash data on the road network


# setup -------------------------------------------------------------------

library(dplyr)
library(sf)

city_name = "kathmandu"
n = 500 # how many crashes?

# how much more likely are serious and fatal crashes relative to UK data?
serious_fatal_proportion_inflator = c(Serious = 2, Fatal = 3)


# attribute data ----------------------------------------------------------

original_crash_data_uk = stats19::get_stats19(year = 2018, type = "ac")
names(original_crash_data_uk)
variables_to_keep = c("accident_severity", "number_of_vehicles", "day_of_week", "time")
crash_data_uk = original_crash_data_uk[variables_to_keep]
names(crash_data_uk)[1] = "crash_severity"

crash_data_uk$weight = serious_fatal_proportion_inflator[crash_data_uk$crash_severity]
crash_data_uk$weight[is.na(crash_data_uk$weight)] = 1
summary(crash_data_uk$weight)

simulated_crashes = crash_data_uk %>%
  sample_n(size = n, weight = weight) %>%
  select(-weight)

# geographic data ---------------------------------------------------------

net = readRDS(paste0("net-", city_name, ".Rds"))
net_sam = net[sample(nrow(net), size = n, prob = net$flow), ]
crs_new = stplanr::geo_select_aeq(net_sam)
# simulated_crash_points = st_line_sample(net_sam, n = n) # fail
simulated_crash_points = net_sam %>%
  st_transform(crs_new) %>%
  st_line_sample(n = n, type = "random", sample = n) %>%
  st_transform(st_crs(net_sam))
class(simulated_crash_points)
plot(simulated_crash_points)

# combine and save --------------------------------------------------------

crashes = st_sf(simulated_crashes, geometry = simulated_crash_points)
crashes_file_name = paste0("crashes-simulated-", city_name, ".Rds")
saveRDS(crashes, crashes_file_name)
piggyback::pb_upload(crashes_file_name)

# out-takes

# shows how verbose sampling can be in base R
s = sample(nrow(crash_data_uk), size = n, prob = crash_data_uk$weight)
