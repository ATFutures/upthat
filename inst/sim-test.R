# Aim: test out spatial interaction model/data fit


# set-up ------------------------------------------------------------------

devtools::install_github("itsleeds/geofabric")
devtools::install_github("itsleeds/pct")
library(tidyverse)
library(gravity)
library(brms)
library(tmap)

city_name = "Bristol"

# test data from bristol --------------------------------------------------

z = spDataLarge::bristol_zones
zc = st_centroid(z) # centroids
od_region = spDataLarge::bristol_od %>%
  filter(o != d)
l = stplanr::od2line(od_region, z)

plot(l$geometry, lwd = l$all / mean(l$all) / 5)

nrow(l) # just under 3k
l$distance = as.numeric(sf::st_length(l)) / 1000

summary(l$distance)
plot(l$all, l$distance)
plot(l$all, log(l$distance))
cor(l$all, l$distance)
cor(l$all, l$distance)^2

# modelling total flow ----------------------------------------------------

# unconstrained gravity model
m2 = lm(all ~ log(distance), data = l)
pred_lm1 = predict(m2, l)
plot(l$distance, pred_lm1)
plot(l$distance, m2$residuals)
# Huge variability early on - must be other important predictors...
cor(m2$fitted.values, l$all)^2 # ~ 5% explained

# calculate proximity to city centre
cities = rnaturalearth::ne_download("large", type = "populated_places", returnclass = "sf")
bristol_midpoint = cities %>% filter(NAME == "Bristol") %>%
  filter(POP_MAX == max(POP_MAX))

if(city_name == "Bristol") {
  centrepoint_new = tmaptools::geocode_OSM(q = "Bristol", as.sf = TRUE)
}
mapview::mapview(centrepoint_new)
bristol_midpoint$geometry = centrepoint_new$geometry

st_crs(bristol_midpoint) = st_crs(z)
# mapview::mapview(bristol_midpoint)
z$distance_to_centre = as.numeric(sf::st_distance(zc, bristol_midpoint)[, 1]) / 1000 # units of km more understandable
plot(z["distance_to_centre"])
l = inner_join(l, z %>% select(o = geo_code, distance_to_centre_o = distance_to_centre) %>% st_drop_geometry())
l = inner_join(l, z %>% select(d = geo_code, distance_to_centre_d = distance_to_centre) %>% st_drop_geometry())
summary(l$distance_to_centre_o)
plot(l[c("distance_to_centre_o", "distance_to_centre_d")])
m3 = lm(all ~ log(distance) + distance_to_centre_d, data = l)
pred = predict(m3, l)
plot(l$distance, pred)
plot(l$distance, m3$residuals)
# Huge variability early on - must be other important predictors...
cor(pred, l$all)^2 # ~ 5% explained still

m4 = lm(all ~ log(distance) + log(distance_to_centre_d), data = l)
pred = predict(m4, l)
plot(l$distance, pred)
plot(l$distance, m4$residuals)
# Huge variability early on - must be other important predictors...
cor(pred, l$all)^2 # ~ 13% explained still

# m5 = lm(all ~ log(distance) + log(distance_to_centre_d) + log(distance) * log(distance_to_centre_d), data = l)
m5 = lm(all ~ log(distance) + log(distance_to_centre_d) +
          distance * distance_to_centre_d
        , data = l)
pred = predict(m5, l)
plot(l$distance, pred)
plot(l$distance, m5$residuals)
# Huge variability early on - must be other important predictors...
cor(pred, l$all)^2 # ~28% explained still

# identify largest residuals
l_error_more_than_500 = l %>% filter(m5$residuals > 500)
mapview::mapview(l_error_more_than_500) +
  mapview::mapview(z[bristol_midpoint, ])

# with weights for largest flows
m6 = lm(all ~ log(distance) + log(distance_to_centre_d) +
          distance * distance_to_centre_d
        , weights = all, data = l)
pred = predict(m6, l)
plot(l$distance, pred)
plot(l$distance, m6$residuals)
# Huge variability early on - must be other important predictors...
cor(pred, l$all)^2 # ~28% explained still

# with flows and non-linear interaction
m7 = lm(all ~ log(distance) + log(distance_to_centre_d) +
          distance * distance_to_centre_d +
          log(distance) * log(distance_to_centre_d)
        , data = l)
pred = predict(m7, l)
plot(l$distance, pred)
plot(l$distance, m7$residuals)
cor(pred, l$all)^2 # ~37%

m8 = lm(all ~ log(distance) + log(distance_to_centre_d) +
          distance * distance_to_centre_d +
          log(distance) * log(distance_to_centre_d)
        , weights = all, data = l)
pred = predict(m8, l)
plot(l$distance, pred)
plot(l$distance, m8$residuals)
cor(pred, l$all)^2 # 35%

# with distance of origin to centre...
m9 = lm(all ~ log(distance) + log(distance_to_centre_d) +
          distance * distance_to_centre_d +
          log(distance) * log(distance_to_centre_d) +
          distance_to_centre_d * distance_to_centre_o
        , weights = all, data = l)
pred = predict(m9, l)
plot(l$distance, pred)
plot(l$distance, m9$residuals)
# Huge variability early on - must be other important predictors...
cor(pred, l$all)^2 # 39%

saveRDS(m9, "pred-m9-bristol-sim-test.Rds")
piggyback::pb_upload("pred-m9-bristol-sim-test.Rds")

m10 = lm(all ~ log(distance) + log(distance_to_centre_d) +
          distance * distance_to_centre_d +
          distance_to_centre_d * distance_to_centre_o
        , weights = all, data = l)
pred = predict(m10, l)
plot(l$distance, pred)
plot(l$distance, m10$residuals)
cor(pred, l$all)^2 # 26%


# with distance of origin to centre...
m11 = lm(all ~ log(distance) + log(distance_to_centre_d) +
           log(distance) * log(distance_to_centre_d) +
           distance_to_centre_d * distance_to_centre_o
         , weights = all, data = l)
pred = predict(m11, l)
cor(pred, l$all)^2 # 30%

m12 = lm(all ~ log(distance) + log(distance_to_centre_d) +
           log(distance) * log(distance_to_centre_d) +
           distance_to_centre_d * distance_to_centre_o +
           log(distance_to_centre_d) * log(distance_to_centre_o)
         , weights = all, data = l)
pred = predict(m12, l)
cor(pred, l$all)^2 # 34%

# go with m9
l$all_m9 = predict(m9, l)
summary(l$all_m9)
l_large_m9 = l %>% filter(all_m9 > 50)
tm_shape(l_large_m9) +
  tm_lines(lwd = c("all", "all_m9"))

# try to make it origin-constrained model
l_grouped_by_origin = l_large_m9 %>%
  group_by(o) %>%
  summarise(all_m = sum(all_m9), all = sum(all)) %>%
  mutate(reality_model_ratio = all / all_m)

summary(l_grouped_by_origin$reality_model_ratio)

l_model = l_large_m9 %>%
  group_by(o) %>%
  mutate(all_m = all_m9 * (sum(all) / sum(all_m9))) %>%
  ungroup()

cor(l_model$all_m9, l_model$all_m)
cor(l_model$all, l_model$all_m)^2 # 51%
cor(l_model$all, l_model$all_m9)^2
sum(l_model$all_m)
sum(l_model$all) # the same
sum(l_model$all_m9) # 2 times bigger

tm_shape(l_model) +
  tm_lines(lwd = c("all", "all_m9", "all_m"))




plot(l_grouped_by_origin$all, l_grouped_by_origin$all_m) # to make that 100% fit...



# generate zones
z = bristol_midpoint %>%
  st_transform(stplanr::geo_select_aeq(.)) %>%
  sz_zone(n_circles = 20)



# radiation model

# additional variables


# bayesian approach
m3 = brm(all ~ log(distance), data = l, cores = 4)
plot(m3)
pred3 = predict(m3, l)[, 1]
plot(l$distance, pred3)
cor(l$all, pred3)^2 # ~ 5% explained

# get data for city -------------------------------------------------------

uk_region = "avon"
z = pct::get_pct_zones(uk_region)
c = pct::get_pct_centroids(uk_region)
mapview::mapview(z)
od = pct::get_od()

od_region = od %>%
  filter(geo_code1 %in% c$geo_code) %>%
  filter(geo_code2 %in% c$geo_code) %>%
  filter(geo_code1 != geo_code2)

l = stplanr::od2line(od_region, c)
nrow(l) # nearly 16k


# failed attempts / tests ---------------------------------------------------

# b <- c(2, 0.75)
# x <- rnorm(100)
# y <- rnorm(100, mean = b[1] * exp(b[2] * x))
# dat1 <- data.frame(x, y)
#
# prior1 <- prior(normal(1, 2), nlpar = "b1") +
#   prior(normal(0, 2), nlpar = "b2")
# fit1 <- brm(bf(y ~ b1 * exp(b2 * x), b1 + b2 ~ 1, nl = TRUE),
#             data = dat1, prior = prior1, cores = 4)
#
# # m1 = brm(log(all) ~ distance, data = l, cores = 4)
# # m1 = brm(all ~ distance, data = l, cores = 4)
# #
# ld = sf::st_drop_geometry(l)
# ld$dummy = 1
# # with gravity package
# m2 = ddm("all", "distance",
#          code_origin = "o", code_destination = "d",
#          additional_regressors = "dummy",
#          data = ld)
#
# all_predicted = exp(m2$fitted.values)
#
#
# plot(l$distance, all_predicted)
# cor(l$all, all_predicted)^2 # 0.2
# l$dist_log_ddm = log(l$distance)
# l$dummy_ddm = 1
# all_predicted2 = exp(predict(m2, l)) # fails...
# plot(l$distance, all_predicted2) # can generate predictions...
# cor(l$all, all_predicted2)^2 # 0.04
#
# stplanr::od_radiation

#
#
# fit <- ddm(
#   dependent_variable = "flow",
#   distance = "distw",
#   additional_regressors = c("rta", "comcur", "contig"),
#   code_origin = "iso_o",
#   code_destination = "iso_d",
#   data = gravity_no_zeros
# )

# non-linear brms - generates unexpected results:
# p = prior(normal(-0.1, 0.5), nlpar = "b1")
# flow_unconstrained = brm(bf(all | weights(all) ~ exp(b1 * distance), b1 ~ 1, nl = TRUE),
#                          data = l, prior = p, cores = 4)
# plot(flow_unconstrained)
# all_brms1 = predict(flow_unconstrained, l)
# plot(l$all, all_brms1[, 1])
# plot(l$distance, all_brms1[, 1])
# cor(all_brms1[, 1], l$all)
