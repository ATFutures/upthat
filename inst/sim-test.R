# Aim: test out spatial interaction model/data fit


# set-up ------------------------------------------------------------------

devtools::install_github("itsleeds/geofabric")
devtools::install_github("itsleeds/pct")
library(tidyverse)
library(gravity)
library(brms)

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

# test data from bristol --------------------------------------------------

z = spDataLarge::bristol_zones
od_region = spDataLarge::bristol_od %>%
  filter(o != d)
l = stplanr::od2line(od_region, z)
nrow(l) # just under 3k
l$distance = as.numeric(sf::st_length(l)) / 1000
summary(l$distance)
plot(l$all, l$distance)
cor(l$all, l$distance)^2

# modelling total flow ----------------------------------------------------

# unconstrained gravity model
m2 = lm(all ~ log(distance), data = l)
plot(l$distance, m2$fitted.values)
cor(m2$fitted.values, l$all)^2 # ~ 5% explained

# radiation model

# additional variables


# bayesian approach
m3 = brm(all ~ log(distance), data = l, cores = 4)
plot(m3)
pred3 = predict(m3, l)[, 1]
plot(l$distance, pred3)
cor(l$all, pred3)^2 # ~ 5% explained


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
ld$dummy = 1
# with gravity package
m2 = ddm("all", "distance",
         code_origin = "o", code_destination = "d",
         additional_regressors = "dummy",
         data = ld)

all_predicted = exp(m2$fitted.values)


plot(l$distance, all_predicted)
cor(l$all, all_predicted)^2 # 0.2
l$dist_log_ddm = log(l$distance)
l$dummy_ddm = 1
all_predicted2 = exp(predict(m2, l)) # fails...
plot(l$distance, all_predicted2) # can generate predictions...
cor(l$all, all_predicted2)^2 # 0.04

stplanr::od_radiation

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
