rm(list=ls())

library("sf")            # Spatial data objects and methods
library("mapview")       # Interactive Map Viewing
library("ggmap")         # ggplot2 addon for base maps
library("cowplot")
library("spatstat")      # KDE and other spatial functions
library("raster")        # cell-based spatial operations
library("tidyverse")     # data manipulation framework
library("Hmisc")         # using cut2() functions for ggplot legends
library("hrbrthemes")
library("gridExtra")
library("knitr")         # for kable table
library("viridis")
library("viridisLite")
library("readxl")
library("spdep")
library("tidycensus")
library("tigris")

source('~/predict-align-prevent/R/fishnet.r')

# load and prep general purpose data
nj <- load_neighborhood()
cumberland <- extract_area(nj)
cumberland_bm <- collect_basemap(cumberland)
cumberland_pop <- collect_population(key_path='~/CooksProTX/us_census_api_key.txt')
geocoded_incidents <- read.csv('~/PredictAlign/171819_NJ_geocoded_incidents.csv')
incidents_shp <- create_incidents_sf(geocoded_incidents)

# create a three-mile fishnet grid
pop_net_3 <- create_population_fishnet(pop_data=cumberland_pop, cover_area=cumberland, fishnet_size=3, fishnet_units='mi')
inc_net_3 <- create_incidents_fishnet(pop_fishnet=pop_net_3, incidents_data=incidents_shp)

# check that populations are the same in original fishnet and incidence fishnet
fn_inc_3 <- left_join(pop_net_3, as_tibble(inc_net_3), by='net_id')
fn_inc_3$diff <- fn_inc_3$net_pop.x - fn_inc_3$net_pop.y
max(fn_inc_3$diff)
min(fn_inc_3$diff)

inc_net_3$centroid <- st_centroid(inc_net_3)
net3 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(inc_net_3, crs=4326), aes(fill=inc_per_100), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  labs(title='3-mile fishnet')

# create a two-mile fishnet grid
pop_net_2 <- create_population_fishnet(pop_data=cumberland_pop, cover_area=cumberland, fishnet_size=2, fishnet_units='mi')
inc_net_2 <- create_incidents_fishnet(pop_fishnet=pop_net_2, incidents_data=incidents_shp)

# check that populations are the same in original fishnet and incidence fishnet
fn_inc_2 <- left_join(pop_net_2, as_tibble(inc_net_2), by='net_id')
fn_inc_2$diff <- fn_inc_2$net_pop.x - fn_inc_2$net_pop.y
max(fn_inc_2$diff)
min(fn_inc_2$diff)

net2 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(inc_net_2, crs=4326), aes(fill=inc_per_100), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  labs(title='2-mile fishnet')

# create a 1-mile fishnet grid
pop_net_1 <- create_population_fishnet(pop_data=cumberland_pop, cover_area=cumberland, fishnet_size=1, fishnet_units='mi')
inc_net_1 <- create_incidents_fishnet(pop_fishnet=pop_net_1, incidents_data=incidents_shp)

# check that populations are the same in original fishnet and incidence fishnet
fn_inc_1 <- left_join(pop_net_1, as_tibble(inc_net_1), by='net_id')
fn_inc_1$diff <- fn_inc_1$net_pop.x - fn_inc_1$net_pop.y
max(fn_inc_1$diff)
min(fn_inc_1$diff)

net1 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(inc_net_1, crs=4326), aes(fill=inc_per_100), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  labs(title='1-mile fishnet')

# create a half-mile fishnet grid
pop_net_05 <- create_population_fishnet(pop_data=cumberland_pop, cover_area=cumberland, fishnet_size=0.5, fishnet_units='mi')
inc_net_05 <- create_incidents_fishnet(pop_fishnet=pop_net_05, incidents_data=incidents_shp)

# check that populations are the same in original fishnet and incidence fishnet
fn_inc_05 <- left_join(pop_net_05, as_tibble(inc_net_05), by='net_id')
fn_inc_05$diff <- fn_inc_05$net_pop.x - fn_inc_05$net_pop.y
max(fn_inc_05$diff)
min(fn_inc_05$diff)

net05 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(inc_net_05, crs=4326), aes(fill=inc_per_100), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  labs(title='1/2-mile fishnet')

cowplot::plot_grid(net3, net2, net1, net05, nrow=2, align = "hv", axis = "lrbt")

# write files
esri_names <- c('NETID', 'NETPOP', 'GEOMETRY', 'COUNT', 'COUNT100') # max 10 characters

inc_net_3_final <- inc_net_3 %>% select(-unitless_net_pop)
names(inc_net_3_final) <- esri_names
st_geometry(inc_net_3_final) <- 'GEOMETRY'
st_write(inc_net_3_final, '~/PredictAlign/cumberland_three_mile_fishnet.shp')
write.csv(inc_net_3_final, '~/PredictAlign/cumberland_three_mile_fishnet.csv')

inc_net_2_final <- inc_net_2 %>% select(-unitless_net_pop)
names(inc_net_2_final) <- esri_names
st_geometry(inc_net_2_final) <- 'GEOMETRY'
st_write(inc_net_2_final, '~/PredictAlign/cumberland_two_mile_fishnet.shp')
write.csv(inc_net_2_final, '~/PredictAlign/cumberland_two_mile_fishnet.csv')

inc_net_1_final <- inc_net_1 %>% select(-unitless_net_pop)
names(inc_net_1_final) <- esri_names
st_geometry(inc_net_1_final) <- 'GEOMETRY'
st_write(inc_net_1_final, '~/PredictAlign/cumberland_one_mile_fishnet.shp')
write.csv(inc_net_1_final, '~/PredictAlign/cumberland_one_mile_fishnet.csv')

inc_net_05_final <- inc_net_05 %>% select(-unitless_net_pop)
names(inc_net_05_final) <- esri_names
st_geometry(inc_net_05_final) <- 'GEOMETRY'
st_write(inc_net_05_final, '~/PredictAlign/cumberland_half_mile_fishnet.shp')
write.csv(inc_net_05_final, '~/PredictAlign/cumberland_half_mile_fishnet.csv')
