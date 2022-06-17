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
source('~/predict-align-prevent/R/map_themes.r')

# load and prep general purpose data
nj <- load_neighborhood()
cumberland <- extract_area(nj)
cumberland_bm <- collect_basemap(cumberland)

# load the fishnets and bin the data
half_mi = st_read('~/PredictAlign/cumberland_half_mile_fishnet.shp')
half_mi_binned <- bin_equal_n_obs(half_mi, bin_name='incidence_bins', to_bin='COUNT100', n=10)

one_mi = st_read('~/PredictAlign/cumberland_one_mile_fishnet.shp')
one_mi_binned <- bin_equal_n_obs(one_mi, bin_name='incidence_bins', to_bin='COUNT100', n=10)

two_mi = st_read('~/PredictAlign/cumberland_two_mile_fishnet.shp')
two_mi_binned <- bin_equal_n_obs(two_mi, bin_name='incidence_bins', to_bin='COUNT100', n=10)

three_mi = st_read('~/PredictAlign/cumberland_three_mile_fishnet.shp')
three_mi_binned <- bin_equal_n_obs(three_mi, bin_name='incidence_bins', to_bin='COUNT100', n=10)

# generate maps
half_mi_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(half_mi_binned, crs=4236), aes(fill=incidence_bins), inherit.aes=FALSE, color=NA, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = "CPS Count per 100"
  ) +
  mapTheme() +
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='1/2-mile incidence per 100')

one_mi_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(one_mi_binned, crs=4236), aes(fill=incidence_bins), inherit.aes=FALSE, color=NA, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = "CPS Count per 100"
  ) +
  mapTheme() +
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='1-mile incidence per 100')

two_mi_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(two_mi_binned, crs=4236), aes(fill=incidence_bins), inherit.aes=FALSE, color=NA, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = "CPS Count per 100"
  ) +
  mapTheme() +
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='2-mile incidence per 100')

three_mi_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(three_mi_binned, crs=4236), aes(fill=incidence_bins), inherit.aes=FALSE, color=NA, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = "CPS Count per 100"
  ) +
  mapTheme() +
  theme(
    plot.title = element_text(size = 14, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 10, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='3-mile incidence per 100')

cowplot::plot_grid(half_mi_map, one_mi_map, two_mi_map, three_mi_map, nrow=2, align = "hv", axis = "lrbt")

one_mi_binned$incidence_bins <- sapply(one_mi_binned$incidence_bins, fix_bins)

inc_net_3_bin <- incidence_and_count_bins(inc_net_3, inc_col='inc_per_100', count_col='incident_count', cut_fxn=cut_number, n=3)

incidents_grp_net$count_bins <- sapply(incidents_grp_net$count_bins, fix_bins)
incidents_grp_net$count_bins_discrete <- as.character(incidents_grp_net$count_bins, fix_bins)

incidents_grp_net$incidence_bins <- sapply(incidents_grp_net$incidence_bins, fix_bins)
incidents_grp_net$incidence_bins_discrete <- as.character(incidents_grp_net$incidence_bins, fix_bins)

incidents_grp_net$incidence_bins_discrete <- factor(incidents_grp_net$incidence_bins_discrete, levels = sort(unique(incidents_grp_net$incidence_bins)))
incidents_grp_net <- incidents_grp_net %>% arrange(incidence_bins_discrete)