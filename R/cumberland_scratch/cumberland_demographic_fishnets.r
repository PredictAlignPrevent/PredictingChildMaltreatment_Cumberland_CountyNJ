## Demographic data from US Census

# run command below in command line to get data at census tract level for all of NJ
# Rscript /Users/kpierce/demographic-toolkit/census_aggregato.r -k /Users/kpierce/CooksProTX/us_census_api_key.txt -s nj-pap-census-vars -o /Users/kpierce/PredictAlign

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
library("tidygeocoder")
library("stringr")
library("units")

source('~/predict-align-prevent/R/fishnet.r')
source('~/predict-align-prevent/R/map_themes.r')

# basic datasets for mapping
nj <- load_neighborhood()
cumberland <- extract_area(nj)
cumberland_bm <- collect_basemap(cumberland)

# Census data and census tract shapes (not all variables of interest are at the block group level, so all are downloaded by tract)
nj_census <- read.csv('~/PredictAlign/nj-pap-census-vars_20210610154857.csv')
nj_census$GEOID <- as.character(nj_census$GEOID)
cumberland_tract <- tigris::tracts(state=34, county=11, year=2019)
tract_census <- left_join(cumberland_tract, nj_census, by='GEOID')
tract_census <- st_transform(tract_census, crs='ESRI:102311')

colnames_ <- names(tract_census)
estimate_cols <- colnames_[grepl('^E', colnames_)]

fishnet_data = list()
for(i in 1:length(estimate_cols)){
  ec <- estimate_cols[i]
  est_subset <- tract_census %>% select(.data[[ec]])
  est_subset$block_area <- st_area(est_subset$geometry)
  est_subset$pop_rate = est_subset[[ec]]/est_subset$block_area # the population data are in column "estimate"
  names(est_subset) <- c('estimate', 'geometry', 'block_area', 'pop_rate')
  est_subset_net <- create_population_fishnet(
    pop_data=est_subset, 
    cover_area=cumberland, 
    fishnet_size=1, 
    fishnet_units='mi'
  )
  est_subset_net <- est_subset_net %>% select(-unitless_net_pop)
  names(est_subset_net) <- c('net_id', ec, 'geometry')
  fishnet_data[[i]] <- est_subset_net
}

net_summaries = list()
for(j in 1:length(fishnet_data)){
  fn_data <- fishnet_data[[j]]
  summary <- as_tibble(st_drop_geometry(fn_data))
  net_summaries[[j]] <- summary
}

final_summary <- net_summaries[[1]]
for(k in 2:length(net_summaries)){
  final_summary <- full_join(final_summary, net_summaries[[k]], by='net_id')
}

final_summary_unitless <- units::drop_units(final_summary)
geo_only <- fishnet_data[[1]] %>% select(net_id, geometry)
final_summary_shp <- left_join(geo_only, final_summary_unitless, by='net_id')
final_summary_shp$grid_area <- st_area(final_summary_shp)
final_summary_shp$unitless_grid_area <- units::drop_units(final_summary_shp$grid_area)

# a couple of quick maps to check data
ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(final_summary_shp, crs=4326), aes(fill=unitless_grid_area), inherit.aes=FALSE, alpha=0.8)

ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(final_summary_shp, crs=4326), aes(fill=E_TOTPOP), inherit.aes=FALSE, alpha=0.8)

# crosswalk net ids -- the one mi fishnet used in other parts of the analysis was generated independently
# check that the fishnets generated for these data have the same ID
check_IDs$area <- st_area(check_IDs) 
nonzero_area <- check_IDs %>% filter(area > set_units(0, 'm2'))
mismatch <- nonzero_area %>% filter(net_id != net_id.1) # there are no mismatches in the original fishnet ID and the new IDs created in this script

final_with_maltrt <- left_join(one_mi, final_summary, by=c('NETID'='net_id'))

# NETPOP is generated from block group level data, while E_TOTPOP is generated from tract level data
# consequently the individual net cell populations differ. however, the total population is approximately equivalent:
sum(final_with_maltrt$NETPOP)
sum(final_with_maltrt$E_TOTPOP)
# we will drop E_TOTPOP from the final table

nonzero_cells <- final_with_maltrt %>% filter(COUNT100 > 0)
count_100_quantiles <- stats::quantile(nonzero_cells$COUNT100,  probs=c(0.3, 0.5, 0.7, 0.9))

final_with_maltrt$risk_category <- sapply(final_with_maltrt$COUNT100, assign_risk_cat, count_100_quantiles)
final_with_maltrt$risk_category <- factor(final_with_maltrt$risk_category, levels=c(1, 2, 3, 4, 5))

st_write(final_with_maltrt, '~/PredictAlign/PredictAlignFishnets/maltreatment_and_demographic_data_one_mile_grid.shp')
final_no_geo <- final_with_maltrt %>% st_drop_geometry() %>% as_tibble()
write.csv(final_no_geo, '~/PredictAlign/PredictAlignFishnets/maltreatment_and_demographic_data_one_mile_grid.csv')

final_with_maltrt <- st_read('~/PredictAlign/PredictAlignFishnets/maltreatment_and_demographic_data_one_mile_grid.shp')

nonzero_count_cells <- final_with_maltrt %>% filter(COUNT > 0)
count_quantiles <- stats::quantile(nonzero_count_cells$COUNT,  probs=c(0.3, 0.5, 0.7, 0.9))
final_with_maltrt$count_risk_cat <- sapply(final_with_maltrt$COUNT, assign_risk_cat, count_quantiles)
final_with_maltrt$count_risk_cat <- factor(final_with_maltrt$count_risk_cat, levels=c(1, 2, 3, 4, 5))

inc_risk <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(final_with_maltrt, crs=4326), aes(fill=rsk_ctg), inherit.aes=FALSE, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = ""
  ) +
  mapTheme() +
  theme(
    plot.title = element_text(size = 22, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='Incidence-Based Risk Categories')

count_risk <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(final_with_maltrt, crs=4326), aes(fill=count_risk_cat), inherit.aes=FALSE, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = ""
  ) +
  mapTheme() +
  theme(
    plot.title = element_text(size = 22, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='Count-Based Risk Categories')

cowplot::plot_grid(inc_risk, count_risk, nrow=1, align = "hv", axis = "lrbt")

# with urban area polygons overlaid
nj_bm <- collect_basemap(nj)

urban <- st_read('~/CooksProTX/spatial/tigris/usa_urban_areas/urban_areas_2019/urban_areas_2019.shp')
nj_urban <- urban %>% filter(grepl('NJ', NAME10))
ggmap(nj_bm) + geom_sf(data=nj_urban, inherit.aes=FALSE, alpha=0.8)

inc_risk_urban <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(final_with_maltrt, crs=4326), aes(fill=rsk_ctg), inherit.aes=FALSE, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = ""
  ) +
  geom_sf(data=nj_urban, aes(size=1.5), color='black', inherit.aes=FALSE, alpha=0) +
    scale_size_identity() +
  mapTheme() +
  theme(
    plot.title = element_text(size = 22, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='Incidence-Based Risk Categories')

count_risk_urban <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(final_with_maltrt, crs=4326), aes(fill=count_risk_cat), inherit.aes=FALSE, alpha=0.8) +
  scale_fill_viridis_d(
    na.value = NA,
    option = "D",
    direction = 1,
    name = ""
  ) +
  geom_sf(data=nj_urban, aes(size=1.5), color='black', inherit.aes=FALSE, alpha=0) +
    scale_size_identity() +
  mapTheme() +
  theme(
    plot.title = element_text(size = 22, family = "sans", face = "plain", hjust = 0),
    plot.subtitle=element_text(size = 11, family = "sans", hjust = 0),
    plot.caption=element_text(size = 10, family = "sans", face = "italic", hjust = 0),
    axis.line = element_blank(),
    legend.title = element_text(size = 14, family = "sans"),
    legend.text = element_text(size = 9, family = "sans")) +
  labs(title='Count-Based Risk Categories')

cowplot::plot_grid(inc_risk_urban, count_risk_urban, nrow=1, align = "hv", axis = "lrbt")

ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    one_mi_rp_net_summary,
    crs=crs(nj_urban)), aes(fill=total), inherit.aes=FALSE, alpha=0.8) +
  labs(title='All Crime and Violations Data') +
  theme(plot.title=element_text(size=22)) +
  geom_sf(data=nj_urban, aes(size=1.5), color='black', inherit.aes=FALSE, alpha=0) +
  scale_size_identity()

# inspecting some of the category 5 grid cells:

final_cat5 <- final_with_maltrt %>% filter(risk_category==5)
cell196 <- final_with_maltrt %>% filter(NETID==196)

ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(cell196, crs=4326), aes(fill=NETPOP), inherit.aes=FALSE, alpha=0.8)
