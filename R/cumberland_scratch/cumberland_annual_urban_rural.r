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
library("lubridate")
library("readr")

source('~/predict-align-prevent/R/fishnet.r')
source('~/predict-align-prevent/R/map_themes.r')

###############
## LOAD DATA ##
###############

# load and prep general purpose data
nj <- load_neighborhood()
cumberland <- extract_area(nj)
cumberland_bm <- collect_basemap(cumberland)

# incidents data
geocoded_incidents <- read.csv('~/PredictAlign/171819_NJ_geocoded_incidents.csv')
geocoded_incidents$intake_year <- lubridate::year(geocoded_incidents$Intake.RcvdDate)
incidents_shp <- create_incidents_sf(geocoded_incidents)

# unique address-intake date-child age combinations
addr_incidents <- geocoded_incidents %>% 
  filter(match_indicator=='Match') %>%
  group_by(Intake.RcvdDate, full_addr, Intake.ChildAge) %>% 
  tally()

addr_incidents$intake_year <- lubridate::year(addr_incidents$Intake.RcvdDate)
unique_geocodes <- geocoded_incidents %>% select(full_addr, lat, lon) %>% distinct()
unique_incidents <- addr_incidents %>%
  group_by(intake_year, full_addr) %>%
  summarise(estimated_n = sum(n))

hist(unique_incidents$estimated_n)
unique_incidents %>% filter(estimated_n > 20)

# make a shapefile of the presumed unique incidents
unique_geocoded_incidents <- left_join(unique_incidents, unique_geocodes, by='full_addr')
unique_incidents_shp <- create_incidents_sf(unique_geocoded_incidents)

###########################################################
## CREATE INCIDENT FISHNETS FOR EACH YEAR AND RESOLUTION ##
###########################################################

# incidents by year
years <- 2017:2019
resolutions <- c(0.5, 1)
inc_fishnets_list = list()
unique_fishnets_list = list()
for(i in seq_along(years)){
  # filter incidents by year
  year_incidents <- incidents_shp %>% filter(intake_year==years[i])
  unique_year_incidents <- unique_incidents_shp %>% filter(intake_year==years[i])
  # get population for that year
  year_pop <- collect_population(key_path='~/CooksProTX/us_census_api_key.txt', year=years[i])
  for(j in seq_along(resolutions)){  # population fishnet
    pop_net <- create_population_fishnet(pop_data=year_pop, cover_area=cumberland, fishnet_size=resolutions[j], fishnet_units='mi')
    # incident fishnet
    year_res <- paste(years[i], resolutions[j], sep='_')
    inc_fishnets_list[[year_res]] <- create_incidents_fishnet(pop_fishnet=pop_net, incidents_data=year_incidents)
    unique_fishnets_list[[year_res]] <- create_incidents_fishnet(pop_fishnet=pop_net, incidents_data=unique_year_incidents)
  }
}

#######################################
## URBAN AND RURAL AREAS IN FISHNETS ##
#######################################

# load urban areas shapefile
urban <- st_read('~/CooksProTX/spatial/tigris/usa_urban_areas/urban_areas_2019/urban_areas_2019.shp')
nj_urban <- urban %>% filter(grepl('NJ', NAME10)) %>% st_transform(crs=crs(inc_fishnets_list[[1]]))

# only need geometry from one of the fishnets at each resolution
fishnet_half_mi <- inc_fishnets_list[[1]] %>% select(net_id, geometry)
fishnet_one_mi <- inc_fishnets_list[[2]] %>% select(net_id, geometry)

# manual inspection of the data shows these as the urban areas of interest
cumberland_urban_areas <- c(
  'Vineland, NJ Urbanized Area',
  'Bridgeton, NJ Urban Cluster',
  'Laurel Lake, NJ Urban Cluster'
)

# intersect urban areas with fishnet and get lists of net ids in each urban area
fishnet_one_mi_urban_areas <- list()
fishnet_half_mi_urban_areas <- list()
for(k in seq_along(cumberland_urban_areas)){
  urban_area <- nj_urban %>% filter(NAMELSAD10==cumberland_urban_areas[k])
  ## HALF MILE FISHNET
  net_intersect_05 <- st_intersects(urban_area, fishnet_half_mi)
  net_cover_area_05 <- fishnet_half_mi[unique(unlist(net_intersect_05)),]
  fishnet_half_mi_urban_areas[[cumberland_urban_areas[k]]] <- unique(net_cover_area_05$net_id)
  ## ONE MILE FISHNET
  net_intersect_1 <- st_intersects(urban_area, fishnet_one_mi)
  net_cover_area_1 <- fishnet_one_mi[unique(unlist(net_intersect_1)),]
  fishnet_one_mi_urban_areas[[cumberland_urban_areas[k]]] <- unique(net_cover_area_1$net_id)
}

# confirm urban area assignment is unique for fishnet cells
intersect(fishnet_one_mi_urban_areas[[1]], fishnet_one_mi_urban_areas[[2]])
intersect(fishnet_one_mi_urban_areas[[1]], fishnet_one_mi_urban_areas[[3]]) # at this resolution, net_id 363 is shared between Vineland and Laurel Lake
intersect(fishnet_one_mi_urban_areas[[3]], fishnet_one_mi_urban_areas[[2]])

intersect(fishnet_half_mi_urban_areas[[1]], fishnet_half_mi_urban_areas[[2]])
intersect(fishnet_half_mi_urban_areas[[1]], fishnet_half_mi_urban_areas[[3]])
intersect(fishnet_half_mi_urban_areas[[3]], fishnet_half_mi_urban_areas[[2]])

fishnet_half_mi <- fishnet_half_mi %>% mutate(
  urban_area = case_when(
    (net_id %in% fishnet_half_mi_urban_areas[[1]]) ~ cumberland_urban_areas[1],
    (net_id %in% fishnet_half_mi_urban_areas[[2]]) ~ cumberland_urban_areas[2],
    (net_id %in% fishnet_half_mi_urban_areas[[3]]) ~ cumberland_urban_areas[3]
  )
)
fishnet_half_mi$urban_area <- replace_na(fishnet_half_mi$urban_area, 'rural')

fishnet_one_mi <- fishnet_one_mi %>% mutate(
  urban_area = case_when(
    (net_id %in% fishnet_one_mi_urban_areas[[1]] & net_id != 363) ~ cumberland_urban_areas[1],
    (net_id %in% fishnet_one_mi_urban_areas[[2]]) ~ cumberland_urban_areas[2],
    (net_id %in% fishnet_one_mi_urban_areas[[3]] & net_id != 363) ~ cumberland_urban_areas[3],
    (net_id == 363) ~ 'Vineland and Laurel Lake overlap'
  )
)
fishnet_one_mi$urban_area <- replace_na(fishnet_one_mi$urban_area, 'rural')

# sanity check with maps
half_mi_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(fishnet_half_mi, crs=4326), aes(fill=urban_area), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  geom_sf(data=nj_urban, aes(size=1.5), color='black', inherit.aes=FALSE, alpha=0) +
  scale_size_identity() +
  labs(title='Half-mile fishnet')

one_mi_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(fishnet_one_mi, crs=4326), aes(fill=urban_area), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  geom_sf(data=nj_urban, aes(size=1.5), color='black', inherit.aes=FALSE, alpha=0) +
  scale_size_identity() +
  labs(title='One-mile fishnet')
  
cowplot::plot_grid(one_mi_map, half_mi_map)

########################################################
## COMBINE URBAN/RURAL FISHNETS AND INCIDENT FISHNETS ##
########################################################

# list names are the same for inc_fishnets_list and unique_fishnets_list
one_mi_inc_names <- names(inc_fishnets_list)[grepl('_1', names(inc_fishnets_list))]
half_mi_inc_names <- names(inc_fishnets_list)[grepl('_0.5', names(inc_fishnets_list))]

fn1mi <- fishnet_one_mi %>% select(net_id, urban_area) %>% st_drop_geometry
fn05mi <- fishnet_half_mi %>% select(net_id, urban_area) %>% st_drop_geometry

for(i in seq_along(one_mi_inc_names)){
  
  # raw
  data <- inc_fishnets_list[[one_mi_inc_names[i]]]
  data <- left_join(data, fn1mi, by='net_id')
  inc_fishnets_list[[one_mi_inc_names[i]]] <- data
  
  # unique
  udata <- unique_fishnets_list[[one_mi_inc_names[[i]]]]
  udata <- left_join(udata, fn1mi, by='net_id')
  unique_fishnets_list[[one_mi_inc_names[[i]]]] <- udata
}

for(i in seq_along(half_mi_inc_names)){
  
  # raw
  data <- inc_fishnets_list[[half_mi_inc_names[i]]]
  data <- left_join(data, fn05mi, by='net_id')
  inc_fishnets_list[[half_mi_inc_names[i]]] <- data
  
  # unique
  udata <- unique_fishnets_list[[half_mi_inc_names[i]]]
  udata <- left_join(udata, fn05mi, by='net_id')
  unique_fishnets_list[[half_mi_inc_names[i]]] <- udata
}

############################
## ASSIGN RISK CATEGORIES ##
############################

# get risk categories
get_risk_categories <- function(dataset, column){
  # define 4 quantiles that map to 5 risk categories, excluding zero counts
  nonzero <- dataset %>% filter(.data[[column]] > 0)
  quantiles <- stats::quantile(nonzero[[column]], probs=c(0.3, 0.5, 0.7, 0.9))
  new_name <- paste(column, 'risk_category', sep='_')
  dataset[[new_name]] <- sapply(dataset[[column]], assign_risk_cat, quantiles)
  dataset[[new_name]] <- factor(dataset[[new_name]], levels=c(1, 2, 3, 4, 5))
  return(dataset)
}

# raw
for(d in seq_along(inc_fishnets_list)){
  rc <- get_risk_categories(inc_fishnets_list[[d]], column='incident_count')
  rc <- get_risk_categories(rc, column='inc_per_100')
  inc_fishnets_list[[d]] <- rc
}

# unique
for(d in seq_along(unique_fishnets_list)){
  rc <- get_risk_categories(unique_fishnets_list[[d]], column='incident_count')
  rc <- get_risk_categories(rc, column='inc_per_100')
  unique_fishnets_list[[d]] <- rc
}

######################
## COMBINE ALL DATA ##
######################

for(l in seq_along(inc_fishnets_list)){
  indicators <- unlist(strsplit(names(inc_fishnets_list)[l], split='_'))
  inc_fishnets_list[[l]]$intake_year <- indicators[1]
  inc_fishnets_list[[l]]$fishnet_resolution_mi <- indicators[2]
  inc_fishnets_list[[l]]$summary_type <- 'raw_count'
}
for(l in seq_along(unique_fishnets_list)){
  indicators <- unlist(strsplit(names(unique_fishnets_list)[l], split='_'))
  unique_fishnets_list[[l]]$intake_year <- indicators[1]
  unique_fishnets_list[[l]]$fishnet_resolution_mi <- indicators[2]
  unique_fishnets_list[[l]]$summary_type <- 'unique_addr_child_age_combination'
}

all_raw <- bind_rows(inc_fishnets_list)
all_unique <- bind_rows(unique_fishnets_list)
all <- bind_rows(all_raw, all_unique)

long_annual_fishnet_risk <- all %>% 
  as_tibble() %>% # pivot hates sf
  pivot_longer(
    cols=c("incident_count_risk_category", "inc_per_100_risk_category"), 
    names_to=c("count_type"), values_to=c("risk_category")) 

long_annual_fishnet_count <- all %>%
  pivot_longer(
    cols=c('incident_count', 'inc_per_100'),
    names_to=c('count_type'), values_to=c("count")
  ) %>%
  st_as_sf(crs=crs(all))

# maps
one_mi_raw_map <- ggmap(cumberland_bm) + 
  geom_sf(
    data=st_transform(
      filter(long_annual_fishnet_risk, fishnet_resolution_mi=='1'), 
      crs=4326
      ), aes(fill=risk_category), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  facet_grid(summary_type ~ intake_year) +
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
  labs(title='Annual Intakes')

half_mi_raw_map <- ggmap(cumberland_bm) + 
  geom_sf(
    data=st_transform(
      filter(long_annual_fishnet_risk, fishnet_resolution_mi=='0.5'), 
      crs=4326
    ), aes(fill=risk_category), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  facet_grid(summary_type ~ intake_year) +
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
  labs(title='Annual Intakes')

all_final <- all %>% select(-unitless_net_pop)
names(all_final) <- c('net_id', 'net_pop', 'geometry', 'count', 'count100', 'area', 'count_risk', 'inc_risk', 'year',
                      'fn_width', 'type')
st_write(all_final, '~/PredictAlign/PredictAlignFishnets/cumberland_fishnets_urban_areas_and_maltreatment_aggregations.shp')

## risk/protective data
one_mi_net <- all_final %>% filter(fn_width=='1') %>% select(net_id, area) %>% st_drop_geometry() %>% as_tibble() %>% distinct()
half_mi_net <- all_final %>% filter(fn_width=='0.5') %>% select(net_id, area) %>% st_drop_geometry() %>% as_tibble() %>% distinct()

bt_crime <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_Crime_all_geocoded_with_netid_wgs84.csv')
bt_crime$fmt_date <- readr::parse_datetime(bt_crime$Report.Date...Time, "%m/%d/%Y %H:%M")
bt_crime$report_year <- lubridate::year(bt_crime$fmt_date)
bt_crime_urban_rural_net <- left_join(bt_crime, one_mi_net, by=c('NETID'='net_id'))
bt_crime_urban_rural_net <- left_join(bt_crime, half_mi_net, by=c('NETID'=))
write.csv(bt_crime_urban_rural_net, '~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_Crime_all_geocoded_with_netid_wgs84_urban_rural.csv')

mv_viol <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/Millville_violations_all_geocoded_with_netid_wgs84.csv')
mv_viol$violation_year <- lubridate::year(mv_viol$Violation.Date)
mv_viol_urban_rural_net <- left_join(mv_viol, one_mi_net, by=c('NETID'='net_id'))
write.csv(mv_viol_urban_rural_net, '~/PredictAlign/PredictAlignFishnets/cleaned_data/Millville_violations_all_geocoded_with_netid_wgs84_urban_rural.csv')

bt_viol <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_violations_all_geocoded_with_netid_wgs84.csv')
bt_viol$issue_year <- lubridate::year(bt_viol$Issue.Date)
bt_viol_urban_rural_net <- left_join(bt_viol, one_mi_net, by=c('NETID'='net_id'))
write.csv(bt_viol_urban_rural_net, '~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_violations_all_geocoded_with_netid_wgs84_urban_rural.csv')

st_pol <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/St_Pol_crime_with_netid_wgs84.csv')
st_pol$report_year <- lubridate::year(st_pol$Police.report.date)
st_pol_urban_rural_net <- left_join(st_pol, one_mi_net, by=c('NETID'='net_id'))
write.csv(st_pol_urban_rural_net, '~/PredictAlign/PredictAlignFishnets/cleaned_data/St_Pol_crime_with_netid_wgs84_urban_rural.csv')

