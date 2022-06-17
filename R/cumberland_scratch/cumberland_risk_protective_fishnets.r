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

source('~/predict-align-prevent/R/fishnet.r')

WGS84 = 4326
WGS84_proj = 3857
NJ_PLANAR = 'ESRI:102311'

one_mi = st_read('~/PredictAlign/PredictAlignFishnets/cumberland_one_mile_fishnet.shp')
one_mi_tbl <- one_mi %>% 
  st_centroid() %>% 
  st_transform(crs=WGS84) %>%
  mutate(fishnet_centroid_lon=sf::st_coordinates(.)[,1], fishnet_centroid_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(NETID, fishnet_centroid_lon, fishnet_centroid_lat)

half_mi = st_read('~/PredictAlign/PredictAlignFishnets/cumberland_half_mile_fishnet.shp')
half_mi_tbl <- half_mi %>% 
  st_centroid() %>% 
  st_transform(crs=WGS84) %>%
  mutate(fishnet_centroid_lon=sf::st_coordinates(.)[,1], fishnet_centroid_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry() %>%
  select(NETID, fishnet_centroid_lon, fishnet_centroid_lat)

#### NOT RUN: CRIME DATA TEOCODE BACKFILL ####
bridgeton_crime <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_Crime_all.csv')

bc_summary <- bridgeton_crime
bc_summary$date <- lubridate::date(bc_summary$"Report.Date...Time")
bc_summary$year <- lubridate::year(bc_summary$"Report.Date...Time")

# previously aggregated all the unique addresses but did not geocode; finish that up real quick (4,127 addresses can go in one batch)
#bridgeton_crime_addr <- read.csv('~/PredictAlign/RiskProtectiveGeocoded/bridgeton_police_data_unique_addresses_to_clean.csv')
#crime_loc <- tidygeocoder::geocode(bridgeton_crime_addr, address=Incident.Location, method='census', full_results=TRUE, return_type='geographies')
#table(crime_loc$match_indicator)
#matched <- crime_loc %>% filter(!is.na(match_type))

# the addresses are a bit of a mess, but perhaps street and state name will get us closer...
#unmatched <- crime_loc %>% filter(is.na(match_type)) %>% select(Incident.Location, Address, City, State.Zip)
#unmatched$state_name <- 'NJ'
#backfill <- tidygeocoder::geocode(unmatched, street=Address, state=state_name)
#backfill_complete <- backfill %>% drop_na(lat, long)
#backfill_full <- tidygeocoder::geocode(unmatched, street=Address, state=state_name, full_results=TRUE, return_type='geographies')

# all geocoded
#geocoded_crime <- bind_rows(matched, backfill_complete)
#write.csv(geocoded_crime, '~/PredictAlign/RiskProtectiveGeocoded/bridgeton_police_data_unique_addresses_geocoded.csv')
#pct_match <- dim(geocoded_crime)[1]/dim(bridgeton_crime_addr)[1]

#### CRIME DATA ####

geocoded_crime <- read.csv('~/PredictAlign/RiskProtectiveGeocoded/bridgeton_police_data_unique_addresses_geocoded.csv')

bridgeton_crime_geocoded <- left_join(bridgeton_crime, geocoded_crime, by='Incident.Location')
bridgeton_crime_geocoded_match <- bridgeton_crime_geocoded %>% drop_na(lat, long)

# convert to shapefile
names(bridgeton_crime_geocoded_match)[names(bridgeton_crime_geocoded_match) == 'long'] <- 'lon'
bridgeton_crime_sf <- create_incidents_sf(bridgeton_crime_geocoded_match, initial_crs=WGS84, transform_crs=WGS84)

## one mile fishnet
bridgeton_crime_fishnet <- st_join(bridgeton_crime_sf, one_mi, join=st_within) 
bridgeton_crime_fishnet_final <- bridgeton_crime_fishnet %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(crime_lon=sf::st_coordinates(.)[,1], crime_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

bridgeton_crime_fishnet_final <- dplyr::left_join(bridgeton_crime_fishnet_final, one_mi_tbl, by='NETID')
bridgeton_crime_fishnet_final <- bridgeton_crime_fishnet_final %>%
  select(Report.Date...Time, PD.Case.., Incident.Location, Agency.Incident...Actual.CFS.Type, crime_type,
         Address, City, State.Zip, input_address, incident_count, NETID, NETPOP, COUNT, COUNT100,
         crime_lon, crime_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  bridgeton_crime_fishnet_final,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_Crime_all_geocoded_with_netid_wgs84.csv',
  quote=TRUE
  )

## half mile fishnet
bridgeton_crime_fishnet05 <- st_join(bridgeton_crime_sf, half_mi, join=st_within) 
bridgeton_crime_fishnet_final05 <- bridgeton_crime_fishnet05 %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(crime_lon=sf::st_coordinates(.)[,1], crime_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

bridgeton_crime_fishnet_final05 <- dplyr::left_join(bridgeton_crime_fishnet_final05, half_mi_tbl, by='NETID')
bridgeton_crime_fishnet_final05 <- bridgeton_crime_fishnet_final05 %>%
  select(Report.Date...Time, PD.Case.., Incident.Location, Agency.Incident...Actual.CFS.Type, crime_type,
         Address, City, State.Zip, input_address, incident_count, NETID, NETPOP, COUNT, COUNT100,
         crime_lon, crime_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  bridgeton_crime_fishnet_final05,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_Crime_all_geocoded_with_netid_wgs84_half_mile.csv',
  quote=TRUE
)

# sanity check map
nj <- load_neighborhood(
  url='https://opendata.arcgis.com/datasets/5f45e1ece6e14ef5866974a7b57d3b95_1.geojson',
  crs=NJ_PLANAR)
cumberland <- extract_area(nj)
cumberland_bm <- collect_basemap(cumberland, crs=WGS84)

bcsf <- bridgeton_crime_sf %>% st_transform(crs=WGS84) %>% mutate(lon=sf::st_coordinates(.)[,1], lat=sf::st_coordinates(.)[,2])
ggmap(cumberland_bm) + geom_sf(data=bcsf, color='red')

#### VIOLATIONS DATA ####

mv_viol <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/Millville_violation_100.csv')
# 6486 observations but only 3376 unique addresses
mv_viol$clean_addr <- str_trim(mv_viol$Property.Location, side=c('right'))
# added dummy empty column names because I forgot to enquote the output when originally saving.
mv_viol_gc <- read.csv('~/PredictAlign/RiskProtectiveGeocoded/unique_geocoded_addresses/2020Jul22_Millville_Code_Enforcement_1-4_geocoded.csv')
table(mv_viol_gc$match_indicator)

mv_viol_gc_combined <- left_join(mv_viol, mv_viol_gc, by=c("clean_addr"="input_address"))
mv_viol_sf <- create_incidents_sf(mv_viol_gc_combined, initial_crs=WGS84, transform_crs=crs(one_mi))
addr_counts <- table(mv_viol_sf$clean_addr)
max(addr_counts) # check that duplication of address is not lost

# sanity check map
mvsf <- mv_viol_sf %>% st_transform(crs=WGS84) %>% mutate(lon=sf::st_coordinates(.)[,1], lat=sf::st_coordinates(.)[,2])
ggmap(cumberland_bm) + geom_sf(data=mvsf, color='red')

mv_viol_fishnet <- st_join(mv_viol_sf, one_mi, join=st_within)

mv_viol_fishnet_final <- mv_viol_fishnet %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(violation_lon=sf::st_coordinates(.)[,1], violation_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

mv_viol_fishnet_final <- dplyr::left_join(mv_viol_fishnet_final, one_mi_tbl, by='NETID')
mv_viol_fishnet_final <- mv_viol_fishnet_final %>%
  select(Violation.Id, Block.Lot.Qual, Property.Location, Property.Class,
         Ordinance.Id.1, Compliance.Date.1, Ordinance.1.Conditions, Violation.Date,
         Status, Status.Date, Description, Conditions, violation_type, clean_addr,
         matched_address, incident_count, NETID, NETPOP, COUNT, COUNT100,
         violation_lon, violation_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  mv_viol_fishnet_final,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/Millville_violations_all_geocoded_with_netid_wgs84.csv',
  quote=TRUE
)

# half mi fishnet
mv_viol_fishnet05 <- st_join(mv_viol_sf, half_mi, join=st_within)

mv_viol_fishnet_final05 <- mv_viol_fishnet05 %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(violation_lon=sf::st_coordinates(.)[,1], violation_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

mv_viol_fishnet_final05 <- dplyr::left_join(mv_viol_fishnet_final05, half_mi_tbl, by='NETID')
mv_viol_fishnet_final05 <- mv_viol_fishnet_final05 %>%
  select(Violation.Id, Block.Lot.Qual, Property.Location, Property.Class,
         Ordinance.Id.1, Compliance.Date.1, Ordinance.1.Conditions, Violation.Date,
         Status, Status.Date, Description, Conditions, violation_type, clean_addr,
         matched_address, incident_count, NETID, NETPOP, COUNT, COUNT100,
         violation_lon, violation_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  mv_viol_fishnet_final05,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/Millville_violations_all_geocoded_with_netid_wgs84_half_mile.csv',
  quote=TRUE
)

#### MORE VIOLATIONS ####

bt_viol <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/bridgeton_violation_100.csv')
addr_freq <- table(bt_viol$Location.Address)

bt_viol_gc <- read.csv('~/PredictAlign/RiskProtectiveGeocoded/unique_geocoded_addresses/2020Jul22_Bridgeton_Violations_geocoded.csv')

bt_viol_gc_combined <- left_join(bt_viol, bt_viol_gc, by=c("Location.Address"="input_address"))
bt_viol_sf <- create_incidents_sf(bt_viol_gc_combined, initial_crs = WGS84, transform_crs = crs(one_mi))

# sanity check map
btsf <- bt_viol_sf %>% st_transform(crs=WGS84) %>% mutate(lon=sf::st_coordinates(.)[,1], lat=sf::st_coordinates(.)[,2])
ggmap(cumberland_bm) + geom_sf(data=btsf, color='red')

bt_viol_fishnet <- st_join(bt_viol_sf, one_mi, join=st_within)

bt_viol_fishnet_final <- bt_viol_fishnet %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(violation_lon=sf::st_coordinates(.)[,1], violation_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

bt_viol_fishnet_final <- dplyr::left_join(bt_viol_fishnet_final, one_mi_tbl, by='NETID')
bt_viol_fishnet_final <- bt_viol_fishnet_final %>%
  select(Tracking.Number, Location.Address, Issue.Date, Statute, Statute.Number,
         violation_type, Address, City, State, id, X.y, X.1, X.2, matched_address,
         incident_count, NETID, NETPOP, COUNT, COUNT100,
         violation_lon, violation_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  bt_viol_fishnet_final,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_violations_all_geocoded_with_netid_wgs84.csv',
  quote=TRUE
)

# half mi fishnet
bt_viol_fishnet05 <- st_join(bt_viol_sf, half_mi, join=st_within)

bt_viol_fishnet_final05 <- bt_viol_fishnet05 %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(violation_lon=sf::st_coordinates(.)[,1], violation_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

bt_viol_fishnet_final05 <- dplyr::left_join(bt_viol_fishnet_final05, half_mi_tbl, by='NETID')
bt_viol_fishnet_final05 <- bt_viol_fishnet_final05 %>%
  select(Tracking.Number, Location.Address, Issue.Date, Statute, Statute.Number,
         violation_type, Address, City, State, id, X.y, X.1, X.2, matched_address,
         incident_count, NETID, NETPOP, COUNT, COUNT100,
         violation_lon, violation_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  bt_viol_fishnet_final05,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/Bridgeton_violations_all_geocoded_with_netid_wgs84_half_mile.csv',
  quote=TRUE
)

#### RISK AND PROTECTIVE ####

# these are already geocoded; the second one is cleaned and with corrected longitude
cumberland_risk_protective <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/Cumberland_County_Risk_and_Protective_Factor_10-1-2020-nrm.csv')
cumberland_risk_protective2 <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/risk_protect_factors.csv')

# THE CRS IS A COMPLETE GUESS. just using a common one and hoping for the best
# the Data Axle online documentation discusses lat/lon but does not explicitly state the CRS they use.
crp <- st_as_sf(cumberland_risk_protective2, coords=c('Longitude', 'Latitude'), crs=WGS84)
crp_planar <- st_transform(crp, crs=NJ_PLANAR)

# sanity check map
crpsf <- crp %>% mutate(lon=sf::st_coordinates(.)[,1], lat=sf::st_coordinates(.)[,2])
ggmap(cumberland_bm) + geom_sf(data=crpsf, color='red')

crp_fishnet <- st_join(crp_planar, one_mi, join=st_within)

crp_fishnet_final <- crp_fishnet %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(risk_protect_lon=sf::st_coordinates(.)[,1], risk_protect_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

crp_fishnet_final <- dplyr::left_join(crp_fishnet_final, one_mi_tbl, by='NETID')
crp_fishnet_final <- crp_fishnet_final %>%
  select(ABI.Number, Primary.SIC.4, Primary.SIC.6, PrimarySIC6.Description, Census.Tract,
         County.Code, Census.Block,
         NETID, NETPOP, COUNT, COUNT100,
         risk_protect_lon, risk_protect_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  crp_fishnet_final,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/risk_protect_factors_with_netid_wgs84.csv',
  quote=TRUE
)

# half mi fishnet
crp_fishnet05 <- st_join(crp_planar, half_mi, join=st_within)

crp_fishnet_final05 <- crp_fishnet05 %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(risk_protect_lon=sf::st_coordinates(.)[,1], risk_protect_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

crp_fishnet_final05 <- dplyr::left_join(crp_fishnet_final05, half_mi_tbl, by='NETID')
crp_fishnet_final05 <- crp_fishnet_final05 %>%
  select(ABI.Number, Primary.SIC.4, Primary.SIC.6, PrimarySIC6.Description, Census.Tract,
         County.Code, Census.Block,
         NETID, NETPOP, COUNT, COUNT100,
         risk_protect_lon, risk_protect_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  crp_fishnet_final05,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/risk_protect_factors_with_netid_wgs84_half_mile.csv',
  quote=TRUE
)

#### STATE POLICE CRIME DATA


#st_pol <- read.csv('~/PredictAlign/PredictAlignFishnets/cleaned_data/St_Pol_crime.csv')
# update 10/2021: the old version was annotated by Weijia to include column `crime_type`,
# but had a bunch of data inadvertently filtered out. The new version does not have the
# `crime_type` annotations yet, but does have all the data.
st_pol <- read.csv('~/PredictAlign/2020Jul28_St_Pol_Jul19_Jun20_PAP_Police_Variables-tacc-no-filters.csv')

st_pol$date <- as.Date(as.character(st_pol$Police.report.date), "%m/%d/%y")
st_pol$year <- lubridate::year(st_pol$date)
st_pol %>% group_by(year) %>% tally()

# THE CRS IS A COMPLETE GUESS. just using a common one and hoping for the best
st_pol_complete <- st_pol %>% drop_na(Latitude, Longitude)
st_pol_complete <- st_as_sf(st_pol_complete, coords=c('Longitude', 'Latitude'), crs=WGS84)
st_pol_planar <- st_transform(st_pol_complete, crs=NJ_PLANAR)

# sanity check map
st_pol_complete_sf <- st_pol_complete %>% 
  mutate(lon=sf::st_coordinates(.)[,1], lat=sf::st_coordinates(.)[,2])
ggmap(cumberland_bm) + geom_sf(data=st_pol_complete_sf, color='red')
# there are a few observations that end up in the water, but most are in the 
# Commercial Township area which is the most commonly represented area in this dataset

st_pol_fishnet <- st_join(st_pol_planar, one_mi, join=st_within)

st_pol_fishnet_final <- st_pol_fishnet %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(st_pol_lon=sf::st_coordinates(.)[,1], st_pol_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

st_pol_fishnet_final <- dplyr::left_join(st_pol_fishnet_final, one_mi_tbl, by='NETID')
st_pol_fishnet_final <- st_pol_fishnet_final %>%
  select(Data.Source, Crime.Code, Police.report.number, Police.report.date, Police.report.time.of.day,
         Location..Address...Street.Number, Location..Address...Street.Name, County, Municipality, #crime_type,
         NETID, NETPOP, COUNT, COUNT100,
         st_pol_lon, st_pol_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  st_pol_fishnet_final,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/St_Pol_crime_with_netid_wgs84.csv',
  quote=TRUE
)

# half mile fishnet
st_pol_fishnet05 <- st_join(st_pol_planar, half_mi, join=st_within)

st_pol_fishnet_final05 <- st_pol_fishnet05 %>% 
  drop_na(NETID) %>%
  st_transform(crs=WGS84) %>% 
  mutate(st_pol_lon=sf::st_coordinates(.)[,1], st_pol_lat=sf::st_coordinates(.)[,2]) %>%
  st_drop_geometry()

st_pol_fishnet_final05 <- dplyr::left_join(st_pol_fishnet_final05, half_mi_tbl, by='NETID')
st_pol_fishnet_final05 <- st_pol_fishnet_final05 %>%
  select(Data.Source, Crime.Code, Police.report.number, Police.report.date, Police.report.time.of.day,
         Location..Address...Street.Number, Location..Address...Street.Name, County, Municipality, #crime_type,
         NETID, NETPOP, COUNT, COUNT100,
         st_pol_lon, st_pol_lat, fishnet_centroid_lon, fishnet_centroid_lat)

write.csv(
  st_pol_fishnet_final05,
  '~/PredictAlign/PredictAlignFishnets/cleaned_data/St_Pol_crime_with_netid_wgs84_half.csv',
  quote=TRUE
)

#### FISHNET SUMMARIES ####

bridgeton_crime_fishnet_final$source <- 'Bridgeton Crime'
br_short <- bridgeton_crime_fishnet_final %>% select(NETID, source)

mv_viol_fishnet_final$source <- 'Millville Violations'
mv_short <- mv_viol_fishnet_final %>% select(NETID, source)

bt_viol_fishnet_final$source <- 'Bridgeton Violations'
bt_short <- bt_viol_fishnet_final %>% select(NETID, source)

crp_fishnet_final$source <- 'Cumberland Risk and Protective Factors'
crp_short <- crp_fishnet_final %>% select(NETID, source)

st_pol_fishnet_final$source <- 'Commercial Township Crime (State Police)'
st_short <- st_pol_fishnet_final %>% select(NETID, source)

all_risk_protective <- bind_rows(br_short, mv_short, bt_short, crp_short, st_short)
all_risk_protective_net <- all_risk_protective %>% group_by(NETID, source) %>% summarise(total = sum(n()))

one_mi_rp_net <- left_join(one_mi, all_risk_protective_net, by='NETID')
st_write(one_mi_rp_net, '~/PredictAlign/PredictAlignFishnets/all_risk_protect_summary_by_datasource_20211014.shp')
one_mi_rp_net <- st_read('~/PredictAlign/PredictAlignFishnets/all_risk_protect_summary_by_datasource_20211014.shp')

old_one_mi_rp_net <- st_read('~/PredictAlign/PredictAlignFishnets/all_risk_protect_summary_by_datasource.shp')

bt_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(one_mi_rp_net, source=='Bridgeton Crime'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Bridgeton Crime') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=one_mi, inherit.aes=FALSE, alpha=0)

mv_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(one_mi_rp_net, source=='Millville Violations'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Millville Violations') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=one_mi, inherit.aes=FALSE, alpha=0)

bv_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(one_mi_rp_net, source=='Bridgeton Violations'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Bridgeton Violations') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=one_mi, inherit.aes=FALSE, alpha=0)

cr_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(one_mi_rp_net, source=='Cumberland Risk and Protective Factors'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Risk and Protective Factors') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=one_mi, inherit.aes=FALSE, alpha=0)

st_map <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(old_one_mi_rp_net, source=='Commercial Township Crime (State Police)'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, color='gray', alpha=0.8) +
  geom_sf(data=st_transform(
    filter(one_mi_rp_net, source=='Commercial Township Crime (State Police)'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='State Police Crime') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=one_mi, inherit.aes=FALSE, alpha=0)

cowplot::plot_grid(bt_map, bv_map, mv_map, cr_map, st_map, nrow=2, align = "hv", axis = "lrbt")

# can we break cumberland co into smaller chunks around cities?
nj_bm <- collect_basemap(nj, crs=WGS84)

# vineland-bridgeton is its own CBSA
cbsa <- st_read('~/CooksProTX/spatial/tigris/usa_core_base_statistical_areas/cbsa_2019/cbsa_2019.shp')
nj_cbsa <- cbsa %>% filter(grepl('NJ', NAME))
head(nj_cbsa, 20)
ggmap(nj_bm) + geom_sf(data=nj_cbsa, inherit.aes=FALSE, alpha=0.8)

urban <- st_read('~/CooksProTX/spatial/tigris/usa_urban_areas/urban_areas_2019/urban_areas_2019.shp')
nj_urban <- urban %>% filter(grepl('NJ', NAME10))
ggmap(nj_bm) + geom_sf(data=nj_urban, inherit.aes=FALSE, alpha=0.8)

ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    one_mi_rp_net_summary,
    crs=crs(nj_urban)), aes(fill=total), inherit.aes=FALSE, alpha=0.8) +
  labs(title='All Crime and Violations Data') +
  theme(plot.title=element_text(size=22)) +
  #geom_sf(data=one_mi, inherit.aes=FALSE, color='black', alpha=0) +
  geom_sf(data=nj_urban, aes(size=1.5), color='black', inherit.aes=FALSE, alpha=0) +
  scale_size_identity()

# half mile fishnet summaries

bridgeton_crime_fishnet_final05$source <- 'Bridgeton Crime'
br_short05 <- bridgeton_crime_fishnet_final05 %>% select(NETID, source)

mv_viol_fishnet_final05$source <- 'Millville Violations'
mv_short05 <- mv_viol_fishnet_final05 %>% select(NETID, source)

bt_viol_fishnet_final05$source <- 'Bridgeton Violations'
bt_short05 <- bt_viol_fishnet_final05 %>% select(NETID, source)

crp_fishnet_final05$source <- 'Cumberland Risk and Protective Factors'
crp_short05 <- crp_fishnet_final05 %>% select(NETID, source)

st_pol_fishnet_final05$source <- 'Commercial Township Crime (State Police)'
st_short05 <- st_pol_fishnet_final05 %>% select(NETID, source)

all_risk_protective05 <- bind_rows(br_short05, mv_short05, bt_short05, crp_short05, st_short05)
all_risk_protective_net05 <- all_risk_protective05 %>% group_by(NETID, source) %>% summarise(total = sum(n()))

half_mi_rp_net <- left_join(half_mi, all_risk_protective_net05, by='NETID')
st_write(half_mi_rp_net, '~/PredictAlign/PredictAlignFishnets/all_risk_protect_summary_by_datasource_half_mile_20211014.shp')

bt_map05 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(half_mi_rp_net, source=='Bridgeton Crime'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Bridgeton Crime') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=half_mi, inherit.aes=FALSE, alpha=0)

mv_map05 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(half_mi_rp_net, source=='Millville Violations'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Millville Violations') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=half_mi, inherit.aes=FALSE, alpha=0)

bv_map05 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(half_mi_rp_net, source=='Bridgeton Violations'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Bridgeton Violations') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=half_mi, inherit.aes=FALSE, alpha=0)

cr_map05 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(half_mi_rp_net, source=='Cumberland Risk and Protective Factors'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Risk and Protective Factors') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=half_mi, inherit.aes=FALSE, alpha=0)

st_map05 <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(
    filter(half_mi_rp_net, source=='Commercial Township Crime (State Police)'),
    crs=WGS84), aes(fill=source), inherit.aes=FALSE, alpha=0.8) +
  labs(title='Commercial Township Crime') +
  theme(legend.position="none", plot.title=element_text(size=22)) +
  geom_sf(data=half_mi, inherit.aes=FALSE, alpha=0)

cowplot::plot_grid(bt_map05, bv_map05, mv_map05, cr_map05, st_map05, nrow=2, align = "hv", axis = "lrbt")
