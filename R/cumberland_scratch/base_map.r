
setwd('~/predict-align-prevent') # set working dir as repo directory
source('R/FUNCTIONS_VAPAP.R', echo = TRUE, keep.source = TRUE)

library("sf")         # version 0.9-8
library("mapview")    # version 2.9.0
library("ggmap")      # version 3.3.0 
library("raster")     # version 3.4-5
library("tidyverse")  # version 1.3.0

#######################################################
## CREATE AN EMPTY RASTER                            ##
#######################################################

# all New Jersey counties in a single geojson file
nbr <- read_sf("https://opendata.arcgis.com/datasets/5f45e1ece6e14ef5866974a7b57d3b95_1.geojson", quiet=FALSE) %>%
  st_transform('ESRI:102311')  %>% # NAD_1983_HARN_StatePlane_New_Jersey_FIPS_2900
  filter(COUNTY=='CUMBERLAND') # subset to just cumberland county

# turn into empty raster which will later contain heatmap data
nbr_diss <- nbr %>%
  mutate(dissolve = 1) %>%
  # get rid of slivers
  st_buffer(., dist = 0.1) %>%
  group_by(dissolve) %>%
  summarise()

nbr_rast_SP <- raster(as(nbr_diss, "Spatial"), nrows = 2000, ncol = 2000)

#######################################################
## DOWNLOAD THE BASEMAP                              ##
#######################################################

# re-download the geojson file, filter, and use to get basemap
nbr <- read_sf("https://opendata.arcgis.com/datasets/5f45e1ece6e14ef5866974a7b57d3b95_1.geojson", quiet=FALSE) %>%
  st_transform('ESRI:102311')  %>% # NAD_1983_HARN_StatePlane_New_Jersey_FIPS_2900
  filter(COUNTY=='CUMBERLAND')

# here I define the bounding box for collecting the basemap by Cumberland county; while the original example
# restricted the area to only the bounding box (plus a small buffer) that contained the maltreatment data
nbr_bbox <- unname(st_bbox(ll(nbr))) # note the ll() function is devined in R/FUNCTIONS_VAPAP.R

# the original version of the code used get_map(), a wrapper to get_googlemap(), get_openstreetmap(), and get_stamenmap()
# I had more success using the get_stamenmap() function directly
cps_base_map   <- get_stamenmap(bbox = nbr_bbox, maptype = "toner", force=TRUE)
plot(cps_base_map)
