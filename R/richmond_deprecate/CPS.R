library("sf")         # version 0.9-8
library("mapview")    # version 2.9.0
library("ggmap")      # version 3.3.0 
library("raster")     # version 3.4-5
library("tidyverse")  # version 1.3.0


nbr <- read_sf("https://data.richmondgov.com/resource/7juf-nwis.geojson")  %>%
  st_sf() %>%
  st_transform('ESRI:102311')

nbr_diss <- nbr %>%
  mutate(dissolve = 1) %>%
  # get rid of slivers
  st_buffer(., dist = 0.1) %>%
  group_by(dissolve) %>%
  summarise()

nbr_rast_SP <- raster(as(nbr_diss, "Spatial"), nrows = 2000, ncol = 2000)


## ----basemap-------------------------------------------------------------------------------------
nbr <- read_sf("https://data.richmondgov.com/resource/7juf-nwis.geojson") %>%
  st_transform(crs = 'ESRI:102311')

nbr_bbox <- unname(st_bbox(ll(st_buffer(var_list[[3]]))))
cps_base_map   <- get_map(location = unname(st_bbox(nbr_bbox)),
                          source = "stamen",
                          maptype = "toner")
plot(cps_base_map)
### get CPS_Accepted values (add 1 column for dissolving)
cps_dissolve <- var_list[[3]] %>%
  mutate(value = 1) %>%
  #print(cps_dissolve)
  dplyr::select(value)
  print(cps_dissolve)
cps_dissolve= st_transform(cps_dissolve, crs='ESRI:102311') 
## ----fishnet-------------------------------------------------------------------------------------
net <- st_make_grid(nbr, cellsize = fishnet_grid_dim)
print(net)
# count CPS incidents per net cell - really just to get net raster into sf polygon format

net_agg <- aggregate(x = cps_dissolve, by = net, FUN = sum) %>%

  tibble::rowid_to_column(.,"geometry")
# list of net cells IDs that intersect with Richmond
net_intersect <- st_intersects(nbr, net_agg) 
# extract Richmonds net cells based on intersect ID
net_NJ <- net_agg[unique(unlist(net_intersect)),]
net_hood <- st_join(net_NJ, nbr, largest = TRUE)
listw <- nb2listw(poly2nb(as(net_NJ, "Spatial"), queen = TRUE))
