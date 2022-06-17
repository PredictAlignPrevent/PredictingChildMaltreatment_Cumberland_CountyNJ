library("sf")            # Spatial data objects and methods
library("raster")        # cell-based spatial operations
library("tidyverse")     # data manipulation framework
library("Hmisc")         # using cut2() functions for ggplot legends
library("tidycensus")
library("tigris")

load_neighborhood <- function(url, crs){
  # The geojson contains the vector data for all counties in New Jersey
  nbr <- read_sf(url) %>% 
    st_sf() %>%
    st_transform(crs)
  return(nbr)
}

extract_area <- function(data, area_column='COUNTY', area_name='CUMBERLAND'){
  filtered <- data %>% filter(.data[[area_column]] == area_name)
  return(filtered)
}

collect_basemap <- function(sf_data, crs){
  bbox <- unname(st_bbox(st_transform(sf_data, crs=crs)))
  base_map <- get_stamenmap(bbox=bbox, maptype="toner", force=TRUE)
  return(base_map)
}

mi_to_m <- function(mi){return(mi*1609)}

collect_population <- function(key_path, crs, state='NJ', county='Cumberland County', year=2019){
  # Simple wrapper to get_acs() assuming you want total population by block group
  
  key <- read.table(key_path, header=TRUE, colClasses='character')
  census_api_key(key)
  
  # use the American Community Survey (ACS) to get population by block group. 
  # the default year is 2019 (most recent available at run time).
  nbr1 <- get_acs(
    geography='block group',
    variables='B01003_001',
    state=state,
    county=county,
    year=year,
    survey='acs5',
    show_call=TRUE,
    geometry=TRUE
  ) %>% st_transform(crs=crs)
  
  # add population density
  nbr1$block_area <- st_area(nbr1$geometry)
  nbr1$pop_rate = nbr1$estimate/nbr1$block_area # the population data are in column "estimate"
  
  return(nbr1)
}

create_population_fishnet <- function(pop_data, cover_area, fishnet_size, fishnet_units='mi'){
  # check that units are correct
  stopifnot(fishnet_units=='mi')
  
  # proceed with grid
  print(paste('Creating fishnet grid of dimensions', fishnet_size, fishnet_units, 'by', fishnet_size, fishnet_units, sep=' '))
  grid_width_m = mi_to_m(fishnet_size)
  print(paste('Fishnet width is', grid_width_m, 'meters', sep=' '))
  net <- st_make_grid(x=cover_area, cellsize = grid_width_m, what='polygons', square=TRUE) 
  
  # convert the fishnet raster into a sf polygon dataset and add a "net_id" column
  print('Rasterizing fishnet.')
  net_agg <- st_as_sf(net) %>% tibble::rowid_to_column(., "net_id")
  print(paste('Fishnet contains', as.character(length(unique(net_agg$net_id))), 'cells', sep=' '))
  
  # list of net cells IDs that intersect with the cover area (if cover area is irregular)
  print('Intersecting fishnet with population data.')
  net_intersect <- st_intersects(cover_area, net_agg) 
  net_cover_area <- net_agg[unique(unlist(net_intersect)),]
  net_cover_area$net_area <- st_area(net_cover_area$x)
  net_blocks_intersect <- st_intersection(pop_data, net_cover_area)
  print(paste('Dimensions of intersection data', 
              dim(net_blocks_intersect), sep=': '))
  
  # group by fishnet cell and calc block stats.
  print('Calculating block and fishnet intersection areas.')
  net_blocks_intersect <- net_blocks_intersect %>%
    # calculate the area of each tract/fishet intersection
    mutate(intersect_area = st_area(net_blocks_intersect) ) %>%
    # group by the fishnet ID
    group_by(net_id) %>%
    # calculate the fraction of each block group covered by a fishnet cell
    # then assume the population is proportional to the overlap area
    mutate(cnt = n(), pcnt_of_block = intersect_area/block_area, intersect_pop = estimate * pcnt_of_block) %>%
    arrange(net_id)
  
  print('Summarizing fishnet population sizes.')
  fishnet_pop <- net_blocks_intersect %>%
    group_by(net_id) %>%
    summarise(net_pop = sum(intersect_pop)) 
  
  # drop units (required for plotting)
  fishnet_pop$unitless_net_pop <- units::drop_units(fishnet_pop$net_pop)
  
  return(fishnet_pop)
}

create_incidents_sf <- function(incident_data, initial_crs, transform_crs){
  incidents_shp <- incident_data %>% drop_na(lat, lon) %>%
    st_as_sf(.,coords=c("lon","lat"), crs=initial_crs) %>%
    st_transform(transform_crs)
}

# point-in polygon join between lat/lon coordinates and an existing fishnet grid,
# where the fishnet grid includes population data,
# combined with incidence calculation (points per 100 people)
create_incidents_fishnet <- function(pop_fishnet, incidents_data){
  
  # get the fishnet cell for each incident; discard geometry as only incident lat/lon are retained by st_join
  incidents_net <- st_join(incidents_data, pop_fishnet, join=st_within) %>%
    st_drop_geometry() 
  
  # remove any incidence data outside the fishnet area and calculate the final number of incidents in each fishnet cell
  incidents_grp_net_pt1 <- incidents_net %>% 
    drop_na(net_id) %>%
    dplyr::select(net_id, incident_count) %>%
    group_by(net_id) %>% 
    summarise(incident_count = sum(incident_count))
  
  # add back the fishnet polygon geometry by rejoining the fishnet data
  incidents_grp_net <- st_as_sf(left_join(pop_fishnet, incidents_grp_net_pt1, by='net_id'), crs=crs(pop_fishnet))
  
  # replace NA with 0; calculate incidence per 100
  incidents_grp_net$incident_count <- replace_na(incidents_grp_net$incident_count, 0)
  incidents_grp_net$inc_per_100 <- units::drop_units((incidents_grp_net$incident_count/incidents_grp_net$net_pop)*100)
  
  return(incidents_grp_net)
}

fix_bins <- function(bin){
  listified_bin <- as.numeric(unlist(str_extract_all(as.character(bin), "\\d+\\.*\\d*")))
  return(listified_bin[1])
}

bin_equal_n_obs <- function(data, to_bin, bin_name, n){
  
  # separate out empty cells
  data_empty <- data %>% filter(.data[[to_bin]] == 0)
  data_empty$bin_name <- '(0,0]'
  
  # bin the non-empty cells into groups with equal number of observations
  data_binned <- data %>% filter(.data[[to_bin]] > 0)
  data_binned[[bin_name]] <- cut_number(data_binned[[to_bin]], n=n, ordered_result=TRUE)
  
  # combine empty and non-empty data
  data_binned_final <- bind_rows(data_empty, data_binned)
  
  # extract lower bound; order by lower bound to sort incidence bins
  data_binned_final$lower_bound <- sapply(data_binned_final[[bin_name]], fix_bins)
  data_binned_final <- data_binned_final %>% arrange(lower_bound)
  
  # reassign incidence bin factor levels in the correct order (now conveniently sorted by lower_bound)
  data_binned_final[[bin_name]] <- factor(data_binned_final[[bin_name]], levels = unique(data_binned_final[[bin_name]]))
  return(data_binned_final)
}

incidence_and_count_bins <- function(incidence_fishnet, inc_col, count_col, cut_fxn, n=10){
  incidence_fishnet <- incidence_fishnet %>% 
  mutate(count_bins = cut_fxn(.data[[count_col]], n=n, ordered_result=TRUE)) %>%
  mutate(incidence_bins = cut_fxn(.data[[inc_col]], n=n, ordered_result=TRUE))
  return(incidence_fishnet)
}

assign_risk_cat <- function(count, quantile_cutoffs){
  #check low end of range
  if(count < quantile_cutoffs[1]){
    return(1)
    # check high end of range
  }else if(count > quantile_cutoffs[4]){
    return(5)
    # check middle of range
  }else{
    if(count < quantile_cutoffs[2]){
      return(2)
    }else if(count > quantile_cutoffs[3]){
      return(4)
      # if count > cutoff 2 but less than cutoff 3
    }else{
      return(3)
    }
  }
}

tally_percent <- function(data, groupby, percentby){
  
  grp <- data %>% group_by_at(groupby) %>% summarise(count=n())
  total <- grp %>% group_by_at(percentby) %>% summarise(total=sum(count))
  grp_total <- left_join(grp, total, by=percentby)
  grp_total$percent <- (grp_total$count / grp_total$total) * 100
  
  return(grp_total)
  
}

# point-in polygon join between lat/lon coordinates and an existing fishnet grid
apply_fishnet <- function(geocode_data_sf, fishnet, fishnet_centroids, crs){
  
  geocode_fishnet <- st_join(geocode_data_sf, fishnet, join=st_within)
  geocode_fishnet_centroids <- left_join(geocode_fishnet, fishnet_centroids, 
                                         by=c('net_id', 'year', 'fn_width', 'include_area'))
  
  geocode_fishnet_centroids <- geocode_fishnet_centroids %>% 
    drop_na(net_id) %>%
    st_transform(crs=crs) %>% 
    mutate(data_lon=sf::st_coordinates(.)[,1], data_lat=sf::st_coordinates(.)[,2]) %>%
    st_drop_geometry()
  
  return(geocode_fishnet_centroids)
}
