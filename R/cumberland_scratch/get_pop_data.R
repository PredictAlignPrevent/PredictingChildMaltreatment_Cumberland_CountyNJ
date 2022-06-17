#To get population data for all the below variables

vars_sf1 <- c("P0010001",  # Total Population,
              "H00010001", # housing units
              "H0030003",  # vacant housing,
              "H0040002",  # owned and mortgaged,
              "H0040004",  # renter occupied
              "H0060001",  # occupied housing units
              "H0130001",  # Houshold size
              "P0360001",  # Population in families
              "P0360002",  # Population under 18yo
              "P0120003",  # Male under 5yo
              "P0120004",  # Male under 5-9yo
              "P0120005",  # Male under 10-14yo
              "P0120006",  # Male under 15-17yo
              "P0120027",  # Female under 5yo
              "P0120028",  # Female under 5-9yo
              "P0120029",  # Female under 10-14yo
              "P0120030"   # Female under 15-17yo
)
vars_sf1_desc <- c("Total Pop",
                   "Housing units",
                   "Housing, vacant",
                   "Housing, owned",
                   "Housing, rented",
                   "Housing, occupied units",
                   "Houshold size",
                   "Families Pop",
                   "Pop, under 18y",
                   "Male under 5y",
                   "Male under 5-9y",
                   "Male under 10-14y",
                   "Male under 15-17y",
                   "Female under 5y",
                   "Female under 5-9y",
                   "Female under 10-14y",
                   "Female under 15-17y")
                   
vars_names <- data.frame(variable = vars_sf1, var_name = vars_sf1_desc, stringsAsFactors = FALSE)
key <- read.table(
  '~/CooksProTX/us_census_api_key.txt', header=TRUE, colClasses='character'
)
census_api_key(key)

NJ_block_sf1 <- get_decennial(geography = "block group", variables = vars_sf1, year = 2010,
 state = 'NJ', county = 'Cumberland County', geometry = TRUE, show_call = TRUE) 
#%>%
#st_transform(crs = 'WGS84')

NJ_block_sf1<- get_acs(
  geography='block group',
  variables=vars_sf1,
  state='NJ',
  county='Cumberland County',
  year=2019,
  summary_var = "P0010001",
  survey='acs5',
  show_call=TRUE,
  geometry=TRUE
) 
#%>% st_transform(crs=nbr_crs)
