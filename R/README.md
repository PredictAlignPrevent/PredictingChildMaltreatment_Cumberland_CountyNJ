# Cumberland County fishnets, urban areas and maltreatment aggregations shapefile

Data processing script on [github](https://github.com/TACC/predict-align-prevent/blob/main/R/cumberland_annual_urban_rural.r).

Columns names and descriptions

|name | description|
|:---|:---|
|`net_id`|fishnet grid cell ID|
|`net_pop`|areal weighted estimate of fishnet grid cell population based on census block group level populations|
|`count`|incident count|
|`count100`|incident count per 100 people (incidence)|
|`area`|name of the urban area; one of "rural", "Laurel Lake, NJ Urban Cluster", "Vineland, NJ Urbanized Area",  "Bridgeton, NJ Urban Cluster", or "Vineland and Laurel Lake overlap" |
|`count_risk`|percentile-based risk category (1 through 5) derived from count data|
|`inc_risk`|percentile-based risk category (1 through 5) derived from incidence (count per 100) data|
|`year`|intake incident year|
|`fn_width`|fishnet grid cell resolution (width of one fishnet grid square) in miles; one of '0.5' or '1'|
|`type`|summary type; one of "raw\_count" or "unique\_addr\_child\_age\_combination"|
|`geometry`|polygon data defining the fishnet grid cell|


