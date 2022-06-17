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
library("SpatialKDE")

source('~/predict-align-prevent/R/fishnet.r')
source('~/predict-align-prevent/R/map_themes.r')

# load and prep general purpose data
nj <- load_neighborhood()
cumberland <- extract_area(nj)
cumberland_bm <- collect_basemap(cumberland)

# graph Weijia predictions
#predict1 <- read.csv('~/PredictAlign/count_predictions_20210611.csv')
predict05 <- read.csv('~/PredictAlign/count_predictions_20210615_county.csv')
#fishnet1 <- st_read('~/PredictAlign/PredictAlignFishnets/cumberland_one_mile_fishnet.shp')
fishnet05 <- st_read('~/PredictAlign/PredictAlignFishnets/cumberland_half_mile_fishnet.shp')
predict <- left_join(fishnet05, predict05, by=c("NETID"))
predict$risk_by_count100_preds <- factor(predict$risk_by_count100_preds, levels=c(1, 2, 3, 4, 5))
predict$risk_by_count_preds <- factor(predict$risk_by_count_preds, levels=c(1, 2, 3, 4, 5))
predict$risk_by_count100 <- factor(predict$risk_by_count100, levels=c(1, 2, 3, 4, 5))
predict$risk_by_count <- factor(predict$risk_by_count, levels=c(1, 2, 3, 4, 5))

names(predict) <- c('NETID', 'NETPOP.x', 'COUNT.x', 'COUNT100.x', 'NETPOP.y', 'lat', 'lon', 'year',
                    'area', 'COUNT.y', 'COUNT100.y', 'risk', 'risk100', 'COUNTpred', 'abs_err', 
                    'COUNT100pred', 'riskpred', 'risk100pred', 'geometry')

st_write(predict, '~/PredictAlign/PredictAlignFishnets/cumberland_half_mile_fishnet_predictions_20210615.shp')

pred_by_count <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(predict, crs=4326), aes(fill=risk_by_count_preds), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  facet_grid(.~year) +
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
  labs(title='Risk by Count Predictions')
pred_by_inc <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(predict, crs=4326), aes(fill=risk_by_count100_preds), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  facet_grid(.~year) +
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
  labs(title='Risk by Count100 Predictions')

cowplot::plot_grid(pred_by_count, pred_by_inc, nrow=2, align = "hv", axis = "lrbt")


raw_by_count <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(predict, crs=4326), aes(fill=risk_by_count), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  facet_grid(.~year) +
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
  labs(title='Risk by Count Original')
raw_by_inc <- ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(predict, crs=4326), aes(fill=risk_by_count100), inherit.aes = FALSE, color = NA, alpha = 0.8) +
  facet_grid(.~year) +
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
  labs(title='Risk by Count100 Original')

cowplot::plot_grid(raw_by_count, raw_by_inc, nrow=2, align = "hv", axis = "lrbt")

cowplot::plot_grid(raw_by_count, pred_by_count, raw_by_inc, pred_by_inc, nrow=4, align = "hv", axis = "lrbt")

# creating shapefile for Weijia (half mile grid)

yrlypred <- read.csv('~/PredictAlign/predictions_basedon_yearly_average.csv')

yrlypredshp <- left_join(yrlypred, fishnet05, by='NETID') %>% select(-lat, -long) %>% st_as_sf(crs=crs(fishnet05))
st_write(yrlypredshp, '~/PredictAlign/PredictAlignFishnets/predictions_basedon_yearly_average.shp')

yrlypredshp$risk_by_count_preds <- factor(yrlypredshp$risk_by_count_preds, levels=c(1, 2, 3, 4, 5))
ggmap(cumberland_bm) + 
  geom_sf(data=st_transform(yrlypredshp, crs=4326), aes(fill=risk_by_count_preds), inherit.aes = FALSE, color = NA, alpha = 0.8) +
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
  labs(title='Predicted Risk by Count')

# attempt at KDE plot

names(yrlypred)[names(yrlypred)=='long'] <- 'lon'
elongaged_yrlypred <- yrlypred[rep(row.names(yrlypred), yrlypred$risk_by_count_preds), ]
ggmap(cumberland_bm)+
  stat_density2d(data=elongaged_yrlypred, geom="polygon", aes(fill = risk_by_count_preds, alpha=0.8), contour=TRUE) +
  scale_fill_manual(values=c("5"="#FFFF00", "4"="#90EE90", "3"="013220", "2"="#00008B", "1"="#800080"))
