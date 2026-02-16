# https://rpubs.com/ials2un/trrclmt
library(AOI)
library(climateR)
library(sf)
library(raster)
library(rasterVis)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(glue)
# libs <- c('AOI', 'climateR', 'sf', 'raster', 'rasterVis', 'dplyr')
# installed_libs <- libs %in% rownames(installed.packages())
# if(any(installed_libs == F)) {
#   install.packages(libs[installed_libs])
# }

(boyaca <-
    st_read(
      "/Users/sousekilyu/Documents/R/BeijingShanghai/data/北京市.shp"
    ))
# 1958-01-01/2021-12-01
sdate = "2020-07-01"
tc_prcp = getTerraClim(boyaca, varname = "tmax", startDate = "2020-07-01")
tc_tmp <- tc_prcp[[1]]
tc_tmp

pal <-
  colorNumeric(
    c("red", "orange", "#fcc000", "yellow", "cyan", "blue", "#3240cd"),
    values(tc_tmp),
    na.color = "transparent"
  )

leaflet() %>% 
  addTiles() %>%
  addRasterImage(raster(tc_tmp), colors = pal, opacity = 0.8) %>%
  addLegend(pal = pal,
            values = values(tc_tmp),
            title = "Rainfall-Feb.2019 [mm]")
