library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(lubridate)
library(data.table)
library(ggtext)
# https://wilkelab.org/ggtext/
library(glue)
library(showtext)

# https://rpubs.com/boyerag/297592
# https://data.tpdc.ac.cn/zh-hans/data/8028b944-daaa-4511-8769-965612652c49/

setwd('/Users/sousekilyu/Documents/R/ClimateR')

nc_data <-
  nc_open('data/shum_CMFD_V0106_B-01_01dy_010deg_201801-201812.nc')
# Save the print(nc) dump to a text file
{
  sink('data/shum_CMFD_V0106_B-01_01dy_010deg_201801-201812_meta.txt')
  print(nc_data)
  sink()
  }
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector

ndvi.array <-
  ncvar_get(nc_data, "shum") # store the data in a 3-dimensional array
dim(ndvi.array)

fillvalue <- ncatt_get(nc_data, "shum", "_FillValue")
fillvalue

nc_close(nc_data)

## Extract data at a study site
r_brick <-
  brick(
    ndvi.array,
    xmn = min(lat),
    xmx = max(lat),
    ymn = min(lon),
    ymx = max(lon),
    crs = CRS(
      "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
    )
  )
# note that you may have to play around with the transpose (the t() function) and flip() before the data are oriented correctly. In this example, the netcdf file recorded latitude on the X and longitude on the Y, so both a transpose and a flip in the y direction were required.
r_brick <- flip(t(r_brick), direction = 'y')

## get data
shum_df <- data.frame()
for (i in 1:dim(city_loc_list)[1]) {
  .toolik_lon <- city_loc_list$lon[i]
  .toolik_lat <- city_loc_list$lat[i]
  .toolik_series <-
    raster::extract(r_brick, SpatialPoints(cbind(.toolik_lon, .toolik_lat)), method =
              'simple')
  .toolik_df <-
    data.frame(
      day = seq(ymd('2018-01-01'), ymd('2018-12-31'), by = '1 day'),
      humidity = t(.toolik_series),
      city = city_loc_list$addr[i]
    )
  shum_df <- rbind(shum_df, .toolik_df)
}

###
shum_df %<>%
  unique() %>% 
  group_by(city) %>%
  arrange(city, day) %>%
  mutate(humidity_s = zoo::rollmean(humidity, 7, na.pad = TRUE))
shum_df_week <- shum_df %>%
  mutate(week = week(day)) %>%
  group_by(week, city) %>%
  summarise(humidity = mean(humidity, na.rm = TRUE))
shum_df_month <- shum_df %>%
  mutate(month = month(day)) %>%
  group_by(month, city) %>%
  summarise(humidity = mean(humidity, na.rm = TRUE))
