library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(lubridate)
library(data.table)
library(magrittr)
library(dplyr)
library(ggtext)
# https://wilkelab.org/ggtext/
library(glue)

# https://rpubs.com/boyerag/297592
# https://data.tpdc.ac.cn/zh-hans/data/8028b944-daaa-4511-8769-965612652c49/

setwd('/Users/sousekilyu/Documents/R/ClimateR')

nc_data <-
  nc_open('data/prec_CMFD_V0106_B-01_01dy_010deg_201801-201812.nc')
# Save the print(nc) dump to a text file
{
  sink('data/prec_CMFD_V0106_B-01_01dy_010deg_201801-201812_meta.txt')
  print(nc_data)
  sink()
  }
lon <- ncvar_get(nc_data, "lon")
lat <- ncvar_get(nc_data, "lat", verbose = F)
t <- ncvar_get(nc_data, "time")

head(lon) # look at the first few entries in the longitude vector

ndvi.array <-
  ncvar_get(nc_data, "prec") # store the data in a 3-dimensional array
dim(ndvi.array)

fillvalue <- ncatt_get(nc_data, "prec", "_FillValue")
fillvalue

nc_close(nc_data)


# ndvi.array[ndvi.array == fillvalue$value] <- NA
# ndvi.slice <- ndvi.array[, , 180]
# dim(ndvi.slice)
# r <-
#   raster(
#     t(ndvi.slice),
#     xmn = min(lon),
#     xmx = max(lon),
#     ymn = min(lat),
#     ymx = max(lat),
#     crs = CRS(
#       "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"
#     )
#   )
# r <- flip(r, direction = 'y')
# plot(r)
# 
# writeRaster(r,
#             "data/prec_CMFD_V0106_B-01_01dy_010deg_201801-201812.tif",
#             "GTiff",
#             overwrite = TRUE)
# 


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


##
city_loc <- fread('data/全国县级以上地名代码及经纬度2.csv')
city_loc %<>%
  rename(lon = `东经`, lat = `北纬`, addr = `地名`) %>% 
  mutate(lat2 = paste0(format(round(lat,1), nsmall = 1), "°N"),
         lon2 = paste0(format(round(lon,1), nsmall = 1), "°E"),
         loc = paste0("(", lat2, ", ", lon2, ")")) %>% 
  unique()

city_loc_list <- city_loc %>%
  filter(
    addr %in% c(
      '北京市',
      '上海市',
      '广东省广州市',
      '陕西省西安市',
      '新疆乌鲁木齐市',
      '黑龙江省哈尔滨市',
      '云南省昆明市',
      '湖北省武汉市',
      '四川省成都市',
      '山东省青岛市',
      '西藏拉萨市',
      '福建省福州市',
      '甘肃省兰州市',
      '河南省郑州市'
    )
  ) %>%
  unique()
## get data
toolik_df <- data.frame()
for (i in 1:dim(city_loc_list)[1]) {
  .toolik_lon <- city_loc_list$lon[i]
  .toolik_lat <- city_loc_list$lat[i]
  .toolik_series <-
    raster::extract(r_brick, SpatialPoints(cbind(.toolik_lon, .toolik_lat)), method =
              'simple')
  .toolik_df <-
    data.frame(
      day = seq(ymd('2018-01-01'), ymd('2018-12-31'), by = '1 day'),
      precipitation = t(.toolik_series),
      city = city_loc_list$addr[i],
      loc = city_loc_list$loc[i]
    )
  toolik_df <- rbind(toolik_df, .toolik_df)
}

###
toolik_df %<>%
  unique() %>% 
  group_by(city) %>%
  arrange(city, day) %>%
  mutate(precipitation_s = zoo::rollmean(precipitation, 7, na.pad = TRUE))
toolik_df_week <- toolik_df %>%
  mutate(week = week(day)) %>%
  group_by(week, city) %>%
  summarise(precipitation = mean(precipitation, na.rm = TRUE))
toolik_df_month <- toolik_df %>%
  mutate(month = month(day)) %>%
  group_by(month, city) %>%
  summarise(precipitation = mean(precipitation, na.rm = TRUE))
