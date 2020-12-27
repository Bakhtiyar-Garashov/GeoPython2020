library(XML)
library(rvest)
library(tidyverse)
library(sf)
library(maptools)
library(gplots)
library(lubridate)
library(ggspatial)
library(ggmap)
library(gstat)
library(GISTools)
library(knitr)
library(ggthemes)
library(tmap)
library(spatstat)
library(curl)
library(ggsn)# library to download data from web
library(GISTools)

url <- "http://aasa.ut.ee/Rspatial/data/snow_duration_days_2.csv"

# define the name for downloaded file
destfile <- "snow_coverage.csv"

# download the data according to previous parameters
curl_download(url, destfile)

# import data to R
snowCoverage <- read.csv(file = 'snow_coverage.csv',head = TRUE, sep=";")

ggplot()+
  geom_point(data = snowCoverage, aes(x = lon, y=lat))

# Station names are wrongly encoded; set the encoding to UTF-8:
#snowCoverage$station <- repair_encoding(as.vector(snowCoverage$station), from ="UTF-8")

# convert columns from character to numeric:
snowCoverage <- snowCoverage %>% 
  mutate(lon = as.numeric(as.character(lon)), 
         lat= as.numeric(as.character(lat)),
         duration = as.numeric(as.character(duration)),
         change=as.numeric(as.character(change)))



# Spatial context is still almost missing. Download the contour of Estonia:

download.file("http://aasa.ut.ee/GisMaps/data/est_wgs84.zip", destfile = "est_wgs84.zip") 
unzip("est_wgs84.zip")

eesti_shp <- st_read("eestimaa_wgs84.shp")

est_cont_3301 <- st_transform(eesti_shp, 3301)

# generalization:
est_cont_3301_smpl <- st_simplify(est_cont_3301, preserveTopology = F, dTolerance = 200)



est_cont_3301_sp <- as_Spatial(est_cont_3301)



# create simple feature: 
snowCover_sf <- st_as_sf(snowCoverage, coords= c("lon", "lat"), crs = 3301)

glimpse(snowCover_sf )

# definition of CRS lest97 (epsg:3301):
lest97 <- CRS("+proj=lcc +lat_1=59.33333333333334 +lat_2=58 +lat_0=57.51755393055556 +lon_0=24 +x_0=500000 +y_0=6375000 +ellps=GRS80 +units=m +no_defs")

# definition of WGS84 (epsg:4326):
wgs84 <- CRS("+proj=longlat +ellps=WGS84")

# Make the hexagonal grid (points)
grid_hex <- spsample(est_cont_3301_sp, type = "hexagonal", cellsize = 5000) # gridding may be time consuming!
#plot(grid_hex)


# convert it to polygons:
grid_hex <- HexPoints2SpatialPolygons(grid_hex, dx = 5000)

#define CRS:
proj4string(grid_hex) <- lest97
plot(grid_hex)

glimpse(snowCover_sf)


# Convert temperature observatins from sf to SPDF:
snowCover_sp_3301 <- as(snowCover_sf, "Spatial")
# calculate IDW for hexagons:
P_idw_hex <- gstat::idw(duration~1, snowCover_sp_3301, newdata = grid_hex, idp = 2)

class(P_idw_hex)


# convert SpatialPolygonDataFrame to sf:
rslt_hex <- st_as_sf(P_idw_hex)


# plot it:
ggplot()+
  theme_map()+
  
  theme(panel.grid = element_line(color = "grey60", size = 0.15),
        legend.position = "right")+
  geom_sf(data = rslt_hex, aes(fill = var1.pred), col = "grey20", size = 0.1)+
  scale_fill_viridis_c(option = "plasma")+
  labs(title = "Snow coverage in Estonia",subtitle = "Bakhtiyar Garashov",
       fill = "Days")+
  annotation_scale()+
  annotation_north_arrow(pad_x = unit(0.5, "cm"),
                         pad_y = unit(6, "cm"),
                         which_north = "grid")
