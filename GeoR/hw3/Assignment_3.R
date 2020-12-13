
#install.packages("rnaturalearth")


library(sf)
library(tidyverse)
library(rnaturalearth)
library(ggthemes)
library(knitr)
library(ggspatial)
library(tmap)
library(tmaptools)
library(lubridate)
library(raster)
library(rgeos)
library(maptools)
library(knitr)


#get the data for Latvia
latvia0 = getData('GADM', country='LVA', level=0)
latvia1 = getData('GADM' , country="LVA", level=1)

# get raster data
latviaele =  raster::getData('alt', country='LVA', mask=TRUE)

#some web configs to download the data remotely
options(download.file.method="libcurl")

#download data
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_admin_0_countries_lakes.zip", destfile="ne_10m_admin_0_countries_lakes.zip")

# extracting
unzip("ne_10m_admin_0_countries_lakes.zip")

# importing
ne_countries_sf <- st_read("ne_10m_admin_0_countries.shp")


ne_countries_sf <- ne_countries_sf %>% 
  filter(NAME == "Latvia")

download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_lake_centerlines.zip",
              destfile = "ne_10m_rivers_lake_centerlines.zip")

unzip("ne_10m_rivers_lake_centerlines.zip")

# download rivers data
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_rivers_europe.zip", 
              destfile="ne_10m_rivers_europe.zip")

unzip("ne_10m_rivers_europe.zip")

# lakes
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes.zip",
              destfile = "ne_10m_lakes.zip")

unzip("ne_10m_lakes.zip")

# lakes (Europe supplement):
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/physical/ne_10m_lakes_europe.zip",
              destfile = "ne_10m_lakes_europe.zip")

unzip("ne_10m_lakes_europe.zip")

# populated
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_populated_places.zip", 
              destfile = "ne_10m_populated_places.zip")

unzip("ne_10m_populated_places.zip")


# roads
download.file("https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/cultural/ne_10m_roads.zip",
              destfile = "ne_10m_roads.zip")

unzip("ne_10m_roads.zip")


ne_rivers_sf <- st_read("ne_10m_rivers_lake_centerlines.shp")
ne_rivers_europe_sf <- st_read("ne_10m_rivers_europe.shp")
ne_lakes_europe_sf <- st_read("ne_10m_lakes_europe.shp")
ne_lakes_sf <- st_read("ne_10m_lakes.shp")
ne_populated_places_sf <- st_read("ne_10m_populated_places.shp")
ne_roads_sf <- st_read("ne_10m_roads.shp")


ne_rivers_sf <- st_intersection(ne_rivers_sf, ne_countries_sf)
ne_rivers_europe_sf <- st_intersection(ne_rivers_europe_sf, ne_countries_sf)
ne_rivers_europe_sf <- st_intersection(ne_rivers_europe_sf, ne_countries_sf)
ne_lakes_sf <- st_intersection(ne_lakes_sf, ne_countries_sf)
ne_lakes_europe_sf <- st_intersection(ne_lakes_europe_sf, ne_countries_sf)
ne_roads_sf <- st_intersection(ne_roads_sf, ne_countries_sf)
ne_populated_places_sf <- st_intersection(ne_populated_places_sf, ne_countries_sf)

elev <- raster::getData('alt', country = "LVA", mask = TRUE)

# Map result 1
tm_shape(elev)+
  tm_raster(palette = c("darkgreen", "yellow", "orange", "red"), style = "cont", title = "m.a.s.l.")+
  tm_shape(ne_rivers_sf)+
  tm_lines("blue")+
  tm_shape(ne_rivers_europe_sf)+
  tm_lines("blue")+
  tm_shape(ne_lakes_sf)+
  tm_polygons("dodgerblue1")+
  tm_shape(ne_roads_sf)+
  tm_lines("red")+
  tm_shape(ne_populated_places_sf)+
  tm_dots("red")+
  tm_shape(ne_populated_places_sf)+
  tm_text("NAME")+
  tm_scale_bar(position = c("left", "bottom"))+
  tm_layout("Physical map of Latvia")+
  tm_credits("Bakhtiyar Garashov")



latvia1 = getData('GADM' , country="LVA", level=1)

# getting Raster Data based on predefined country code
latviaele =  raster::getData('alt', country='LVA', mask=TRUE)
plot(latviaele)


avgele = raster::extract(latviaele, latvia1, fun = mean, na.rm = TRUE, sp = T)


# Convert  to sf:
avgele_sf = st_as_sf(avgele)

# round the mean value 
avgele_sf <- avgele_sf %>% 
  mutate(LVA_msk_alt2 = round(LVA_msk_alt, 0))


#temprature variations map
climate = raster::getData('worldclim', var ='bio', res = 0.5, lon = 26, lat = 58)
climate_2 <- climate$bio1_16

climate_2 <- crop(climate_2, extent(ne_countries_sf))
climate_masked <- mask(climate_2, ne_countries_sf)

climate_masked <- climate_masked / 10 =
plot(st_geometry(ne_countries_sf))
plot(climate_masked, add= T, col = topo.colors(20))
plot(st_geometry(ne_countries_sf), add = T)

#Map result 2

tm_shape(climate_masked)+
  tm_raster("bio1_16", palette = c("darkgreen", "grey90", "orange"), style = "cont")+
  tm_shape(ne_populated_places_sf)+
  tm_text("NAME")

ne_populated_places_sp <- as(ne_populated_places_sf, "Spatial")



place_temp <- raster::extract(climate_masked, ne_populated_places_sp, fun = mean, na.rm=TRUE, sp = T)



place_temp@data %>% 
  dplyr::select(NAME, bio1_16) %>% 
  arrange(-bio1_16) %>% 
  kable()



which.max(place_temp$bio1_16)
colnames(place_temp)[apply(place_temp,2,which.max)]


