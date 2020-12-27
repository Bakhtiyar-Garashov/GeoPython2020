
library(gstat)
library(GISTools)
library(knitr)
library(ggthemes)
library(tmap)
library(spatstat)
library(data.table)
library(dplyr)
library(raster)
library(sp)
library(XML)
library(rvest)
library(tidyverse)
library(sf)
library(maptools)
library(gplots)
library(lubridate)
library(ggspatial)
library(ggmap)
library(tmap)
library(plotly)
library(ggplot2)
library(ggsn)
library(ggspatial)


#download file
download.file("http://aasa.ut.ee/Rspatial/data/osm_buildings_trt.zip", destfile = "osm_buildings_trt.zip") 

unzip("osm_buildings_trt.zip")

osm <- st_read("osm_buildings_trt.shp")

osm_buildings <- st_transform(osm, 3301) #converting to projected estonian one as we have to do calculations


#download adminstrative data
download.file("https://geoportaal.maaamet.ee/docs/haldus_asustus/asustusyksus_shp.zip?t=20201201020022",mode='wb', destfile = "asustusyksus.zip") 

unzip("asustusyksus.zip")

asustusyksus <- st_read("asustusyksus_20201201.shp")


#make subset of the data
tartu_maakond <- asustusyksus %>% 
  filter(MKOOD == "0079")

tartu_linn <- asustusyksus %>% 
  filter(ANIMI == "Tartu linn")



#create 5km buffer
buffer = st_buffer(tartu_linn, 5000) 

#clip buildings within that buffer
tartu_buildings <- osm_buildings %>% 
  filter(st_within(st_transform(osm_buildings,3301), st_transform(buffer,3301), sparse = FALSE))


#clip administrative boundaries contain the osm buildings
tartu_adm <- tartu_maakond %>% 
  filter(st_intersects(st_transform(tartu_maakond,3301), st_transform(osm_buildings,3301), sparse = FALSE))

#Finding the count
tartu_maakond$pt_count <- lengths(st_intersects(st_transform(tartu_maakond,3301), st_transform(osm_buildings,3301)))

final_dataset <- subset(tartu_maakond, pt_count >0)


# Output #1
ggplot()+
  geom_sf(data = buffer, colour="black", fill=NA)+
  geom_sf(data = tartu_buildings, color="red", size=0.5, alpha=0.1)+
  labs(title = "Spread of buildings within 5km distance",subtitle = "Bakhtiyar Garashov")+
  annotation_scale()+
  annotation_north_arrow(pad_x = unit(10, "cm"),
                         pad_y = unit(10, "cm"),
                         which_north = "grid")




# Output #2
tm_shape(final_dataset) + tm_polygons(col="pt_count",style = "log10_pretty",palette="YlGnBu", border.col = "white", title="No of Buildings")+
  tm_text("pt_count", just = "center", xmod = 0, size = 0.5)+
  tm_layout(
    inner.margins = 0.08,
    main.title = "Number of buildings in administrative units", 
    main.title.position = "center")+
  tm_compass(
    text.size = 0.8,
    show.labels = 1,
    cardinal.directions = c("N"),
    lwd = 1,
    position=c("left", "top"),
  )+
  tm_scale_bar(position=c("right", "bottom"))+
  tm_credits("Author: Bakhtiyar Garashov", size = 0.7, position = c("right", "top"),
             bg.color = NA, bg.alpha = NA)

