

#add libs to use
library(ggplot2)
library(ggsn)
library(curl)# library to send request to url over http network protocol
library(tidyverse)
library(ggthemes) 
library(RColorBrewer)
library(GISTools)
library(tmap)
library(plotly)
library(GISTools)  
library(knitr)
library(stringr)
library(readxl)
library(sf) 
library(maptools) 
library(spatstat) 
library(tmap) 


#remote data to download
url = "http://aasa.ut.ee/Rspatial/data/FarmedAnimalsByLocation_31102018.xlsx"

data = "FarmedAnimalsByLocation_31102018.xlsx"

curl_download(url, data)

# import data to R
animals = read_excel(data)



#Make Subset of the file with specific columns as required
cattle_sub = animals %>% select(`action place`, `estonian holstein cattle`, `estonian red cattle`,  `estonian native cattle`, `beef cattle`, sheeps, goats, pigs, `X koordinaat`, `Y koordinaat`, municipality)


#changing column names
cattle_sub = cattle_sub %>% rename(action_place = `action place`,
                                  estonian_holstein_cattle = `estonian holstein cattle`,
                                  estonian_red_cattle = `estonian red cattle`,
                                  estonian_native_cattle = `estonian native cattle`, 
                                  beef_cattle = `beef cattle`, 
                                  X_coord = `X koordinaat`,
                                  Y_coord = `Y koordinaat`)


#conversion of columns to numeric
cattle_sub = cattle_sub %>% mutate(X_coord = as.numeric(X_coord), Y_coord = as.numeric(Y_coord))


#conversion to long format

cattle_long = gather(cattle_sub, "key", "value", 2:8)

cattle_long = cattlelong %>% 
  filter(value > 0)


# adding geospatial data from downloaded source on local machine
unzip("omavalitsus_shp.zip")



# import shp and transform the crs 
municipality =  st_read("omavalitsus_20201101.shp", quiet = T)


st_transform (municipality,3301)



cattle_df = cattle_long %>% 
  mutate(municip_key = str_to_lower(municipality))

municip = municipality %>% 
  mutate(municip_key = str_to_lower(ONIMI))



#create a geodataframe and spatial joining

cattlegdf = st_as_sf(cattle_df, coords = c("Y_coord", "X_coord"), crs = 3301)

cattleMunicip = st_join(st_transform(cattlegdf, 3301), st_transform(municip, 3301), join = st_intersects)

#Ungrouping
cattleMunicip = cattleMunicip %>% 
  st_drop_geometry()

cattleFinal = cattleMunicip %>% 
  group_by(OKOOD, key) %>% 
  summarise(sum = sum(value)) %>% 
  ungroup()

cattleFinal <- cattleFinal %>% 
  spread(key, sum)

glimpse(cattleFinal)

cattleFinal <- left_join(municip, cattleFinal, by="OKOOD")



cattleFinal$area = st_area(cattleFinal)

cattleFinal = cattleFinal %>% 
  mutate(area = as.numeric(area) / 1000000) # m2 => km2


# showing distribution per square km

ggplot()+
  geom_sf(data=cattleFinal, aes(fill = pigs / area), size=0.2, colour = "grey70")+
  scale_fill_gradientn(colours =c("yellow","blue", "magenta","green"), na.value = "gray")+
  north(cattleFinal, location = "topleft", scale = 0.15, symbol = 12)+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_y = unit(0, "cm"),
    pad_x=unit(1,"cm"))+
  labs(fill = "N per square km", 
       title = " Density of pigs in Estonian municipalities",
       caption = "Author: Bakhtiyar Garashov")+
  ggsave("Pigs_per_km2.png")





#showing absolute numbers for each muncip

ggplot()+
  geom_sf(data=cattleFinal, aes(fill = pigs), size=0.2, colour = "grey70")+
  scale_fill_gradientn(colours =c("yellow","blue", "magenta","green"), na.value = "gray")+
  geom_sf_text(data = cattleFinal, aes(label=pigs),check_overlap = TRUE,fun.geometry = sf::st_centroid)+
  north(cattleFinal, location = "topleft", scale = 0.15, symbol = 12)+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_y = unit(0, "cm"),
    pad_x=unit(1,"cm"))+
  labs(fill = "Absolute values", 
       title = "Number of pigs per muncipality of Estonia",
       subtitle = "Highest number observed in Viljandi",
       caption = "Author:Bakhtiyar Garashov")+
  ggsave("Pigs_absolute_numbers.png")



