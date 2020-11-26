install.packages("tidyverse")
install.packages("sf")
install.packages("maptools")
install.packages("spatstat")
install.packages("tmap")
install.packages("ggthemes")
install.packages("RColorBrewer")
install.packages("GISTools")
install.packages("tmap")
install.packages("plotly")
install.packages('ggsn')
install.packages("ggspatial")


library(tidyverse) #  collection of R packages designed for data science
library(sf) # spatial data as simple features
library(maptools) # spatial data
library(spatstat) # spatial statistics
library(tmap) # thematic maps
library(ggthemes) # additional predefined themes for ggplot2 plots
library(ggthemes)
library(RColorBrewer)
library(GISTools)
library(tmap)
library(plotly)
library(ggplot2)
library(ggsn)
library(ggspatial)



#Unzip the Data
unzip("maakond_shp.zip")

#Reading the data and loading assigning it to a dataframe
maakond = st_read("maakond_20201101.shp")


ggplot()+
  theme_minimal()+
  geom_sf(data = maakond, aes(fill = as.factor(MNIMI)), size=0.5)+
  north(maakond, location = "topleft", scale = 0.15, symbol = 12)+
  ggspatial::annotation_scale(
    location = "bl",
    bar_cols = c("black", "white"),
    pad_y = unit(0, "cm"),
    pad_x=unit(1,"cm"))+
  labs(fill = "Muncipalities",caption = "Author: Bakhtiyar Garashov")+
  ggtitle("Estonian muncipalities")

