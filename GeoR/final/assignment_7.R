library(XML)
library(rvest)
library(tidyverse)
library(sf)
library(maptools)
library(knitr)
library(ggthemes)
library(spatstat)
library(data.table)
library(dplyr)
library(raster)
library(sp)
library(gplots)
library(lubridate)
library(ggspatial)
library(ggmap)
library(gstat)
library(GISTools)
library(tmap)
library(plotly)
library(ggplot2)
library(ggsn)
library(sqldf)
library(COVID19)

# include world spatial context
data(World)

# As i have technical background and Sql query skills,
# i feel more comfortable using sqldf library to code

#load the dataset from covid library
x <- covid19()
glimpse(data)

# clean the countries where no data (filter countries and save in variable where data is not 0)
gatheringrestrict= sqldf("SELECT * FROM  x WHERE gatherings_restrictions !=0")


gatherrestrict= gatheringrestrict %>% 
  group_by(id) %>%
  filter(date==max(date))

# Output1
map1= merge(World, gatherrestrict, by.x="iso_a3", by.y="id", all.x = TRUE)


tm_shape(map1) +
  tm_polygons(col= "gatherings_restrictions",
              title="Gatherings Restrictions", 
              labels = c('0: No restrictions', 
                         '1: Large gathering res.', 
                         '2: Restrictions for 100-1000 people', 
                         '3: Restrictions for 10-100 people', 
                         '4: Less than 10 people res.',"No data"))+
  tm_layout(
    inner.margins = 0.08,
    main.title = "Gathering restrictions", 
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



gatheringrestrictions= sqldf("SELECT * FROM  x WHERE gatherings_restrictions =4")


# Get number of days where gathering restriction class is 4 which means
# Restrictions on gatherings of less than 10 people. For more info please check dataset doc.
gatherrestrict4count= sqldf("select  *, count() from x where gatherings_restrictions = 4 group by id")

# Output2
map2= merge(World, gatherrestrict4count, by.x="iso_a3", by.y="id", all.x = TRUE)

tm_shape(map2) + 
  tm_polygons(col="count()", palette="YlOrRd", border.col = "white", title="Number of Days gathering" )+
  tm_layout(
    inner.margins = 0.08,
    main.title = "Number of days for restrictions less than 10 people (4th class of dataset)",
    main.title.size = 1,
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
