library(sp)
library(raster)
library(rgdal)
library(RStoolbox)
library(tmap)
library(sf)
library(rgeos)
library(tidyverse)
library(ggthemes)
library(scico)
library(ggmap)
library(data.table)
library(knitr)
library(tibble)
library(dplyr)


#crime data 
crime_data = fread("https://opendata.smit.ee/ppa/csv/avalik_1.csv", encoding = "UTF-8")



crime_names = colnames(crime_data)

crime_names_en = c("CaseId", 
                   "Date", 
                   "Time", 
                   "Weekday", 
                   "CaseType", 
                   "CaseTypeAdditional", 
                   "Law", 
                   "Paragraph", 
                   "ParagraphFull", 
                   "Section", 
                   "DamagesEuro", 
                   "PlaceType", 
                   "County", 
                   "Municipality", 
                   "Place", 
                   "Lest_X", 
                   "Lest_Y", 
                   "Type")

data_frame("Columns, et" = crime_names, "Columns, en" = crime_names_en) %>% 
  kable()


colnames(crime_data) = crime_names_en


TOP_caseType <- crime_data %>% 
  group_by(CaseType) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n))

TOP_caseType %>% 
  head()


Sys.setlocale("LC_TIME", "English")

crime_data <- crime_data %>% 
  mutate(Date = ymd(Date), 
         wday = lubridate::wday(Date, abbr = T, label =T), 
         hour = as.integer(substr(Time, 1, 2)))


Sys.setlocale("LC_TIME","Estonian")



crime_data <- crime_data %>% 
  mutate(Lest_X_bu = Lest_X, Lest_Y_bu = Lest_Y) 


crime_data <- crime_data %>% 
  separate(Lest_X, c("x_min", "x_max"), sep = "-") 

crime_data <- crime_data %>% 
  separate(Lest_Y, c("y_min", "y_max"), sep = "-")

crime_data <- crime_data %>% 
  mutate(x_min = as.integer(x_min),
         x_max = as.integer(x_max),
         y_min = as.integer(y_min),
         y_max = as.integer(y_max))


crime_data <- crime_data %>% 
  mutate(x=(x_min + x_max) / 2, y=(y_min + y_max) / 2)

#counties shp
download.file("https://geoportaal.maaamet.ee/docs/haldus_asustus/maakond_shp.zip", destfile="maakond_shp.zip")

unzip("maakond_shp.zip")

counties = st_read("maakond_20201201.shp")

county <- st_simplify(counties, preserveTopology = T, dTolerance = 200) # ?st_simplify
object.size(county) 



crime_data <- crime_data %>% 
  rename(tmp = y) %>% 
  rename(y = x) %>% 
  rename(x = tmp)


crime_data_grd_aggr <-  crime_data %>% 
  group_by(x, y) %>% 
  summarise(n = n()) %>% 
  ungroup()

crime_data_grd_aggr1 = crime_data_grd_aggr[complete.cases(crime_data_grd_aggr), ]

crime_data_grd_aggr_sf <- st_as_sf(crime_data_grd_aggr1, coords = c("x", "y"), crs = 3301)

unzip("asustusyksus_shp.zip")

eesti <- st_read("asustusyksus_20201201.shp")

tallin_sf <- eesti %>% 
  filter(OKOOD == "0784")


a=st_transform(crime_data_grd_aggr_sf,3301)
b=st_transform(tallin_sf,3301)

crime_data_grd_aggr_sf_tll <- st_intersection(a,b)

# map#1
ggplot()+
  theme_minimal()+
  ggtitle("Crimes density in Districts of Tallin")+
  theme(legend.position = "right")+
  geom_sf(data = tallin_sf, fill="black")+
  geom_sf(data = crime_data_grd_aggr_sf_tll, aes(colour = n, size=n), shape=15)+
  scale_colour_gradientn(colours = c("darkgreen", "grey95", "orange"))


crimes_tallin <- crime_data %>% 
  filter(Municipality == "Tallinn")

crimes_tallin1 = crimes_tallin[complete.cases(crimes_tallin), ]


crime_data_tallin_sf <- st_as_sf(crimes_tallin1, coords = c("x", "y"), crs = 3301)



c=st_transform(tallin_sf,3301)
d=st_transform(crime_data_tallin_sf,3301)

tallin_sf$pt_count <- lengths(st_intersects(c,d))

tmap_mode("view")

tm_shape(tallin_sf)+
  tm_polygons(col = "firebrick", border.col = "grey30", lwd = 0.2, alpha = 0.1)+
  tm_text("pt_count",auto.placement = TRUE)


