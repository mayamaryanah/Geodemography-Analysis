
library(tidyverse)
library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(rgeos)
library(dplyr)
library(readxl)
options(scipen=999)

library(readr)
#cluster <- read_csv("D:/BATAS KEKELURAHAN/cluster.csv")
#View(cluster)

library(readr)
data<- read_csv("C:/Users/User/Downloads/data.csv")
View(data)
str(data)
shp <- st_read("D:/BATAS KEKELURAHAN/Batas Kelurahan copy 3sept.shp", stringsAsFactors =FALSE)
str(shp)
head(shp)

glimpse(shp)
st_crs(shp)



data_map <- merge(data, shp, by='ID_KEL')
data_map<-st_as_sf(data_map)
st_crs(data_map)
str(data_map)
glimpse(data_map)
head(data_map)

####
qtm(data_map, fill="cluster")
####
tm_shape(data_map)+tm_fill("cluster")
####
tm_shape(data_map)+
  tm_polygons("cluster", pallette= "Spectral")+
  tm_scale_bar(breaks=c(0,50,100,150,200), size =1, position = c("RIGHT", "BOTTOM"), lwd =3)

####
MyPalette <- c("#f2f0f7", "#cbc9e2", "#9e9ac8", "#6a51a3")
data_map$cluster=as.character(data_map$cluster)
tm_shape(data_map) +
  tm_fill("cluster", id = "KELURAHAN", pallette="MyPalette"
          ) +
 tm_scale_bar(breaks=c(0,50,100,150,200), size =1, position = c("RIGHT", "BOTTOM"), lwd =3)


tmap_mode("view")
tmap_last()

####
library(RColorBrewer)
pal <- brewer.pal(4, "OrRd") 
class(pal)
plot(data_map["cluster"], pal = pal)
####
library(sp)
spplot(data_map, "cluster")

####
library(leaflet)
leaflet(data_map) %>%
  addPolygons(
    stroke = FALSE,
    fillColor=~pal_fun(cluster),
    fillOpacity = 0.8, smoothFactor = 0.5) %>%
  addTiles()

elbow_room1 <- ca_base + 
  geom_polygon(data = data_map, aes(fill = cluster), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

elbow_room1 
