library(leaflet)
library(tidyverse)

#coordonnees gps des regions 
gps=read.csv(file='region_gps.csv',sep=';')
gps

#map avec pop up sur chaque region 
#les données doivent 
mymap = leaflet(gps)%>% addTiles() %>%
  addMarkers(lng=~longitude,
             lat=~latitude,
             popup=~Pays,data=gps)
mymap