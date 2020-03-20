library(leaflet)
library(tidyverse)


#coordonnees gps des regions 
data=read.csv(file='co2_temperature_pop_gps.csv',sep=';')
data

#map avec pop up sur chaque region 
#les données doivent 
mymap = leaflet(data)%>% addTiles() %>%
  addMarkers(lng=~lng,
             lat=~lat,
             popup=~region_x,data=data)
mymap

data90 = data %>% filter(annee==1990)
data91 = data %>% filter(annee==1991)
data92 = data %>% filter(annee==1992)
data93 = data %>% filter(annee==1993)
data94 = data %>% filter(annee==1994)
data95 = data %>% filter(annee==1995)
data96 = data %>% filter(annee==1996)
data97 = data %>% filter(annee==1997)
data98 = data %>% filter(annee==1998)
data99 = data %>% filter(annee==1999)
data2000 = data %>% filter(annee==2000)
data2001 = data %>% filter(annee==2001)
data2002 = data %>% filter(annee==2002)
data2003 = data %>% filter(annee==2003)
data2004 = data %>% filter(annee==2004)
data2005 = data %>% filter(annee==2005)
data2006 = data %>% filter(annee==2006)
data2007 = data %>% filter(annee==2007)
data2008 = data %>% filter(annee==2008)
data2009 = data %>% filter(annee==2009)
data2010 = data %>% filter(annee==2010)

couleurs <- colorNumeric("YlOrRd", data90$variation_temperature, n=22)
couleurs
mymap90 = leaflet(data90)%>% addTiles() %>%
  addCircleMarkers(lng = data90$lng, 
                  lat = data90$lat, 
                  fillOpacity = 10,
                  popup = ~paste("Variation de la température pour la région",data90$region_x, "pour l'année",data90$annee, ":", data90$variation_temperature),
                  color= ~couleurs(variation_temperature)) %>% 
  addLegend(pal = couleurs, values = ~variation_temperature, opacity = 0.9)
mymap90



couleurs <- colorNumeric("YlOrRd", data91$variation_temperature, n=22)
couleurs
mymap91 = leaflet(data91)%>% addTiles() %>%
  addCircleMarkers (lng = data91$lng, 
                   lat = data91$lat, 
                   fillOpacity = 10,
                   popup = ~paste("Variation de la température pour la région",data91$region_x, "pour l'année",data91$annee, ":", data91$variation_temperature),
                   color= ~couleurs(variation_temperature)) %>% 
  addLegend(pal = couleurs, values = ~variation_temperature, opacity = 0.9)
mymap91


