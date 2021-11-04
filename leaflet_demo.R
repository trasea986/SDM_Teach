library(leaflet)
library(tidyverse)
library(leafem)
library(mapview)
library(htmlwidgets)

#first, lets pick the center of the map by figuring out average lat/long

mean_long <- mean(points_clean$decimalLongitude)
mean_lat <- mean(points_clean$decimalLatitude)

#we will use the calculated +/- change map from the make_maps script.

#we then have to define the color pallete for leaflet. feel free to play with this colorNumeric function.
pal <- colorNumeric(
  palette = c("blue", "red"),
  domain = predict_change@data@values,
  na.color = "#00000000"
)

#first line adds the base map
#second line adds our raster and sets colors
#third line gives us the gps coordinates depending on where our mouse is
#next chunk of lines adds the legend
#next adds the initial view and zoom of the map
#last add minimap adds an inset whe you are zooming around

m <- leaflet() %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addRasterImage(predict_change, group = "ENM", layerId = "values", opacity = 0.5, colors = pal) %>% 
  addMouseCoordinates() %>%
  addLegend("bottomright", pal = pal, values = predict_change@data@values,
          title = "ENM Change",
          opacity = 1) %>%
  setView(lat = mean_lat, lng = mean_long, zoom = 6) %>%
  addMiniMap(
    toggleDisplay = TRUE,
    tiles = providers$Stamen.TonerLite
  )


saveWidget(m, file="map.html")
