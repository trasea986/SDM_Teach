#this is run after the maxent_models.R script, using Maxent in R. At this point you should have two predicted raster files, predict and predict_future

#we are going to load cowplot, mapdata and ggthemes, some other packages. you will need to install.packages if you don't have these already

#we are going to take our predictions, and then map just the comparison in WA/ID/OR for A. macrodactylum. You might want to map something different and want to change some of the base layers or info below. Let me know if you need help customizing.

#what I am showing is mostly modified from the following two websites:
# https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
# https://geocompr.robinlovelace.net/adv-map.html
# https://ggplot2-book.org/maps.html

library(ggmap)
library(maps)
library(mapdata)
library(cowplot)
library(ggthemes)
library(sf)
library(RColorBrewer)
library(maptools)
library(FRK)

#plotting rasters with ggplot can take a long time, so it is important to get everything else on the maps working correctly before adding that in

#we are going to download some shapefiles to outline the country

#instead we are going to just focus on North America, specifically Washington, Oregon, and Idaho for the long-toed salamander demonstration we have worked with.


#I'm going to shift back to the original projection for these "zoomed" in maps
#This is a very slow process, so maybe another reason to leave every just in lat/lon from the start

predict_repro <- projectRaster(predict, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")



#load in the states and counties for mapping, here I am focusing on three states

states_plot <- c("washington", "oregon", "idaho")

dmap <- map("state", regions=states_plot, col="transparent", plot=FALSE, fill = TRUE)

area_poly <- map2SpatialPolygons(dmap, IDs=dmap$names, , proj4string=CRS("+proj=longlat +datum=WGS84"))

counties <- map_data("county")

county_sub <- subset(counties, region %in% c("idaho", "oregon", "washington"))

#crop the extent, and then use mask to cut out all the holes
r2 <- crop(predict_repro, extent(area_poly))
predict_map <- mask(r2, area_poly)

## Check that it worked
plot(predict_map)

#convert this new cropped map to df for ggplot
predict_map_df <- as.data.frame(predict_map, xy=TRUE)


map <- ggplot() +
  geom_tile(data=predict_map_df, aes(x=x, y=y, fill=layer), alpha=0.8) + #plot the ENM
  scale_fill_gradient2("ENM Value",
                       low = 'aquamarine', high = 'aquamarine4',
                       na.value = NA) + #use a gradient from light to dark (4) colors
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + #darkgrey county lines
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  coord_fixed(1.3) + #establish coordinates
  ggtitle("Ambystoma macrodactylum, 2020") + #title of the plot
  theme_map(base_size = 16) + #base font size
  theme(legend.position="bottom") + #where to put the legend
  theme(plot.title = element_text(face = "italic")) + #use italics for the ggtitle
  theme(legend.key.width=unit(1, "cm")) #how big the legend key should be


#now pull in the future prediction
predict_repro_future <- projectRaster(predict_future, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
r2_future <- crop(predict_repro_future, extent(area_poly))
predict_map_future <- mask(r2_future, area_poly)
predict_map_df_future <- as.data.frame(predict_map_future, xy=TRUE)

map_future <- ggplot() +
  geom_tile(data=predict_map_df_future, aes(x=x, y=y, fill=layer), alpha=0.8) +
  scale_fill_gradient2("ENM Value",
                       low = 'aquamarine', high = 'aquamarine4',
                       na.value = NA) + 
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + 
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
  coord_fixed(1.3) + 
  ggtitle("Ambystoma macrodactylum, 2081") + 
  theme_map(base_size = 16) + 
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(legend.key.width=unit(1, "cm")) 


#combine the maps into one row, by setting up two columns

maps_combo <- plot_grid(
  map, map_future,
  labels = NULL, ncol = 2
)

#save the map
ggsave("AM_ENM_final.png", plot = maps_combo, width = 12, height = 9, units="in", dpi = 900)  




#you might not want the reprojected your raster, so here is what that would look like:

#reproject back
#instead of text string, you can also use the projection object if you want
predict_map_repro <- projectRaster(predict_map, crs="+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83")
#turn to dataframe
predict_map_repro_df <- as.data.frame(predict_map_repro, xy=TRUE)


#then repro state lines to match the prediction raster
area_poly_repro <- spTransform(area_poly, crs(predict_map_repro))

#next we will do the same for county. note that this is a dataframe already, so we have some extra steps
county_sub_repro <- df_to_SpatialPolygons(county_sub, "group", c("long","lat"), crs(area_poly))
#now convert to match the new area_poly_repro
county_sub_repro <- spTransform(county_sub_repro, crs(predict_map_repro))

#make the plot
map3 <- ggplot() +
  geom_tile(data=predict_map_repro_df, aes(x=x, y=y, fill=layer), alpha=0.8) + #plot the ENM
  scale_fill_gradient2("ENM Value",
                       low = 'aquamarine', high = 'aquamarine4',
                       na.value = NA) + #use a gradient from light to dark (4) colors
  geom_polygon(data = county_sub_repro, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + 
  geom_polygon(data = area_poly_repro, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  coord_fixed(1.3) + #establish coordinates
  ggtitle("Ambystoma macrodactylum, 2020") + #title of the plot
  theme_map(base_size = 16) + #base font size
  theme(legend.position="bottom") + #where to put the legend
  theme(plot.title = element_text(face = "italic")) + #use italics for the ggtitle
  theme(legend.key.width=unit(1, "cm")) #how big the legend key should be









#one thing you might be interested in is making maps of the amount of change, instead of side by side maps

#we can do this by using the built in raster math capabilities of the packages we have loaded

#by doing future - present, areas that are positive are predicted to become more suitable, and negative would be areas less suitable

#to subtract these out we need them to be the same exent. So, you can either rerun the maxent model with the same extent for both the present and future, or you can change the extent by adding cells that are missing data (NA).we did crop above, but cropping with different resolutions will give slightly different extents

#we are using the rasters here and not the dataframes from plotting

#if you use the same extent and resolution while making your input environmental variables, you can skip the extend and resample steps

predict_map_extend <- extend(predict_map, predict_map_future, value=NA)

#next we need the same resolution, so we are going to make our present day map have a larger resolution (so larger cell sizes) to match the predicted raster

predict_map_extend <- resample(predict_map_extend, predict_map_future, method='bilinear')

predict_change = predict_map_future - predict_map_extend

#turn to a data frame and then plot

predict_change_map_df <- as.data.frame(predict_change, xy=TRUE)

map_change <- ggplot() +
  geom_tile(data=predict_change_map_df, aes(x=x, y=y, fill=layer), alpha=0.8) +
  scale_fill_gradient2("ENM Value Change, 2080-2010",
                       low = 'red', high = 'blue',
                       na.value = NA) + 
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + 
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
  coord_fixed(1.3) + 
  ggtitle("Ambystoma macrodactylum") + 
  theme_map(base_size = 16) + 
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(legend.key.width=unit(1, "cm")) 

#view
map_change

#looks pretty good, but what if we only want to focus on areas with large amounts of change? we can do this by visualizing the colors as categories instead of doing a continuous variable

#basically you feed R the value range (2 numbers) and the new number for the reclassification. we do this three times (loss, same, expansion).

#if you want different thresholds than -1 to -0.25, feel free to change the numbers around. You just don't want to change the 1, 2, and 3 for what the ranges are being reclassified as.

predict_change_discrete <- reclassify(predict_change, c(-1, -0.25, 1,
                                                        -0.25, 0.25, 2,
                                                        0.25, 1, 3))

predict_change_discrete_df <- as.data.frame(predict_change_discrete, xy=TRUE)

#now we need to say that the 1, 2, and 3 from above
predict_change_discrete_df$layer <- as.factor(predict_change_discrete_df$layer)

levels(predict_change_discrete_df$layer) <- c("Loss", "Same", "Gain")

#remove NAs by only pulling in complete cases, and then remove the "Same" areas. We do this so that the legend will only show changes.
predict_change_discrete_df <- predict_change_discrete_df[complete.cases(predict_change_discrete_df), ]

predict_change_discrete_df <- predict_change_discrete_df %>%
  filter(layer != "Same")

#now we rename the 1, 2, 3 to be what we want it to be

#map_change_discrete <- 
map_change_discrete <- ggplot() +
  geom_tile(data=predict_change_discrete_df, aes(x=x, y=y, fill=layer), alpha=0.8) +
  scale_fill_manual(values = c("firebrick", "royalblue3"))+ 
  geom_polygon(data = county_sub, mapping = aes(x = long, y = lat, group = group), fill = NA, color = "darkgrey") + 
  geom_polygon(data = area_poly, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + 
  coord_fixed(1.3) + 
  ggtitle("Ambystoma macrodactylum") + 
  theme_map(base_size = 16) + 
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(legend.key.width=unit(1, "cm")) +
  theme(legend.title=element_blank())

#combine the maps into one row, by setting up two columns

maps_combo_2 <- plot_grid(
  map_change, map_change_discrete,
  labels = NULL, ncol = 2
)

#save the map
ggsave("AM_ENM_final_change.png", plot = maps_combo_2, width = 12, height = 9, units="in", dpi = 900) 
