library(tidyverse)
library(MigClim)
  #MigClim.userGuide() will open up the manual
  #MigClim no longer on CRAN, so you have to manually download the zip and install from the zip file
  #e.g. install.packages("package_name.tar.gz")
 
library(dismo)
library(maptools)
library(ggthemes)


predict_ini <- reclassify(predict, c(0, 0.25, 0,
                                      0.25, 1, 1)) 
#rescale for hs map
predict_migclim <- predict * 1000

#convert to lat/long to make compliant with strict ascii format. using original bioclim for crs
predict_ini <- projectRaster(predict_ini, predictors$wc2.1_2.5m_bio_1)
predict_migclim <- projectRaster(predict_migclim, predictors$wc2.1_2.5m_bio_1)

#export the two files
writeRaster(predict_ini, filename='ABMA_ini', format="GTiff", overwrite = TRUE)
writeRaster(predict_migclim, filename='hs_map1', format = "GTiff", overwrite = TRUE)

#MIGCLIM. Note need to manually run creating the function from the 1.6 zip file. The 1.6.2 only has updates/changes.
MigClim.migrate (iniDist = "ABMA_ini",
                 hsMap="hs_map",
                 rcThreshold = 250,
                 envChgSteps=1,
                 dispSteps=5,
                 dispKernel=c(.1),
                 iniMatAge=1, propaguleProd=c(1),
                 lddFreq=0.05, lddMinDist=3, lddMaxDist=4,
                 simulName="ABMA_test", replicateNb=1,
                 overWrite=TRUE,
                 testMode=FALSE, 
                 fullOutput=FALSE, keepTempFiles=TRUE)

#read in the MigClim raster that was created
migclim_output <- raster('ABMA_test/ABMA_test_raster.asc')

#here are the value ranges from the output
#0 [no color] Cells that have never been occupied and are unsuitable habitat at the end of the simulation. There are several reasons why a cell can remain non-colonized
#1 (black) Cells that belong to the species' initial distribution and that have remained occupied during the entire simulation.
#1 < value < 30000  #Positive values greater than 1 but smaller then 30000 represent cells that have been colonized during the simulation and that remain occupied at the end of the simulation. The value of the cell allows to determine the dispersal step during which it was colonized using the following code: each environmental change step  given a value of 100 and each dispersal step a value of 1. Here are some examples 101 = 1st dispersal step of 1st environmental change step (1 × 1 + 1 × 100 = 101). 102 = 2nd dispersal step of 1st environmental change step (2 × 1 + 1 × 100 = 102). 504 = 4th dispersal step of 5th environmental change step (4 × 1 + 5 × 100 = 504). 1003 = 3rd dispersal step of 10th environmental change step (3 × 1 + 10 × 100 = 101).
#30,000 [pink] Cells that are potentially suitable (i.e. habitat is favorable) but that were not colonized due to dispersal
# Value < 1 [grey] Negative values indicate cells that were once occupied but have become decolonized, because their habitat has turned unsuitable. Their absolute value allows determining the exact time when they have been decolonized using the same code as explained just above.


  
#I don't want to see each individual time steps colonization and instead just want to summarize, so I will reclassify to simply the values from above, and the plot and name accordingly
migclim_plot <- reclassify(migclim_output, c(-29999, -0.00001, 1, #lost initial
                                             -0.00001,0.00001,2, #never suitable
                                             0.000011, 1.0001, 3, #maintained initial
                                            1.0002, 29999, 4, #suitable, colonized
                                            29999.01,30005,5)) #suitable, vacant
#next, crop to the area of interest
ext <- extent(-135, -110, 30, 60)
migclim_plot <- crop(migclim_plot, ext)

#convert migclim_output to dataframe for ggplot. converting original as well for factor check
migclim_plot_df <- as.data.frame(migclim_plot, xy = TRUE)

#note that need to specify it is a factor
migclim_plot_df$migclim <- as.factor(migclim_plot_df$ABMA_test_raster)

#going to load in country outline and then plot
dmap <- maps::map("world", regions=c('USA', 'Canada'), col="transparent", plot=FALSE, fill = TRUE)

area_poly <- map2SpatialPolygons(dmap, IDs = dmap$names, proj4string=CRS("+proj=longlat +datum=WGS84"))

#set map extent and crop out country bits you don't want

area_poly_map <- crop(area_poly, ext)

migclim_map <- ggplot() +
  geom_tile(data=migclim_plot_df, aes(x=x, y=y, fill=migclim)) +
  scale_fill_manual(values = c("orange", "white", "darkgrey", "darkblue", "green"), labels = c('Lost Initial', 'Never Suitable', 'Maintained Initial', 'Suitable, Colonized', 'Suitable, Vacant'))+ 
  geom_polygon(data = area_poly_map, mapping = aes(x = long, y = lat, group = group), color = "black", fill = NA) + #black lines for the states
  coord_fixed(1.3) + 
  ggtitle("Ambystoma macrodactylum") + 
  theme_map(base_size = 16) + 
  theme(legend.position="bottom") + 
  theme(plot.title = element_text(face = "italic")) + 
  theme(legend.key.width=unit(1, "cm")) +
  theme(legend.title=element_blank())

ggsave("migclim_map.png", plot = migclim_map, width = 12, height = 12, units="in", dpi = 300)
