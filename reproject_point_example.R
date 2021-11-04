#example script for converting UTM to lat/lon, based on line 54-61 in maxent_models.R script.
library(tidyverse)
library(maptools)
library(rgbif)
library(dismo)
library(rgdal)

#load in and crop an environmental layer to make sure the projections align when you are done
ext <- extent(-135, -100, 15, 60)
files <- list.files(path = './env_layers_raw/present_day', pattern = '*.tif', all.files = TRUE, full.names = TRUE)
predictors <- stack(files)
bio2_crop <- crop(predictors$wc2.1_30s_bio_2, ext)

#bring in the csv
points <- read.csv("PonderosaPine_FieldData.csv")

#set the coordinates for the point file
coordinates(points) <- ~Easting+Northing
#8826 is the Idaho Transverse Mercator
#definte CRS
projection(points) <- CRS('+init=epsg:8826')

#reproject to the CRS of your environmental layer
points_final <- spTransform(points, crs(bio2_crop))

#plot the environmental layer and the points together to make sure alignment looks okay
plot(bio2_crop)
points(points_final, pch = 8, col = "black", cex = 1)


#pulling from line 72 of maxent_models. R, we then turn that point file to a data frame and export. Note that the points_final is  SpatialPointsDataFrame, so we need to turn it into just a dataframe.
points_final_df <- as.data.frame(points_final)

#maxent is expecting the first column to be the species name, not the row numbers.
#This replaces everything in the column with the text string
points_export <- points_final_df %>%
  dplyr::select(Species.Code, Easting, Northing)

#this would write to a folder called "inputs_final" in your current working directory. If you don't have/don't want that folder, you can change the code below to simply write "Pine_points.csv" and have it output in your current working directory.
write.csv(points_export, "./inputs_final/Pine_points.csv", row.names = FALSE)
