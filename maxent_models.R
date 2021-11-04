#after running env_data_prep.R

#this piece from the homework assignment, that was not in the env_data_prep.R file


# Future data prep --------------------------------------------------------

future <- stack("./env_layers_raw/share/spatial03/worldclim/cmip6/7_fut/2.5m/CanESM5/ssp585/wc2.1_2.5m_bioc_CanESM5_ssp585_2081-2100.tif")

ext <- extent(-140, -100, 25, 90)

bio2_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.2, ext)
bio3_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.3, ext)
bio7_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.7, ext)
bio8_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.8, ext)
bio9_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.9, ext)
bio10_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.10, ext)
bio14_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.14, ext)
bio15_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.15, ext)
bio19_crop_future <- crop(future$wc2.1_2.5m_bioc_CanESM5_ssp585_2081.2100.19, ext)

bio2_final_future <- projectRaster(bio2_crop_future, crs=projection)
bio3_final_future <- projectRaster(bio3_crop_future, crs=projection)
bio7_final_future <- projectRaster(bio7_crop_future, crs=projection)
bio8_final_future <- projectRaster(bio8_crop_future, crs=projection)
bio9_final_future <- projectRaster(bio9_crop_future, crs=projection)
bio10_final_future <- projectRaster(bio10_crop_future, crs=projection)
bio14_final_future <- projectRaster(bio14_crop_future, crs=projection)
bio15_final_future <- projectRaster(bio15_crop_future, crs=projection)
bio19_final_future <- projectRaster(bio19_crop_future, crs=projection)

#if you are not using reprojected rasters
#predictors_maxent_future <- stack(bio2_crop_future,
#                                  bio3_crop_future,
#                                  bio7_crop_future,
#                                  bio8_crop_future,
#                                  bio9_crop_future,
#                                  bio10_crop_future,
#                                  bio14_crop_future,
#                                  bio15_crop_future,
#                                  bio19_crop_future)

predictors_maxent_future <- stack(bio2_final_future,
                                  bio3_final_future,
                                  bio7_final_future,
                                  bio8_final_future,
                                  bio9_final_future,
                                  bio10_final_future,
                                  bio14_final_future,
                                  bio15_final_future,
                                  bio19_final_future)

predictors_maxent_future <- setNames(predictors_maxent_future, c('bio2',
                                                                 'bio3',
                                                                 'bio7',
                                                                 'bio8',
                                                                 'bio9',
                                                                 'bio10',
                                                                 'bio14',
                                                                 'bio15',
                                                                 'bio19'))



# Final point prep --------------------------------------------------------

#now we have all of our environmental variables, need to reproject our point file

points_repro <- points_clean
coordinates(points_repro) <- ~decimalLongitude+decimalLatitude
projection(points_repro) <- CRS('+proj=longlat +datum=WGS84')
points_final <- spTransform(points_repro, crs(bio2_final))

#make sure the reprojection works
plot(predictors_maxent$bio2)
points(points_final, pch = 8, col = "black", cex = 1)

plot(predictors_maxent_future$bio2)
points(points_final, pch = 8, col = "black", cex = 1)



# Maxent ------------------------------------------------------------------
###if you don't want to run Maxent in R, will need to export the point file and environmental files. Note that the points_final is  SpatialPointsDataFrame, so we need to turn it into just a dataframe.
points_final_df <- as.data.frame(points_final)

#maxent is expecting the first column to be the species name, not the row numbers.
#This replaces everything in the column with the text string
points_final_df$X <- "Ambystoma macrodactylum"

write.csv(points_final_df, "./inputs_final/ABMA_points.csv", row.names = FALSE)

#then export the rasters
writeRaster(predictors_maxent, filename=paste('./inputs_final/present_day/',names(predictors_maxent)), bylayer=TRUE,format="raster")

writeRaster(predictors_maxent_future, filename=paste('./inputs_final/future/cmip6_2_5_m_CanESM5_ssp585_2081-2100/',names(predictors_maxent_future)), bylayer=TRUE,format="raster")



#and now we can run in R. again, above writing is if only running maxent outside of R

model <-maxent(x=predictors_maxent, p=points_final, args=c(
  'maximumbackground=10000',
  'defaultprevalence=1.00',
  'betamultiplier=0.5',
  'plots=true',
  'pictures=true',
  'randomtestpoints=30',
  'linear=true',
  'quadratic=true',
  'product=true',
  'threshold=false',
  'hinge=false',
  'threads=4',
  'responsecurves=true',
  'jackknife=true',
  'askoverwrite=false'
))



#view the html file
model

#variable importance
plot(model)

#response curves
response(model)

#now to predict across the landscape
predict <- predict(predictors_maxent, model, progress='text')

#view map
plot(predict)

#predict using the future variables
predict_future <- predict(predictors_maxent_future, model, progress='text')

plot(predict_future)


#exporting the prediction files (ascii) from R, if you want to drop into ArcGIS
writeRaster(predict, filename=paste('./outputs/cmip6_2_5_m_CanESM5_ssp585_2081-2100/present_map_R'), format="raster")

writeRaster(predict_future, filename=paste('./outputs/cmip6_2_5_m_CanESM5_ssp585_2081-2100/2081_map_R'), format="raster")



#if you want to load in the ascii you created using the Maxent gui, you can use the following. Note that in maxent I selected to output the file as a grd file.
maxent_output_present <- raster("./outputs/cmip6_2_5_m_CanESM5_ssp585_2081-2100/Ambystoma_macrodactylum.asc")
#then define CRS, note the lower case here
crs(maxent_output_present) <- projection
plot(maxent_output_present)

#the maps look fine on their own, but there appears to be some projection issues when running maxent outside and then using the point file we have here that has been reprojected. For your own work, I think I would not use reprojected layers with Maxent outside of R (keep everything in lat/lon), and export the raster files as ASCII (set the format argument to "asc"). This is a new issue since the last time I ran Maxent models.
