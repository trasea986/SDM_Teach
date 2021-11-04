library(tidyverse)
library(maptools)
library(rgbif)
library(dismo)

#new packages
library(corrplot)
library(caret)
library(rJava)


#establish what projection you will be using throughout, North American Albers Equal Area:

projection <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"


#set extent. pull in the values based on what you are seeing from your point file.
#you can do this by looking at your points on the previous assignment

#first we are going to load in our points from the end of the last assignment:

points_clean <- read.csv("points_cleaned.csv")

max(points_clean$decimalLongitude)
min(points_clean$decimalLongitude)
max(points_clean$decimalLatitude)
min(points_clean$decimalLatitude)

#So we will set this based on above values with some wiggle room
ext <- extent(-135, -110, 30, 60)

#one option is to download directly in R, although this doesn't have CMIP6, only 5

#climate <- getData('worldclim', var='bio', res=2.5)
#future <- getData('CMIP5', var='worldclim', res=2.5, rcp=85, model='CC', year=70)


#so instead, I'm going to load in all of our environment data

files <- list.files(path = "./env_layers_raw/present_day", pattern='*.tif', all.files=TRUE, full.names=TRUE)

#check to make sure that you have everything point to the right place
files

#next we will load these in.

predictors <- stack(files)

#if you want to view information about current crs, extent, and resoultion:
predictors$wc2.1_30s_bio_1

#this is a raster stack, which is all of the rasters lumped together. this can be useful when trying to do something to all of them at once. note that for stacks, generally they all need to be same resolution, extent, and so on.

plot(predictors)

#we can view with plot to see all of them. Notice that some of these are likely correlated. We don't want predictors which are correlated to both be going in the model

#we are going to extract the predictors for our data points. note that we only want to use the 2 and 3 columns here.

extracted_vals <- extract(predictors, points_clean[2:3])

#now we can create a correlation matrix. we first load the package, then use the cor function. The cor function needs a data frame, so we are going change extracted_vals to a data frame.

extracted_df <- as.data.frame(extracted_vals)

#this cor function can work on any data frame. we are going to use spearman rank correlation and only calculate it if we have all of the variables available at a point using the use argument.

mydata.cor = cor(extracted_df, method = c("spearman"), use = "complete.obs")

corrplot(mydata.cor)

#so what do we do with correlated predictors? select those which are potentially more biologically significant or remove those that are correlated with many variables.

#one option is to use a package to iteratively remove for you

hc <- findCorrelation(mydata.cor, cutoff=0.8)
hc = sort(hc)
predictors_final_list = extracted_df[,-c(hc)]

#we can also test and see if this looks better, and even view the correlation coefficient

mydata.cor2 = cor(predictors_final_list, method = c("spearman"), use = "complete.obs")

corrplot(mydata.cor2, addCoef.col = "grey")

#so now we want to cut down the raster stack to only those from this final list

predictors_final <- subset(predictors, names(predictors_final_list))
plot(predictors_final)

  
#now we need to reproject and trim

#this can be pretty intensive on your computer if you try to do the whole stack.

#always crop before reprojecting

bio2_crop <- crop(predictors_final$wc2.1_30s_bio_2, ext)
bio3_crop <- crop(predictors_final$wc2.1_30s_bio_3, ext)
bio7_crop <- crop(predictors_final$wc2.1_30s_bio_7, ext)
bio8_crop <- crop(predictors_final$wc2.1_30s_bio_8, ext)
bio9_crop <- crop(predictors_final$wc2.1_30s_bio_9, ext)
bio10_crop <- crop(predictors_final$wc2.1_30s_bio_10, ext)
bio14_crop <- crop(predictors_final$wc2.1_30s_bio_14, ext)
bio15_crop <- crop(predictors_final$wc2.1_30s_bio_15, ext)
bio19_crop <- crop(predictors_final$wc2.1_30s_bio_19, ext)

#now to reproject each of those

bio2_final <- projectRaster(bio2_crop, crs=projection)
bio3_final <- projectRaster(bio3_crop, crs=projection)
bio7_final <- projectRaster(bio7_crop, crs=projection)
bio8_final <- projectRaster(bio8_crop, crs=projection)
bio9_final <- projectRaster(bio9_crop, crs=projection)
bio10_final <- projectRaster(bio10_crop, crs=projection)
bio14_final <- projectRaster(bio14_crop, crs=projection)
bio15_final <- projectRaster(bio15_crop, crs=projection)
bio19_final <- projectRaster(bio19_crop, crs=projection)


# next we stack them back together

predictors_maxent <- stack(bio2_final,
                           bio3_final,
                           bio7_final,
                           bio8_final,
                           bio9_final,
                           bio10_final,
                           bio14_final,
                           bio15_final,
                           bio19_final)
  
predictors_maxent <- setNames(predictors_maxent, c('bio2',
                                                   'bio3',
                                                   'bio7',
                                                   'bio8',
                                                   'bio9',
                                                   'bio10',
                                                   'bio14',
                                                   'bio15',
                                                   'bio19'))


#now to reproject the point final points and bring in/repro our bg points
points_repro <- points_clean
coordinates(points_repro) <- ~decimalLongitude+decimalLatitude
projection(points_repro) <- CRS('+proj=longlat +datum=WGS84')
points_final <- spTransform(points_repro, crs(bio2_final))

plot(predictors_maxent$bio2)
points(points_final, pch = 8, col = "black", cex = 1)


 
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



#add projection layers to above to point to a different folder
#projectionlayers =

plot(model)

response(model)

predict <- predict(predictors_maxent, model, progress='text')

plot(predict)



future <- stack("D:/OneDrive/GEM3_PostDoc/Teaching/ENV404_504_Climate_Change/ENVS_404_504/env_layers_raw/share/spatial03/worldclim/cmip6/7_fut/2.5m/CanESM5/ssp585/wc2.1_2.5m_bioc_CanESM5_ssp585_2081-2100.tif")

names(future)

#expand area a bit
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

#now to reproject each of those

bio2_final_future <- projectRaster(bio2_crop_future, crs=projection)
bio3_final_future <- projectRaster(bio3_crop_future, crs=projection)
bio7_final_future <- projectRaster(bio7_crop_future, crs=projection)
bio8_final_future <- projectRaster(bio8_crop_future, crs=projection)
bio9_final_future <- projectRaster(bio9_crop_future, crs=projection)
bio10_final_future <- projectRaster(bio10_crop_future, crs=projection)
bio14_final_future <- projectRaster(bio14_crop_future, crs=projection)
bio15_final_future <- projectRaster(bio15_crop_future, crs=projection)
bio19_final_future <- projectRaster(bio19_crop_future, crs=projection)


# next we stack them back together

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


predict_future <- predict(predictors_maxent_future, model, progress='text')

plot(predict)
plot(predict_future)
