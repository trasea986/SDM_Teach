#load packages
library(tidyverse)
library(maptools)
library(rgbif)
library(dismo)

#new packages
#install.packages("name") if you do not have them already
library(corrplot)
library(caret)
library(rJava)

#make an object with the projection we will be using, North American Albers Equal Area Conic
#you may want to use a different CRS for your project (e.g. the standard lat/lon is often fine if working with a small area, such as a single state)

projection <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83"

#the extent for our project
#step 1: bring in our point file
points_clean <- read.csv("points_cleaned.csv")

#see the min and max lat/lon
max(points_clean$decimalLongitude)
min(points_clean$decimalLongitude)
max(points_clean$decimalLatitude)
min(points_clean$decimalLatitude)

#set the extent, which I did based on the above output, while adding some buffer to it
ext <- extent(-135, -110, 30, 60)

#one option for data is download in R, but I've commented this out because they don't currently have the most recent climate change models
#climate_present <- getData('worldclim', var = 'bio', res = 2.5)
#climate_future <- getData('CMIP5', var = 'worldclim', res = 2.5, rcp = 8.5, model = 'CC', year = 70)

#so instead, we list all the files that we have unzipped from BioClim. You may need to change the path information to whatever folder has all of the tif files

#this is the 30 second resolution, found under "historical" on their website
#files <- list.files(path = './env_layers_raw/present_day', pattern = '*.tif', all.files = TRUE, full.names = TRUE)
#for 2.5 minute
files <- list.files(path = './env_layers_raw/present_day_25', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

#make sure it worked. you should see a list of files when you call the object "files"
files

#next load all the raster files using stack function
predictors <- stack(files)

#view the raster info for 1 file. calling just the raster allows you to see CRS, resolution, extent, and so on
#note the middle part of the name, which is the resolution. you should make this match what you downloaded
predictors$wc2.1_2.5m_bio_2

#as before, we can plot the raster
#plot(predictors)


#next we will focus on removing correlated variables
#first, extract the predictors at the point files
extracted_vals <- extract(predictors, points_clean[2:3])

#once we have that, we will convert the object extracted_vals to a data frame for the correlation packaged
extracted_df <- as.data.frame(extracted_vals)

#calculate the correlation among our variables at our points
mydata.cor <- cor(extracted_df, method = 'spearman', use = 'complete.obs')

#viewing the plot is a bit messy, but you can see lots of correlation issues
corrplot(mydata.cor)

#so we could manually remove the layers, but we can also automate/iteratively remove. If you want to remove manually by selecting what you know is biologically important, you can do this by subsetting the raster stack (isntead of using names(predictors_final_list below, just feed a list of the rasters you want to keep, using 'subset(predictors, c(raster1, raster2, etc))'

#first set up the correlation value cutoff you want to use to make the list of highly correlated variables
hc <- findCorrelation(mydata.cor, cutoff = 0.8)
#sort the list of highlight correlated variables
hc = sort(hc)
#remove the correlated ones from the data frame. This will create what we feed into subset.
predictors_final_list = extracted_df[,-c(hc)]

#now we have our list, and we will cut out from the raster stack
predictors_final <- subset(predictors, names(predictors_final_list))

#you should now only see the reduced data when plotting
plot(predictors_final)

#next up, cropping. the first argument is the raster you want to crop, and then you simply feed the extent

#this only works if using 30s
bio2_crop <- crop(predictors_final$wc2.1_30s_bio_2, ext)

#check it
plot(bio2_crop)

#now repeat this on the rest of the layers within the raster stack
#bio3_crop <- crop(predictors_final$wc2.1_30s_bio_3, ext)
#bio7_crop <- crop(predictors_final$wc2.1_30s_bio_7, ext)
#bio8_crop <- crop(predictors_final$wc2.1_30s_bio_8, ext)
#bio9_crop <- crop(predictors_final$wc2.1_30s_bio_9, ext)
#bio10_crop <- crop(predictors_final$wc2.1_30s_bio_10, ext)
#bio14_crop <- crop(predictors_final$wc2.1_30s_bio_14, ext)
#bio15_crop <- crop(predictors_final$wc2.1_30s_bio_15, ext)
#bio19_crop <- crop(predictors_final$wc2.1_30s_bio_19, ext)

#if using coarse inputs
bio2_crop <- crop(predictors_final$wc2.1_2.5m_bio_2, ext)
bio3_crop <- crop(predictors_final$wc2.1_2.5m_bio_3, ext)
bio7_crop <- crop(predictors_final$wc2.1_2.5m_bio_7, ext)
bio8_crop <- crop(predictors_final$wc2.1_2.5m_bio_8, ext)
bio9_crop <- crop(predictors_final$wc2.1_2.5m_bio_9, ext)
bio10_crop <- crop(predictors_final$wc2.1_2.5m_bio_10, ext)
bio14_crop <- crop(predictors_final$wc2.1_2.5m_bio_14, ext)
bio15_crop <- crop(predictors_final$wc2.1_2.5m_bio_15, ext)
bio19_crop <- crop(predictors_final$wc2.1_2.5m_bio_19, ext)

#next we want to reproject. the arguments are the cropped raster from above, and then the crs we defined through the object we made at the start of this script
bio2_final <- projectRaster(bio2_crop, crs = projection)
bio3_final <- projectRaster(bio3_crop, crs=projection)
bio7_final <- projectRaster(bio7_crop, crs=projection)
bio8_final <- projectRaster(bio8_crop, crs=projection)
bio9_final <- projectRaster(bio9_crop, crs=projection)
bio10_final <- projectRaster(bio10_crop, crs=projection)
bio14_final <- projectRaster(bio14_crop, crs=projection)
bio15_final <- projectRaster(bio15_crop, crs=projection)
bio19_final <- projectRaster(bio19_crop, crs=projection)


#once done croping and reporjecting, we stack them all back together
#note that cropping and reprojecting can be done on a stack, but you often run into memory issues

#if not wanting the reprojected layers
#predictors_maxent <- stack(bio2_crop,
#                           bio3_crop,
#                           bio7_crop,
#                           bio8_crop,
#                           bio9_crop,
#                           bio10_crop,
#                           bio14_crop,
#                           bio15_crop,
#                           bio19_crop)

predictors_maxent <- stack(bio2_final,
                           bio3_final,
                           bio7_final,
                           bio8_final,
                           bio9_final,
                           bio10_final,
                           bio14_final,
                           bio15_final,
                           bio19_final)

#one more step we didn't talk about during lecture, which is renaming the rasters. we are going to shorten them up to make it easier to match when we do predictions

predictors_maxent <- setNames(predictors_maxent, c('bio2',
                                                   'bio3',
                                                   'bio7',
                                                   'bio8',
                                                   'bio9',
                                                   'bio10',
                                                   'bio14',
                                                   'bio15',
                                                   'bio19'))
