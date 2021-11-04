#one challenge is stacking rasters from different locations with different extents and so on.
library(tidyverse)
library(rgdal)
library(terra) #Hijmans created to help speed up some functions from the raster package. This uses SpatRaster objects

#so here is an example of how you can bring in multiple layers and get matching crs/resolution/extents for your models.

#the goal is to end up with a RasterStack that has these mixed layers. I'm going to focus on just doing the RasterStack for the present day model.

#start with the template layer you want things to match. in this case I am going to go with the bioclim layers
bio_files <- list.files(path = './present_day_25', pattern = '*.tif', all.files = TRUE, full.names = TRUE)

bio_layers <- rast(bio_files)

#next, I am going to pull out just bio 1 out of the stack

bio1 <- bio_layers$wc2.1_2.5m_bio_1

#next, we are going to set up our extent for later
#note that this is the first new function from terra (ext instead of extent, see help("terra") to see all of the equivalents). 
extent <-  ext(-115, -110, 40, 50)


#next, I am going to pull in a NLCD land cover layer (layer from Emily D.) and an AdaptWest layer (layer provided by Casey)

#for the land cover, I am only bringing in one, so I don't need to do the list.files with a pattern of .img and then stack from the list. instead, we just call the file directly

herb <- rast('./lc/nlcd_2016_annual_herbaceous_shrubland_fractional_component_20200715.img')

#footprint file layer
foot <- rast('./Human_FtPrint/humftprt.asc')

#next we fix by cropping, reprojecting, and fixing the resolution

#you can view the CRS and extent when you look at the raster
bio1 # lat long, no special units or anything
herb # this is in lat long but units are meters
foot #crs is brought in with ascii from the metadata included

#first, we are going to crop bio1 and reproject. if you remember, reprojecting in R is really computationally intensive. So, if we tried to reproject the land use layer likely things would crash. Instead, we will 1) crop the bio layer 2) then reproject bio1 to the land use layer 3) crop the land use layer and resample 4) reproject the land use layer back to our desired layer's crs (the original bioclim layer)

bio1_crop <- crop(bio1, extent)

#this takes a long time to get the cropped bio layer into the CRS of the herb/land cover layer
bio1_herb_crs <- project(bio1_crop, crs(herb))

herb_crop <- crop(herb, bio1_herb_crs)

#next is to resample the cropped layer to get the same resolution.

#there are two methods. ngb is most appropriate for categorical variables

herb_crop_resolution <- terra::resample(herb_crop, bio1_herb_crs, method = 'ngb')

#project back to the original bioclim projection
#make sure to point to the cropped object, or else the extent will mask back to being un-cropped.
herb_final <- terra::project(herb_crop_resolution, bio1_crop)



#now to do the same thing with the human footprint layer
bio1_foot_crs <- terra::project(bio1_crop, crs(foot))
foot_crop <- crop(foot, bio1_foot_crs)
foot_crop_resolution <- terra::resample(foot, bio1_foot_crs, method = 'bilinear')
foot_final <- terra::project(foot_crop_resolution, crs(bio1_crop))


#we are then going to do our cropping of the bio_layers next. I'm doing the whole stack, but you might need to break out each piece like in the assignment from class.

bio_final <- crop(bio_layers, extent)

#next would be to stack all of them, but they need to be regular rasters and not spatrasters

bio_final_stack <- bio_final %>% raster::stack()
herb_stack <- herb_final %>% raster::stack()
foot_stack <- foot_final %>% raster::stack()

predictors_final <- raster::stack(bio_final_stack, herb_stack, foot_stack)

# plot the last bit of the stack to make sure things worked
plot(predictors_final[[18:21]])