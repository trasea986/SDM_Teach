library(rgbif)
library(sp)
library(raster)
library(maptools)
library(dismo)
library(rgdal)

stream_region_17 <- readOGR("./env_layers_raw/NHDPlusV21_PN_17_NHDPlusCatchment_02/NHDPlusPN/NHDPlus17/NHDPlusCatchment/Catchment.shp")

stream_raster <-rasterize(stream_region_17, bio2_crop)

#trying to point to different fields

stream_raster <- rasterize(stream_region_17, bio2_crop, field = FEATUREID, background = NA)

writeRaster(stream_raster, "stream.asc")
