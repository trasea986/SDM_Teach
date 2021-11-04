library(tidyverse)
library(rgbif)
library(sp)
library(maptools)
library(dismo)

#demo install.packages error
install.packages("rnaturalearthdata")
library(rnaturalearthdata)

install.packages("CoordinateCleaner")

devtools::install_github("ropensci/CoordinateCleaner")
library(CoordinateCleaner)

#obtain data from GBIF via rgbif
occ <- occ_search(scientificName = "Ambystoma macrodactylum", limit = 20000)

df <- occ$data

#for comparing later
df_original <- occ$data

#display data
world_map <- map_data("world")

map <- ggplot() +
  geom_polygon(data=world_map, aes(x = long, y = lat, group = group), fill="white", colour = "grey")

#map +
#  geom_point(data = df_original, aes(x = decimalLongitude, y = decimalLatitude),
#             colour = "darkgrey", size = 1.5, alpha = 0.5)+
#  geom_point(data = df_final, aes(x = decimalLongitude, y = decimalLatitude),
#             colour = "darkblue", size = 1.5, alpha = 0.5)+
#  xlim(min(df$decimalLongitude)-45, max(df$decimalLongitude)+45) +
#  ylim(min(df$decimalLatitude)-30, max(df$decimalLatitude)+30) +
#  theme_classic()

map +
  geom_point(data = df, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkblue", size = 0.5)+
  xlim(min(df$decimalLongitude)-45, max(df$decimalLongitude)+45) +
  ylim(min(df$decimalLatitude)-30, max(df$decimalLatitude)+30) +
  theme_classic()


#select columns of interest
df <- df %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount,
                gbifID, family, taxonRank, coordinateUncertaintyInMeters, year,
                basisOfRecord, institutionCode, datasetName)



#check for taxonomy issues
unique(df$species)


# remove records without coordinates, but also high uncertainty
df <- df%>%
  filter(!is.na(decimalLongitude))%>%
  filter(!is.na(decimalLatitude)) %>%
  filter(coordinateUncertaintyInMeters < 1000)



#pull only more recent points >= 1950. WorldClim "present" is 1970-2000 averages
df <- df %>% filter(year >= 1970)

#remove duplicate rows based on lon/lat
#note may have same point if collected in different years

df <- df %>% distinct(decimalLongitude, decimalLatitude, .keep_all= TRUE)


#remove points that may be too far away or problematic with cleaner(see east coast point, likely mis identified)
#can also do temporal outliers
flags <- clean_coordinates(x = df,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros", "seas")) # most test are on by default
df <- df[flags$.summary,]

#can also remove based on fixed location
#exclude based on study area
#study_area <- df %>% 
#  filter(decimalLatitude < 40) %>%
#  filter(decimalLatitude > 10)


#might have sampling bias issues. note: maxent will remove if more points than in a single cell
df_cor <- df

#need spatial points
coordinates(df_cor) <- ~decimalLongitude+decimalLatitude

#create a raster with extent from spatial points
r <- raster(df_cor)

# set the resolution of the cells to (for example) 0.5 degree
res(r) <- 0.5

# expand (extend) the extent of the RasterLayer a little
r <- extend(r, extent(r)+1)

# sample:
df1 <- gridSample(df_cor, r, n=1)

p <- rasterToPolygons(r)
plot(p, border='gray')
points(df_cor)
# selected points in green
points(df1, cex=1, col='darkgreen', pch='x')

df1 <- as.data.frame(df1)

df_final <- df1


#now to figure out our random background points:
projection(df_cor) <- CRS('+proj=longlat +datum=WGS84')

# circles with a radius of 150 km
x <- circles(df_cor, d=150000, lonlat=TRUE)
## Loading required namespace: rgeos
pol <- polygons(x)

#Note that you need to have the rgeos package installed for the circles function to ‘dissolve’ the circles (remove boundaries were circles overlap).

#And then we take a random sample of points within the polygons. We only want one point per grid cell.

# sample randomly from all circles
samp1 <- spsample(pol, 500, type='random')
# get unique cells
cells <- cellFromXY(r, samp1)
length(cells)
## [1] 250
cells <- unique(cells)
length(cells)
## [1] 161
xy <- xyFromCell(r, cells)
#Plot to inspect the results:
  
plot(pol, axes=TRUE)
points(xy, cex=0.75, pch=20, col='blue')

bg_final <- as.data.frame(xy)


occ_map +
  geom_point(data = bg_final, aes(x = x, y = y),
             colour = "green", size = 1.5)+
  geom_point(data = df_final, aes(x = decimalLongitude, y = decimalLatitude),
             colour = "darkblue", size = 1.5)+
  xlim(min(df$decimalLongitude)-10, max(df$decimalLongitude)+10) +
  ylim(min(df$decimalLatitude)-10, max(df$decimalLatitude)+10) +
  theme_classic()

#save what you have done
write.csv(df_final, "points_cleaned.csv")
ggsave(occ_map, "occ_map.jpeg", dpi = 360)
