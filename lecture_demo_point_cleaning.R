library(tidyverse)
library(rgbif)
library(sp)
library(maptools)
library(dismo)

#install.packages("rnaturalearthdata") this is commented out, delete "#" and only run the first time
library(rnaturalearthdata)

#install.packages("CoordinateCleaner") see above
library(CoordinateCleaner)

#devtools::install_github("ropensci/CoordinateCleaner") this may be necessary if you get a CoordinateCleaner not available for your version of R error

occ <- occ_search(scientificName = "Ambystoma macrodactylum", limit = 20000)

df <- occ$data

df_original <- df

#display the data
world_map <- map_data("world")

map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", colour = "grey") 

df <- df %>%
  dplyr::select(species, decimalLongitude, decimalLatitude, countryCode, individualCount, gbifID, family, taxonRank, year, coordinateUncertaintyInMeters, basisOfRecord, institutionCode, datasetName)

#check for taxonomy issues, unique function pull out all unique values
unique(df$species)

#remove records without coordinates, or really uncertain coordinates
df <- df %>%
  filter(!is.na(decimalLongitude)) %>%
  filter(!is.na(decimalLatitude)) %>%
  filter(coordinateUncertaintyInMeters < 1000)

#remove records based off of time, WorldClim "present" = 1970-2000s
df <- df %>%
  filter(year >= 1970)

#remove duplicate rows based on lon/lat

df <- df %>%
  distinct(decimalLongitude, decimalLatitude, .keep_all=TRUE)

#check for points with other issues
#see link under resources for th full tutorial of this packages to see the entire description of the tests

flags <- clean_coordinates(x = df,
                           lon ="decimalLongitude",
                           lat = "decimalLatitude",
                           countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal", "gbif", "institutions", "zeros", "seas"))

df <- df[flags$.summary,]


#We can check our points here, by plotting the map, the original points, and our new point file

map +
  geom_point(data = df_original, aes(x = decimalLongitude, y = decimalLatitude), colour = "darkgrey", size = 1.5) +
  geom_point(data = df, aes(x = decimalLongitude, y = decimalLatitude), colour = "darkblue", size = 1.5) +
  theme_classic()