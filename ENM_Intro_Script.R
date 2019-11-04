#To do a distribution model, you need two things. First, you need location information, and second, you need environmental information for the area you want to model. But first, let us spend some time getting and looking at the data.

#to start with, we will bring in presence points from gbif.org, which is a repository of occurance data. feel free to check out their website!

require(rgbif) #instead of require, you can use "library(package name)" but require should download if you do not have it.

#first make an object with the countries you are interested in using the official 2 letters. I reccomend picking one or two countries.
countries <- c("US","CA")


#so let us get some present data. the first part is the function to find the data, and then we pass various arguments. if you are lost on all of the options, and there are a lot, type ?function, where 'function' in this case would be occ_search

#here we are creating an object named "rainbow_trout" and use that throughout. But if you use a different species by changing the scientificnName argument below, you may want to name it  something else. do not be afraid to play with things

#now pull in data. "limit" is how many GPS points you want. to speed things up or if your internet is slow and the connection drops out, try lowering the number to 250 or 500. lots of rainbow trout records.
rainbow_trout <- occ_search(scientificName= "Oncorhynchus mykiss", country=countries, limit=10000)

#this is a gbif object, which has a lot of data to it. however, all we really want is the latitude and longitude so we will make a new dataframe out of the data object
#this is just a formatting tweak. keep in mind I used a new name here ("1") so that I still have my original object. We also have a dataset for each country from above, so you would need to expand this if you use more than 2, or only run one set of code if you only did one country.
rainbow_trout1 <- as.data.frame(rainbow_trout$US$data) #first country data
rainbow_trout2 <- as.data.frame(rainbow_trout$CA$data) #second country

#this removes everything except location information
rainbow_trout_loc1 <- rainbow_trout1[,c("scientificName","decimalLatitude","decimalLongitude")]
rainbow_trout_loc2 <- rainbow_trout2[,c("scientificName","decimalLatitude","decimalLongitude")]

#and finally, bringing the countries together
rainbow_trout_loc3 <- rbind(rainbow_trout_loc1, rainbow_trout_loc2)

#if we want to see what this object looks like, we just give R its name
rainbow_trout_loc3

####So now we have points, and just need environmental information
require(sp)
require(raster)

#this creates a list of rasters using the bioclim variables, which are biologicall relevant forms of temperature and precipiation. See WorldClim / BioClim websites for more information.

#better resolutions are great, but be cautious, these can be very large. Try 10 if you are short on space in the "res" argument.

worldclim <- getData("worldclim", var="bio", res=2.5)

#now before we get fancy with Maxent and distrbution models, just check out what the values are at your points for annual temperature


#first, here is mean annual temperature in tenths of a degree celsius
plot(worldclim$bio1)

#feel free to explore the other values, and check out: https://worldclim.org/bioclim if you have not already


#we can also turn our dataframe of points in to spatial point object, but one more line of code, which removes anything with missing data
rainbow_trout_loc3<-na.omit(rainbow_trout_loc3)
coordinates(rainbow_trout_loc3) <- ~decimalLongitude+decimalLatitude 

plot(rainbow_trout_loc3, add=TRUE)

#this may look a little funny, because your points may only be in a small part of the world
#so, let us do it again but flip the order.
#we are also going to get an existing map now

require(maptools)

data("wrld_simpl")
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE) #feel free to change the limits of your map to make it look nicer. I selected an area focused on the US

#add in the climate variable, but with new colors. n here is the number of color breaks. more=smoother colors but slower for the computer to make
#"add" keeps us from resetting the plot
#smallplot controls our legend
plot(worldclim$bio1, add=TRUE, legend=FALSE, col=hcl.colors(palette='viridis', n=100))


plot(worldclim$bio1, legend.only=TRUE, col=hcl.colors(palette='viridis', n=100), legend.width=1, legend.shrink=0.75, legend.args=list(text='Temperature (1/10 degree Celsius)', side=4, font=2, line=2.5, cex=0.8),
     smallplot=c(.10,.15, .3,.75)); par(mar = par("mar"))

plot(rainbow_trout_loc3, add=TRUE, pch=16) #get the points back

#we can even add a second species here...

brook_trout <- occ_search(scientificName= "Salvelinus fontinalis", country=countries, limit=10000)
brook_trout1 <- as.data.frame(brook_trout$US$data) #first country data
brook_trout2 <- as.data.frame(brook_trout$CA$data) #second country
brook_trout_loc1 <- brook_trout1[,c("scientificName","decimalLatitude","decimalLongitude")]
brook_trout_loc2 <- brook_trout2[,c("scientificName","decimalLatitude","decimalLongitude")]
brook_trout_loc3 <- rbind(brook_trout_loc1, brook_trout_loc2)
brook_trout_loc3<-na.omit(brook_trout_loc3)
coordinates(brook_trout_loc3) <- ~decimalLongitude+decimalLatitude

plot(brook_trout_loc3, add=TRUE, pch=15, col="grey")


#if you want to play with colors and so on, and want to clear the plot area use and start fresh without changing the "add" part of your code:
graphics.off()

#now to explore what the environmental data looks like at our GPS points
#make a new data frame
rainbow_ann_temp <- as.data.frame(rainbow_trout_loc3)
#extract values to a new columm "Temperature
rainbow_ann_temp$Temperature <- extract(worldclim$bio1, rainbow_trout_loc3)
head(rainbow_ann_temp) #head just shows the first part of the dataframe so we can make sure things worked. to see the whole thing, just type the name of the object.

#now we can plot the histogram of the environmental data
hist(rainbow_ann_temp$Temperature)
#if you tried another species above, compare it here simply by changing the object names

#for example
brook_ann_temp <- as.data.frame(brook_trout_loc3)
brook_ann_temp$Temperature <- extract(worldclim$bio1, brook_trout_loc3)
head(brook_ann_temp)

#now we can plot the histogram of the environmental data
hist(brook_ann_temp$Temperature)


#Do they have different environments they live in?
#Can you make a histogram of annual percipitation?


#so obviously looking and trying to compare each one would be difficult, so let us build a mathematical model of the environmental suitability for each species. said another way, where are we most likely to find it?

require(dismo)

rainbow_bioclim <- bioclim(worldclim, rainbow_trout_loc3)
rainbow_prediction <- predict(rainbow_bioclim, worldclim, progress='text')

#now to plot world
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE)

#going to make very small values invisible and then plot with a fun color scheme
rainbow_prediction2 <- rainbow_prediction #this is just a backup object... wouldn't want to run predict again and that way you can play with the threshold of 0.01 by running the backup code again
rainbow_prediction2[rainbow_prediction2 < 0.01] <- NA
#one important argument here is "add" which means to put the plots together
plot(rainbow_prediction2, add=TRUE, col=hcl.colors(palette="Zissou 1",60))

brook_bioclim <- bioclim(worldclim, brook_trout_loc3)
brook_prediction <- predict(brook_bioclim, worldclim, progress='text')
brook_prediction2 <- brook_prediction
brook_prediction2[brook_prediction2 < 0.01] <- NA

#Now, instead of just going back and forth between the two, take a look at them side by side. See if you can ifugre out the extra arguments here.
old.par <- par(mfrow=c(1, 2)) #tells R you are making two plots
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE, main = "Rainbow Trout")
plot(rainbow_prediction2, add=TRUE, col=hcl.colors(palette="Zissou 1",60))
plot(wrld_simpl, xlim=c(-160,-60), ylim=c(25, 80), axes=TRUE,  main = "Brook Trout")
plot(brook_prediction2, add=TRUE, col=hcl.colors(palette="Zissou 1",60))


#to get rid of plotting two columns, use
#dev.off()