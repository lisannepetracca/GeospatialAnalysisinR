# ---- PART 4: RASTER TOOLS IN R ----

#let's set our working directory first
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")
setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

#let's load all the libraries we need
library(ggplot2)
library(terra)
library(tidyterra)


# ---- EXAMPLE: HWANGE NATIONAL PARK, ZIMBABWE ----

#first, let's read in our shapefile of Hwange NP with vect()
Hwange <- vect("Example_Zimbabwe/Hwange_NP.shp")

#and do a simple plot
plot(Hwange)

#let's create 1000 random points within the PA for vegetation sampling
Hwange_pts <- spatSample(Hwange, size=1000, method="random")

#what does this look like?
ggplot() +
  geom_spatvector(data = Hwange, color = "darkgreen", size=1.5) +
  geom_spatvector(data=Hwange_pts, color = "black", size=1.5)+
  ggtitle("1000 Random Points in Hwange NP")

#now let's bring in our waterholes and roads 
roads <- vect("Example_Zimbabwe/ZWE_roads.shp")
waterholes <- vect("Example_Zimbabwe/waterholes.shp")

#let's plot those vectors within Hwange
ggplot() +
  geom_spatvector(data=roads, color = "black", size=1)+
  geom_spatvector(data=waterholes, color= "blue", size=1)+
  geom_spatvector(data = Hwange, color = "darkgreen", fill=NA, size=1.5) +
  ggtitle("Roads and Waterholes in Hwange NP")

#wow, this map looks terrible. how can we change the extent?
#first, let's get the extent for the park
park_extent <- ext(Hwange)

#now we can provide these bounding box coordinates to coord_sf
#here, our coordinate system is WGS 1984 UTM Zone 35S (EPSG 32735)
ggplot() +
  geom_spatvector(data = Hwange, color = "darkgreen", fill = "white", size=1.5) +
  geom_spatvector(data=roads, color = "black", size=1)+
  geom_spatvector(data=waterholes, color= "blue", size=2)+
  ggtitle("Roads and Waterholes in Hwange NP")+
  coord_sf(datum=crs("EPSG:32735"), xlim=c(park_extent[1], park_extent[2]), ylim=c(park_extent[3], park_extent[4]))
#that's better!

#checking the coordinate systems reveals our "roads" layer is WGS 1984. 
crs(roads, describe=T)

#How can we convert to WGS 1984 UTM Zone 35S in order to match the waterholes?
roads_UTM <- project(roads, "EPSG:32735")

#now let's read in the elevation (it's an aster image)
elev <- rast("Example_Zimbabwe/aster_image_20160624.tif") 

#how can we get an overview of the imported raster?
elev

#this is great, but can we get more stats beyond min/max?
#how can we get, say, quartiles of the data?
#turns out it's the same for any vector or data frame column in R
summary(elev)    #WARNING MESSAGE IS OK

#if you want it to use ALL the values in the dataset, use
summary(elev, maxsamp = ncell(elev))
#not much of a difference, eh? 
#we may notice larger changes w bigger rasters

#here is a relatively fast, simple means of plotting a raster
plot(elev)

#what is the coordinate system? 
crs(elev, describe=T)
#it's WGS84

#let's add Hwange to the elevation tile 
#normally I like projecting layers to the same PROJECTED coordinate system 
#(esp when working with distances and/or areas) 
#but in this instance I will convert the Hwange boundary to WGS because it is faster to convert a vector
#and we are just doing a quick visualization
Hwange_WGS <- project(Hwange, "EPSG:4326")

#let's see what it looks like now!
#add=T adds the Hwange boundary to the existing plot 
plot(Hwange_WGS,add=T)

#ok, so there is a lot of extra raster that we don't want to work with
#let's crop it to make raster processing go a bit faster
elev_crop <- crop(elev, Hwange_WGS)

#let's see what it looks like now!
plot(elev_crop)
plot(Hwange_WGS,add=T)

#what's the coordinate system of the elevation raster again?
crs(elev_crop, describe=T)
#it's WGS 84

#now that the raster is of smaller size, we can convert this to a projected coordinate system
#to match the vector data
#let's use project()
#this goes really fast! this resolution will match our resolution for percent veg cover
elev_crop_UTM <- project(elev_crop, res=250, "EPSG:32735")

#let's make sure it looks ok with our Hwange shapefile in UTM coordinates
plot(elev_crop_UTM)
plot(Hwange, border="black",col=NA,lwd=2,add=T)
#ok, we are good!

#we are going to write this raster to file so we can use it later
#set the GeoTIFF tag for NoDataValue to -9999, the National Ecological Observatory Network’s (NEON) standard NoDataValue
writeRaster(elev_crop_UTM, "Example_Zimbabwe/elev_Hwange.tif", filetype="GTiff", overwrite=T, NAflag=-9999)

#what if we wanted to plot in ggplot?
#it's just a bit trickier bc we have to convert the raster to a data frame first
#this is a sneak peak of our "Cartography" section, which is next!
elev_df <- as.data.frame(elev_crop_UTM, xy=TRUE)

#now we can plot as we did for the vector data, but take note of "geom_raster" argument
#also, note that we are taking the third column of "elev_df" as our color fill
#as this is the column that has our raster values
ggplot() +
  geom_raster(data = elev_df , aes(x = x, y = y, fill = elev_df[,3])) +
  #the below is a color bar that is colorblind-friendly
  scale_fill_viridis_c() +
  geom_spatvector(data = Hwange[1], fill=NA, color="black", size = 1) 

#we can make a histogram within ggplot too
#can help you determine if you have wonky values
#values outside of an expected range can be considered suspect
ggplot() +
  geom_histogram(data = elev_df, aes(elev_df[,3]), bins=40) 

#now let's read in our MODIS data
#we are using the 44B product, or Vegetation Continuous Fields; 250-m resolution
percveg <- rast("Example_Zimbabwe/PercVegCover_2016.tif")
crs(percveg, describe=T)

#let's see what it looks like
plot(percveg)

#ok, this plot is weird bc we are seeing values >100, which represent water
#there are also some values <0
#let's set all values <0 & >100 to NA and plot it
percveg[percveg > 100 | percveg<0] <- NA
plot(percveg)

#let's see what it looks like with Hwange NP
plot(Hwange, border="black",col=NA, lwd=2,add=T)

#let's see the spread in values for a subset of cells
hist(percveg)

#let's crop it now, to get rid of the raster extent we don't need
veg_crop <- crop(percveg, elev_crop_UTM) 

#let's see what it looks like now!
plot(veg_crop)
plot(Hwange, border="black",col=NA,lwd=2,add=T)

#what happens when we try to make a raster stack of vegetation and elevation?
stack <- c(veg_crop, elev_crop_UTM)
#ERROR ab different extents!

#let's check out the extents of each
ext(veg_crop)
ext(elev_crop_UTM)

#the extents are slightly different here, even though they are the same resolution
#this could be from pixels having a different lower left origin, for instance
#we will need to realign extents here through resample()
elev_crop_match <- resample(elev_crop_UTM, veg_crop, method="bilinear")
stack <- c(veg_crop, elev_crop_match)
#yay, it works now!

#a quick aside: let's say that we'd like to have four categories for elevation rather than continuous values
#for example, 800-900 meters = 8, 900-1000 meters = 9, etc.
#in this case, we need to build a reclassification matrix and then use reclassify()

#let's set up those reclassification values
reclass_vals <- c(800,900,8, 
                  900,1000,9,
                  1000,1100,10,
                  1100,1200,11)
#now let's make it a matrix with a certain number of columns, and that we are filling by row
reclass_mat <- matrix(reclass_vals, ncol=3, byrow=TRUE)
#now let's reclassify those values!
elev_reclass <- classify(elev_crop_match, reclass_mat)

#let's see what it looks like now!
plot(elev_reclass)
plot(Hwange, border="black",col=NA,lwd=2,add=T)

#let's move on to getting distances from roads and waterholes
#first, let's crop roads to Hwange extent
roads_hwange <- crop(roads_UTM, Hwange)

#let's plot the roads
plot(roads_hwange)

#for distance to linear features (roads), let's use distance()
#first, we create an empty raster of a certain resolution & extent such that we can *eventually* store our distances there
raster_extent <-  rast(ext(veg_crop), res=250, crs="EPSG:32735")

#now we'll use distance() to calculate the distance between the different geometries
distroad_raster <- distance(raster_extent, roads_hwange)

#we're done! let's plot the output
plot(distroad_raster)
plot(roads_hwange, col="black",lwd=2,add=T)

#now let's calculate distance from points using distance()
#creating another empty raster
raster_extent <- rast(ext(veg_crop), res=250, crs="EPSG:32735")

#calculating distance from points (waterholes) 
distwater_raster <- distance(raster_extent, waterholes)

#plotting the output
plot(distwater_raster)
plot(waterholes, col="black",lwd=2,add=T)

#let's write this to raster to we can use it later
writeRaster(distwater_raster, "Dist_Waterhole_Hwange.tif", overwrite=T)

#quick foray into neighborhood statistics
#let's take the mean elevation using a neighborhood of 15 x 15 cells 
#we are using 15 cells here to show how the values are "smoothed out" visually
elev_focal <- focal(elev_crop_match, w=15, fun=mean, na.rm=TRUE)
#let's see the original
plot(elev_crop_match)
#and now let's see the smoothed out version
plot(elev_focal)

#now we are able to make a raster stack of all four rasters! 
stack <- c(veg_crop, elev_crop_match, distroad_raster, distwater_raster)

#what does the stack look like?
stack

#names are ambiguous. let's assign names
names(stack) <- c("perc_veg", "elev", "dist_road", "dist_waterhole")
stack

#cool. now we will use extract to extract values for each of our 1000 random points
#from each of our four raster layers

#there are a number of arguments that one can make w this function; we are keeping it simple
#df=T just means we are returning the output as a data frame (otherwise will return a list)
#a note that this doesn't have to be used with just points; can be used with polygons (e.g. buffers) too - in that case,
#extract() will extract all of the pixels within those polygons
#you may want to add a "FUN = mean" or some other operation to summarize the pixel values for each polygon
values <- extract(stack, Hwange_pts, df=T)

#what does this look like?
head(values)

#let's write this to .csv!
write.csv(values, "extracted_raster_values.csv", row.names=F)

#how can we save a single raster layer?
#set the GeoTIFF tag for NoDataValue to -9999, the National Ecological Observatory Network’s (NEON) standard NoDataValue
writeRaster(stack$elev, "elevation.tif", filetype="GTiff", overwrite=T, NAflag=-9999)

#how can we save a raster stack?
writeRaster(stack, "raster_stack.tif", filetype="GTiff", overwrite=T, NAflag=-9999)

#THEN, in order to re-import the stack and use the individual raster layers, you can do the below
stack_import<- rast("raster_stack.tif")
stack_import
plot(stack_import)

#if we wish to subset elevation only
elev <- subset(stack_import,subset=2)
plot(elev)

#please see links in slides for how to do "other" tasks that we don't have enough time to cover
#(1) merging rasters together
#(2) basic raster calculations (adding, subtracting)
#(3) convert polygon to raster
#(4) calculating more patch, class, and landscape-level metrics a la FRAGSTATS
#(5) calculating proportion of discrete land cover types within polygons (can be grids or
#buffers around points)