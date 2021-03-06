# ---- RASTER TOOLS IN R ----

#let's set our working directory first
setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

#let's load all the libraries we need
library(sf)
library(sp)
library(ggplot2)
library(dplyr)
library(raster)
library(rgdal)
library(velox)
library(rgeos)


# ---- EXAMPLE: HWANGE NATIONAL PARK, ZIMBABWE ----

#first, let's read in our shapefile of Hwange NP
Hwange <- st_read("Example_Zimbabwe/Hwange_NP.shp")
#and do a simple plot
plot(Hwange[c("NAME")])
#or
plot(Hwange[1])

#create random points
#let's create 1000 random points within the PA for vegetation sampling
Hwange_pts <- st_sample(Hwange, 1000, type="random")

#what does this look like?
ggplot() +
  geom_sf(data = Hwange, color = "darkgreen", size=1.5) +
  geom_sf(data=Hwange_pts, color = "black", size=2)+
  ggtitle("1000 Random Points in Hwange NP")

#now let's bring in our waterholes and roads (again using package sf)

roads <- st_read("Example_Zimbabwe/ZWE_roads.shp")
waterholes <- st_read("Example_Zimbabwe/waterholes.shp")

#let's plot those vectors within Hwange
ggplot() +
  geom_sf(data = Hwange, color = "darkgreen", size=1.5) +
  geom_sf(data=roads, color = "black", size=1)+
  geom_sf(data=waterholes, color= "blue", size=3)+
  ggtitle("Roads and Waterholes in Hwange NP")

#man, this map looks terrible. how can we change the extent?

#first, let's get the bounding box for the park
park_extent <- st_bbox(Hwange)

#now we can provide these bounding box coordinates to coord_sf
#here, our coordinate system is WGS 1984 UTM Zone 35S (EPSG 32735)
ggplot() +
  geom_sf(data = Hwange, color = "darkgreen", fill = "white", size=2) +
  geom_sf(data=roads, color = "black", size=1)+
  geom_sf(data=waterholes, color= "blue", size=3)+
  ggtitle("Roads and Waterholes in Hwange NP")+
  coord_sf(crs=32735, xlim=c(park_extent[[1]], park_extent[[3]]), ylim=c(park_extent[[2]], park_extent[[4]]))

#that's better!

#checking the coordinate systems reveals our "roads" layer is WGS 1984. 
crs(roads)
#How can we convert to WGS 1984 UTM Zone 35S?
roads_UTM <- st_transform(roads, crs = 32735)

#now, before reading in the elevation data, let's check it out first
#IGNORE WARNING that "statistics are not supported by this driver"
GDALinfo("Example_Zimbabwe/aster_image_20160624.tif")

#now let's read in the elevation (it's an aster image)
elev <- raster("Example_Zimbabwe/aster_image_20160624.tif") 

#how can we get an overview of the imported raster?
elev

#this is great, but can we get more stats beyond min/max?
#how can we get, say, quartiles of the data?
#turns out it's the same for any vector or data frame column in R
summary(elev)    #WARNING MESSAGE IS OK

#if you want it to use ALL the values in the dataset, use
summary(elev, maxsamp = ncell(elev))
#not much of a difference, eh? may notice larger changes w bigger rasters

#what if we want the mean of the whole raster?
#cellStats can also be used on a raster stack (something we will cover later)
#in that case, will produce a vector where each value is associated with a raster from the stack
(mean <- cellStats(elev, mean))

#here is a relatively fast, simple means of plotting a raster
plot(elev)

#what is the coordinate system? 
crs(elev)
#it's WGS84

#let's add Hwange to the elevation tile (Hwange border needs to be converted to WGS84 first)
#normally I like projecting layers to the same projected coordinate system (esp when working with distances
#and/or areas), but in this instance I will convert the Hwange boundary to WGS because it is faster
#and we are just doing a quick visualization
Hwange_WGS <- st_transform(Hwange, crs=4326)
plot(Hwange_WGS[1], add=T)

#ok, so there is a lot of extra raster that we don't want to work with
#let's use package velox to make raster processing a bit faster

#we'll crop to hwange extent to make things like reprojecting go faster
#we'll get the extent first
extent(Hwange_WGS)
#this line creates a vector from the four numbers within the extent of Hwange_WGS
cropext <- c(extent(Hwange_WGS)[1:4])

#in order to crop a raster, we need to run through a series of steps
#first, we make elev a VeloxRaster object
elev_vx <- velox(elev)
#then we do the crop
elev_vx$crop(cropext)
#then we convert VeloxRaster object to Raster object
elev_crop <- elev_vx$as.RasterLayer(band=1)

#let's see what it looks like now!
#we will plot with the Hwange boundary in WGS 84
plot(elev_crop)
plot(Hwange_WGS[1], border="black",col=NA,lwd=2,add=T)

#what's the coordinate system of the elevation raster again?
crs(elev_crop)
#it's WGS 84

#now that the raster is of smaller size, we can convert this to a projected coordinate system
#to match the vector data
#let's project using projectRaster
#we need to present our crs in a slightly different way than we're used to in package sf

#goes really fast! this resolution will match our resolution for percent veg cover
elev_crop_UTM <- projectRaster(elev_crop, res=250, crs="+init=epsg:32735")

#let's make sure it looks ok with our Hwange shapefile in UTM coordinates
plot(elev_crop_UTM)
plot(Hwange[1], border="black",col=NA, lwd=2,add=T)
#ok, we are good!

#we are going to write this raster to file so we can use it later
#set the GeoTIFF tag for NoDataValue to -9999, the National Ecological Observatory Network’s (NEON) standard NoDataValue
writeRaster(elev_crop_UTM, "Example_Zimbabwe/elev_Hwange.tif", format="GTiff", overwrite=T, NAflag=-9999)

#what if we wanted to plot in ggplot?
#it's just a bit trickier bc we have to convert the raster to a data frame first
elev_df <- as.data.frame(elev_crop_UTM, xy=TRUE)

#now we can plot as we did for the vector data, but take note of "geom_raster" argument
#also, note that we are taking the third column of "elev_df" as our color fill
#as this is the column that has our raster values
ggplot() +
  geom_raster(data = elev_df , aes(x = x, y = y, fill = elev_df[,3])) +
  scale_fill_viridis_c() +
  geom_sf(data = Hwange[1], fill=NA, color="black", size = 1) +
  coord_sf()

#and a spread of data values?
#we can make a histogram within ggplot too
#can help you determine if you have wonky values
#values outside of an expected range can be considered suspect
ggplot() +
  geom_histogram(data = elev_df, aes(elev_df[,3]), bins=40)  #IGNORE WARNING

#now let's read in our MODIS data
#we are using the 44B product, or Vegetation Continuous Fields; 250-m resolution
#the data were originally provided to us as an hdf4 file

###### IMPORTANT ######
#For those of you working with MODIS data, and remote sensing data in general, it is wise to 
#install GDAL for batch tasks, particularly with odd file extensions such as hdf4 (a common
#file format used by NASA, and in my experience with MODIS data)

#We will not be running GDAL in this workshop, but the below lines will work to successfully import
#an .hdf4 file after downloading GDAL (with hdf4 support) through this link:
#https://trac.osgeo.org/osgeo4w/

#Please uncomment the below (L. xx-xx) to see how this would work in GDAL

# library(gdalUtils)
# 
# #creates a list of the subdatasets within the hdf4 MODIS files 
# subdata <- get_subdatasets("Example_Zimbabwe/MOD44B.A2016065.h20v10.006.2017081121817.hdf")
# 
# #ok, let's see what those subdatasets are
# subdata
# subdata[1]
# 
# #i am only interested in percent tree cover, which is the first subdataset
# #now let's use GDAL to convert to a tif!
# #this is the .tif that we will use in the workshop
# 
# gdal_translate(subdata[1], dst_dataset = "PercVegCover_2016.tif")
###### END OF GDAL SECTION THAT IS NOT PART OF WORKSHOP ######

#BACK TO THE WORKSHOP NOW!
#let's read in this .tif as a raster

percveg <- raster("Example_Zimbabwe/PercVegCover_2016.tif")
crs(percveg)
#IGNORE WARNING - this crs is correct
#modis uses a sinusoidal coordinate system that can be found here:
#https://spatialreference.org/ref/sr-org/modis-sinusoidal/

plot(percveg)
#ok, this plot is weird bc we are seeing values >100, which represent various forms of NA

#let's set all values >100 to NA and plot it
percveg[percveg > 100] <- NA
plot(percveg)

#ok so now we have to reproject to WGS 1984 UTM Zone 35S, like the other layers
#normally i would crop first before projecting, but the resolution here is quite coarse (250 m)
#so it shouldn't take too long

#takes ~1 minute
percveg_UTM_S <- projectRaster(percveg, res=250, crs="+init=epsg:32735")

#let's see what it looks like with Hwange NP
plot(percveg_UTM_S)
plot(Hwange[1], border="black",col=NA, lwd=2,add=T)

#let's crop it now, to get rid of the raster we don't need
#using the same steps we used earlier in this exercise
#get the extent for Hwange
cropext <- c(extent(Hwange)[1:4]) 
#convert to VeloxRaster 
veg_vx <- velox(percveg_UTM_S)
#perform cropping using the provided extent
veg_vx$crop(cropext)
#convert to Raster object
veg_crop <- veg_vx$as.RasterLayer(band=1)

#let's see what it looks like now!
plot(veg_crop)
plot(Hwange[1], border="black",col=NA,lwd=2,add=T)

#let's try to make a raster stack of vegetation and elevation
stack <- stack(veg_crop, elev_crop_UTM)
#ERROR ab different extents!
#let's check out the extents of each

extent(veg_crop)
extent(elev_crop_UTM)

#the extents are slightly different here, even though they are the same resolution
#this could be from pixels having a different lower left origin, for instance
#we will need to realign extents here through the resample tool
elev <- resample(elev_crop_UTM, veg_crop, method="bilinear")
stack <- stack(veg_crop, elev)
#yay, it works now!

#a quick aside: let's say that we'd like to have five categories of percent land cover rather than continuous values
#for example, 0-10 = 1, 10-20 = 2, 20-30 = 3, 30-40 = 4, 40-50 = 5
#in this case, we need to build a reclassification matrix and then use the reclassify function

#let's set up those reclassification values
reclass_vals <- c(0,  10, 1, 
                  10, 20, 2,
                  20, 30, 3,
                  30, 40, 4,
                  40, 50, 5)
#now let's make it a matrix with a certain number of columns, and that we are filling by row
reclass_mat <- matrix(reclass_vals, ncol=3, byrow=TRUE)
#now let's reclassify those values!
veg_reclass <- reclassify(veg_crop, reclass_mat)

#let's see what it looks like now!
plot(veg_reclass)
plot(Hwange[1], border="black",col=NA,lwd=2,add=T)

#let's move on to getting distances from roads and waterholes
#first, let's clip roads to hwange extent
roads_hwange <- st_intersection(roads_UTM, Hwange)
#IGNORE WARNING
#let's plot the roads
plot(roads_hwange[1])

#for distance to linear features (roads), let's use rgeos package and its gDistance function
#the steps may seem convoluted, but they get the job done
#first, we create empty raster of a certain resolution & extent such that we can *eventually* store our distances there
dist_road <-  raster(extent(veg_reclass), res=250, crs="+init=epsg:32735")
#need to make roads a spatial object in package sp
roads_sp <- as(roads_hwange,"Spatial")
#let's see what they look like
plot(roads_sp)
#now we'll use gDistance to calculate the distance between the given geometries
#here, it is taking the distance from each road to all the pixels in the extent
#takes ~ 1 min+
distroad_matrix <- gDistance(as(dist_road,"SpatialPoints"), roads_sp,  byid=T)
#IGNORE WARNING - the layers have the same proj4 strings
#with these dimensions, we can see that each raster cell has a distance value to each of the 107 road features
#each row is a road, and each column is a distance to each of the 432165 raster cells
dim(distroad_matrix) 
#but we *really* only want the minimum distance from each raster cell to the nearest road
#so we will take the minimum across columns
distroad_min <-  apply(distroad_matrix,2,min)
#now we give these distances to the empty road matrix (woof! we're nearly done!)
dist_road[] <- distroad_min
#we're done! let's plot the output
plot(dist_road)
plot(roads_hwange[1], col="black",lwd=2,add=T)

#now let's calculate distance from points in package raster
#creating another empty raster
s <- raster(extent(veg_reclass), res=250, crs="+init=epsg:32735")
#calculating distance from points (waterholes) using "distanceFromPoints" function
dist_waterhole <- distanceFromPoints(s, st_coordinates(waterholes))
#plotting the output
plot(dist_waterhole)
plot(waterholes[1], col="black",lwd=2,add=T)

#let's write this to raster to we can use it later
writeRaster(dist_waterhole, "Dist_Waterhole_Hwange.tif", overwrite=T)

#quick foray into neighborhood statistics
#let's take the mean elevation using a neighborhood of 15 x 15 cells 
#we are using 15 cells here to show how the values are "smoothed out" visually
elev_focal <- focal(elev, w=matrix(1,15,15), fun="mean")
#let's see the original
plot(elev)
#and now let's see the smoothed out version
plot(elev_focal)

#now we are able to make a raster stack of all four rasters! 
stack <- stack(veg_crop, elev, dist_road, dist_waterhole )
#what does the stack look like?
stack
#names are ambiguous. let's assign names
names(stack) <- c("perc_veg", "elev", "dist_road", "dist_waterhole")
stack

#cool. now we will use the "extract" tool to extract values for each of our 1000 random points
#from each of our four raster layers

#first, we need to convert to a Spatial* object (a "SpatialPoints" class for package sp)
Hwange_pts_sp <- as(Hwange_pts,"Spatial")

#then we extract values -- this step goes *so* super fast
#there are a number of arguments that one can make w this function; we are keeping it simple
#df=T just means we are returning the output as a data frame (otherwise will return a list)
#a note that this doesn't have to be used with just points; can be used with polygons (e.g. buffers) too - in that case,
#extract() will extract all of the pixels within those polygons
#you may want to add a "FUN = mean" or some other operation to summarize the pixel values for each polygon
values <- extract(stack, Hwange_pts_sp, df=T)
#let's write this to .csv!
write.csv(values, "extracted_raster_values.csv")

#how can we save a single raster layer?
#set the GeoTIFF tag for NoDataValue to -9999, the National Ecological Observatory Network’s (NEON) standard NoDataValue
writeRaster(elev, "elevation.tif", format="GTiff", overwrite=T, NAflag=-9999)

#how can we save a raster stack?
writeRaster(stack, "raster_stack.tif", options="INTERLEAVE=BAND", overwrite=TRUE)
#THEN, in order to re-import the stack and use the individual raster layers, you can use
stack_import<- stack("raster_stack.tif")
elev <- subset(stack_import,subset=2)
plot(elev)

#please see links in slides for how to do "other" tasks that we don't have enough time to cover
#(1) merging rasters together
#(2) basic raster calculations (adding, subtracting)
#(3) convert polygon to raster
#(4) calculating more patch, class, and landscape-level metrics a la FRAGSTATS
#(5) calculating proportion of discrete land cover types within polygons (can be grids or
#buffers around points)