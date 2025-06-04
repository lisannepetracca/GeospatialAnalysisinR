# ---- PART 4: RASTER TOOLS IN R ----

#let's set our working directory first
#setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")

#let's load all the libraries we need
library(ggplot2)
library(terra)
library(tidyterra)
library(mapview)
library(viridis)
library(spatstat)
library(gstat)
library(sf)

#### ---- EXAMPLE: HWANGE NATIONAL PARK, ZIMBABWE ---- ####

#first, let's read in our shapefile of Hwange NP with vect()
Hwange <- vect("Example_Zimbabwe/Hwange_NP.shp")

#and do a simple plot
plot(Hwange)

#To orient everyone to where Hwange National Park is located, we can look at our spatial data using mapview::mapview()
mapview(Hwange)

#let's create 1000 random points within the PA for to represent 'lion sightings' using spatSample()
#size = number of points
#method can be regular or random or stratified if using a spatRaster
#replace=T is the option for sampling with replacement
Hwange_pts <- spatSample(Hwange, size=1000, method="random")

#You can use mapview to show multiple spatial datasets! This is via using a '+' sign between mapview calls or using lists
#We can also adjust our visualization by using a few different commands. Color = fill of color of object, alpha.regions = transparency, and lwd = thickness of the polygon outline
mapview(Hwange, color = "darkgreen", alpha.regions = 0, lwd = 10) + mapview(Hwange_pts)

#what does this look like using ggplot?
ggplot() +
  geom_spatvector(data = Hwange, color = "darkgreen", lwd=1.5) +
  geom_spatvector(data = Hwange_pts, color = "black", size=1)+
  ggtitle("1000 Random Points in Hwange NP")
#depending on your version of R you may need to change the 'size' argument to 'lwd' for lines

#now let's bring in our waterholes and roads 
roads <- vect("Example_Zimbabwe/ZWE_roads.shp")
waterholes <- vect("Example_Zimbabwe/waterholes.shp")

#let's plot those vectors within Hwange
ggplot() +
  geom_spatvector(data = roads, color = "black", lwd=1)+
  geom_spatvector(data = waterholes, color= "blue", size=1)+
  geom_spatvector(data = Hwange, color = "darkgreen", fill=NA, lwd=1.5) +
  ggtitle("Roads and Waterholes in Hwange NP")

#wow, this map looks terrible. how can we change the extent?
#first, let's get the extent for the park
park_extent <- ext(Hwange)

#now we can provide these bounding box coordinates to coord_sf
#here, our coordinate system is WGS 1984 UTM Zone 35S (EPSG 32735)
ggplot() +
  geom_spatvector(data = Hwange, color = "darkgreen", fill = "white", lwd=1.5) +
  geom_spatvector(data = roads, color = "black", lwd=1)+
  geom_spatvector(data = waterholes, color= "blue", size=2)+
  ggtitle("Roads and Waterholes in Hwange NP")+
  coord_sf(datum=crs("EPSG:32735"), xlim=c(park_extent[1], park_extent[2]), ylim=c(park_extent[3], park_extent[4]))
#that's better!

#checking the coordinate systems reveals our "roads" layer is WGS 1984. 
crs(roads, describe=T)

#How can we convert to WGS 1984 UTM Zone 35S in order to match the waterholes?
roads_UTM <- project(roads, "EPSG:32735")


#### ---- BASIC RASTER STATISTICS ---- ####

#OK now time for a raster! Let's read in the elevation (it's an aster image)
elev <- rast("Example_Zimbabwe/aster_image_20160624.tif") 

#how can we get an overview of the imported raster?
#what does this tell us?
elev

#this is great, but can we get more stats beyond min/max?
#how can we get, say, quartiles of the data?
#turns out it's the same for any vector or data frame column in R
summary(elev)    #WARNING MESSAGE IS OK

#if you want it to use ALL the values in the dataset, use
summary(values(elev)) #wrap our summary function with values() from the terra package
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
plot(Hwange_WGS, add=T)


#### ---- USING CROP AND MASK TOOLS ---- ####

#ok, so there is a lot of extra raster that we don't want to work with
#let's crop it to make raster processing go a bit faster
elev_crop <- crop(elev, Hwange_WGS)

#let's see what it looks like now!
plot(elev_crop)
plot(Hwange_WGS, add=T, lwd = 5)

#Quick comparison between Cropping and Masking 
#Cropping: Removing rows and/or columns to reduce the raster extent
#Masking: Setting pixels outside of an area of interest to NA (similar to "SetNull" in ArcMap)

#lets try a mask on the elev layer using Hwange_WGS
elev_mask <- mask(elev, Hwange_WGS)
plot(elev_mask)
plot(Hwange_WGS, add=T, lwd = 5)
#the only weird thing is that the extent stays the same as the original elev layer!

#let's fix that by applying the mask after we crop the image
elev_crop_mask <- mask(elev_crop, Hwange_WGS)
plot(elev_crop_mask)
plot(Hwange_WGS, add=T, lwd = 5)
#masking can be helpful for creating maps, accounting for boundaries, and restricting any analysis to a region of interest 

#Ok back to working with our cropped elevation layer
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
plot(Hwange, border="black", col=NA, lwd=5, add=T)
#ok, we are good!

#we are going to write this raster to file so we can use it later
#set the GeoTIFF tag for NoDataValue to -9999, the National Ecological Observatory Network’s (NEON) standard NoDataValue
writeRaster(elev_crop_UTM, "Example_Zimbabwe/elev_Hwange.tif", filetype="GTiff", overwrite=T, NAflag=-9999)

#what if we wanted to plot in ggplot? - use geom_spatraster!
ggplot() +
  geom_spatraster(data = elev_crop_UTM) +
  #the below is a color bar that is colorblind-friendly
  scale_fill_viridis_c() +
  geom_spatvector(data = Hwange[1], fill=NA, color="black", lwd = 2) 

#we can make a histogram within ggplot too
#can help you determine if you have wonky values
#first convert to a data frame
#values outside of an expected range can be considered suspect
elev_df <- as.data.frame(elev_crop_UTM, xy=TRUE)

#plot it!
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
plot(Hwange, border="black", col=NA, lwd=2, add=T)

#let's see the spread in values for a subset of cells
hist(percveg) #IGNORE WARNING

#let's crop it now, to get rid of the raster extent we don't need
veg_crop <- crop(percveg, elev_crop_UTM) 

#let's see what it looks like now!
plot(veg_crop)
plot(Hwange, border="black", col=NA, lwd=5, add=T)


#### ---- MAKING A RASTER STACK ---- ####

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
#make sure method aligns with data type (i.e. categorical vs. continuous) see ?resample

stack <- c(veg_crop, elev_crop_match)
#yay, it works now!

#### ---- USING RECLASSIFY ---- ####

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
plot(Hwange, border="black", col=NA, lwd=5, add=T)



#### ---- BUILDING DISTANCE LAYERS ---- ####

#let's move on to getting distances from roads and waterholes
#first, let's crop roads to Hwange extent
roads_hwange <- crop(roads_UTM, Hwange)

#let's plot the roads
plot(roads_hwange)

#for distance to linear features (roads), let's use distance()
#first, we create an empty raster of a certain resolution & extent such that we can *eventually* store our distances there
#We did something very similar creating a raster from scratch in part 2 but now we specify
#resolution by 'res='
raster_extent <-  rast(ext(veg_crop), res=250, crs="EPSG:32735")

#now we'll use distance() to calculate the distance between the different geometries
distroad_raster <- distance(raster_extent, roads_hwange)

#we're done! let's plot the output
plot(distroad_raster)
plot(roads_hwange, col="black", lwd=2, add=T)

#now let's calculate distance from points using distance()
#creating another empty raster
raster_extent <- rast(ext(veg_crop), res=250, crs="EPSG:32735")

#calculating distance from points (waterholes) 
distwater_raster <- distance(raster_extent, waterholes)

#plotting the output
plot(distwater_raster)
plot(waterholes, col="black", lwd=2, add=T)

#let's write this to raster to we can use it later
writeRaster(distwater_raster, "Dist_Waterhole_Hwange.tif", overwrite=T)


#### ---- NEIGHBORHOOD STATISTICS ---- ####

#quick foray into neighborhood statistics
#let's take the mean elevation using a neighborhood of 15 x 15 cells 
#we are using 15 cells here to show how the values are "smoothed out" visually
elev_focal <- focal(elev_crop_match, w=15, fun=mean, na.rm=TRUE)
#let's plot them together and compare
elev_stack <- c(elev_crop_match, elev_focal)
plot(elev_stack)

#now we are able to make a raster stack of all four rasters! 
stack <- c(veg_crop, elev_crop_match, distroad_raster, distwater_raster)

#what does the stack look like?
stack

#names are ambiguous. let's assign names
names(stack) <- c("perc_veg", "elev", "dist_road", "dist_waterhole")
stack


#### ---- USING EXTRACT ---- ####

#cool. now we will use extract to extract values for each of our 1000 random points
#from each of our four raster layers

#there are a number of arguments that one can make w this function; we are keeping it simple
#df=T just means we are returning the output as a data frame (otherwise will return a list)
#a note that this doesn't have to be used with just points; can be used with polygons (e.g. buffers) too - in that case,
#extract() will extract all of the pixels within those polygons
#you may want to add a "FUN = mean" or some other operation to summarize the pixel values for each polygon
values <- terra::extract(stack, Hwange_pts, df=T)

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

##############################################################################################################################################################
######################## BONUS (if time allows): Short introduction to point pattern process and interpolation ###############################################
##############################################################################################################################################################

#we are going to do a quick look at spatial interpolation methods
#let's go back to our waterholes and say we sampled the water and calculated parasite density and 
#are now interested in predicting what parasite density might look like in unsampled locations

#we first need some data
waterholes_df <- read.csv("waterholes_df.csv") #read in out data as a csv file
#next we need to convert our data frame to a spatial object
#let's make this an sf object using st_as_sf() where we input the object, coordinates, and the crs of our data
waterholes_sf <- st_as_sf(waterholes_df, coords = c("X","Y"), crs = 32735) 

#let's look at the spatial pattern of parasite density across our waterholes
mapview(waterholes_sf, zcol = "para_den") #zcol: adds a gradient fill based on values in a dataframe column

#we can spatially create an area of influence of our parasite density based on the spatial locations of our waterholes
#we use a function from spatstat r package and some object manipulation to get it into a usable format
#the dirichlet function computes Dirichlet tessellation of a spatial point pattern (also known has the Voronoi and Thiessen tessellation)
#as.ppp() converts our sf object into a spatial point pattern class object for the computation
#we then need to convert back to an sf object for plotting 
tess  <- dirichlet(as.ppp(waterholes_sf)) %>% st_as_sfc() %>% st_as_sf()
st_crs(tess) <- st_crs(waterholes_sf) #reassign our crs
tess <- tess %>% st_join(waterholes_sf, fn=mean) %>% st_intersection(st_as_sf(Hwange)) #rejoin attributes and clip to Hwange NP
#let's see what it looks like
#IGNORE WARNING
#what data type is the final output?
mapview(tess, zcol = "para_den")

#let's look at two more simple approaches to spatial interpolation
#for these approachs, we need a uniform grid
#let's use our elevation raster but resample to 4km pixels to make a uniform grid across Hwange NP
#we can use the aggregate in terra to create a new spatraster with a lower resolution
elev_crop_4km <- aggregate(elev_crop_UTM, 16)
elev_crop_4km
xy <- terra::xyFromCell(elev_crop_4km, 1:ncell(elev_crop_4km)) #this will get the coordinates of the center of the raster cell
grid <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = crs(elev_crop_UTM)) #now create an empty sf object that represents our grid

#take a look at our grid
mapview(grid) #we now have a uniform grid across space that we can make predictions on

#first approach: inverse distance weighted (idw)
#unsampled locations are estimated as the weighted average of values from the rest of locations (inversely proportional to distance)
#we can use the idw function in the gstat function with a few input parameters
#formula: defines the dependent variable and in this case, an intercept only model
#we also tell it the data file (waterholes_sf) and the new data to make predictions on (grid)
idw <- gstat::idw(para_den ~ 1, waterholes_sf, newdata=grid)

#convert to raster object then mask to HWange NP
#we can use the rasterize function in terra to convert our vector grid to a spatraster that matches the spatial properties of our resampled elevation raster
#we also tell the function which field we want to use as our raster cell values (field = "var1.pred")
para_den_idw <- rasterize(idw, elev_crop_4km, field = "var1.pred")
para_den_idw <- mask(para_den_idw, Hwange)
mapview(para_den_idw)

#second approach: kriging
#unlike the previous approaches that are deterministic (based on distances), kriging is probabilistic and relies on statistical properties of observations
#quantifies spatial autocorrelation and accounts for spatial configuration to make predictions
#first we need to fit a variogram to our sample data
#we set the cutoff of the spatial separation distance between points to include in our estimates and the distance intervals (widths) to group data points
v <- variogram(para_den ~ 1, data = waterholes_sf, cutoff = 100000, width = 5000)
plot(v)

#we now need to fit a variogram model to our sample variogram
#to do this we can use the vgm() function in gstat that will generate a variogram model based on a few input parameters
#we need to specify a model function, and in this case, we are using an Exponential function
#using an exponential function means that the correlation between points diminishes gradually 
#as the distance between them increases, approaching a "sill" or plateau 

#you need to describe the shape of the function using the sill, range, and nugget parameters
#sill: maximum variability between two points
#range: the lag distance where the variogram levels off 
#at the range, two points are not spatially correlated if separated by that distance or greater
#nugget: this represents small scale variability or y-intercept
vinitial <- vgm(psill = 1, model = "Exp", range = 10000, nugget = 0.01)
plot(v, vinitial, cutoff = 1000, cex = 1.5, lwd=2)
#now we want to fit our variogram model to our observed/sample variogram 
fv <- fit.variogram(object = v, model = vinitial)
#we use the same function again to create a gstat object with our data and fitted variogram model
k <- gstat(formula = para_den ~ 1, data = waterholes_sf, model = fv)

#once we have our model accounting for spatial autocorrelation we can predict to unsampled locations using the same grid as the idw example
kpred <- predict(k, grid)

#we can once again use the rasterize function in terra to convert our vector grid to a spatraster that matches the spatial properties of our resampled elevation raster
#we also tell the function that we want our prediction and variance field as our raster cell values
para_den_krig <- rasterize(vect(kpred), elev_crop_4km, field = c("var1.pred", "var1.var"))
para_den_krig <- mask(para_den_krig, Hwange)
#take a quick look at the object that is returned
para_den_krig

#let's plot our results
ggplot() + geom_spatraster(data = para_den_krig) +
  geom_sf(data = waterholes_sf) +
  scale_fill_viridis(name = "Parasite density", na.value = "transparent") + theme_bw() +
  facet_wrap(~lyr) #plot each layer separately

#that's it, a quick look at spatial interpolation methods
#this is an example of ordinary kriging, but many other approaches like universal kriging (include covariates) or Bayesian kriging (integrated nested Laplace approximation)

############ Additional resources ################################################################################################################################################
#please see links in slides for how to do "other" tasks that we don't have enough time to cover
#(1) merging rasters together
#(2) basic raster calculations (adding, subtracting)
#(3) convert polygon to raster
#(4) calculating more patch, class, and landscape-level metrics a la FRAGSTATS
#(5) calculating proportion of discrete land cover types within polygons (can be grids or
#buffers around points)