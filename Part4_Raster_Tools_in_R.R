# ---- PART 4: RASTER TOOLS IN R ----

#let's set our working directory first
#setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")
setwd("E:/OneDrive - Texas A&M University - Kingsville/Presentations/Geospatial_Analysis_in_R")
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")

#let's load all the libraries we need
library(ggplot2)
library(terra)
library(tidyterra)
library(mapview)
library(viridis)
library(spatstat)
library(gstat)

# ---- EXAMPLE: HWANGE NATIONAL PARK, ZIMBABWE ----

#first, let's read in our shapefile of Hwange NP with vect()
Hwange <- vect("Example_Zimbabwe/Hwange_NP.shp")

#and do a simple plot
plot(Hwange)

#let's create 1000 random points within the PA for to represent 'lion sightings' using spatSample()
#size = number of points
#method can be regular or random or stratified if using a spatRaster
#replace=T; sampling with replacement (if)
Hwange_pts <- spatSample(Hwange, size=1000, method="random")

#To orient everyone to where Hwange National Park is located, we can look at our spatial data using mapview::mapview()
#Multiple spatial datasets can be added to one plot using a '+' sign between mapview call or using lists
mapview(Hwange, color = "darkgreen", alpha.regions = 0, lwd = 10) + mapview(Hwange_pts)

#what does this look like using ggplot?
ggplot() +
  geom_spatvector(data = Hwange, color = "darkgreen", lwd=1.5) +
  geom_spatvector(data=Hwange_pts, color = "black", size=1)+
  ggtitle("1000 Random Points in Hwange NP")
#depending on your version of R you may need to change the 'size' argument to 'lwd' for lines

#now let's bring in our waterholes and roads 
roads <- vect("Example_Zimbabwe/ZWE_roads.shp")
waterholes <- vect("Example_Zimbabwe/waterholes.shp")

#let's plot those vectors within Hwange
ggplot() +
  geom_spatvector(data=roads, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, color= "blue", size=1)+
  geom_spatvector(data = Hwange, color = "darkgreen", fill=NA, lwd=1.5) +
  ggtitle("Roads and Waterholes in Hwange NP")

#wow, this map looks terrible. how can we change the extent?
#first, let's get the extent for the park
park_extent <- ext(Hwange)

#now we can provide these bounding box coordinates to coord_sf
#here, our coordinate system is WGS 1984 UTM Zone 35S (EPSG 32735)
ggplot() +
  geom_spatvector(data = Hwange, color = "darkgreen", fill = "white", lwd=1.5) +
  geom_spatvector(data=roads, color = "black", lwd=1)+
  geom_spatvector(data=waterholes, color= "blue", size=2)+
  ggtitle("Roads and Waterholes in Hwange NP")+
  coord_sf(datum=crs("EPSG:32735"), xlim=c(park_extent[1], park_extent[2]), ylim=c(park_extent[3], park_extent[4]))
#that's better!

#checking the coordinate systems reveals our "roads" layer is WGS 1984. 
crs(roads, describe=T)

#How can we convert to WGS 1984 UTM Zone 35S in order to match the waterholes?
roads_UTM <- project(roads, "EPSG:32735")

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
summary(values(elev))#wrap our summary function with values() from the terra package
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
plot(Hwange_WGS,add=T, lwd = 5)

#Quick comparison between Cropping and Masking 
#Cropping: Removing rows and/or columns to reduce the raster extent
#Masking: Setting pixels outside of an area of interest to NA

#lets try a mask on the elev layer using Hwange_WGS
elev_mask <- mask(elev, Hwange_WGS)
plot(elev_mask)
plot(Hwange_WGS,add=T, lwd = 5)

#we can also apply the mask after we crop the image
elev_crop_mask <- mask(elev_crop, Hwange_WGS)
plot(elev_crop_mask)
plot(Hwange_WGS,add=T, lwd = 5)
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
plot(Hwange, border="black",col=NA,lwd=5,add=T)
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
plot(Hwange, border="black",col=NA, lwd=2,add=T)

#let's see the spread in values for a subset of cells
hist(percveg)

#let's crop it now, to get rid of the raster extent we don't need
veg_crop <- crop(percveg, elev_crop_UTM) 

#let's see what it looks like now!
plot(veg_crop)
plot(Hwange, border="black",col=NA,lwd=5,add=T)

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
plot(Hwange, border="black",col=NA,lwd=5,add=T)

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

######################################################################################################################################
######################## POTENTIAL NEW EXAMPLE: Point pattern process and interpolation ##############################################
######################################################################################################################################
#we are going to do a quick look at spatial interpolation methods
#let's go back to our watering holes and say we sampled the water for VAR (need a story here)
waterholes_sf <- st_as_sf(waterholes) #let's convert this to an sf object

#first let's simulate some data with a spatially autocorrelated structure
vgm_model <- vgm(psill = 1, model = "Exp", range = 10000, nugget = 0.01)
sim_gstat <- gstat(formula = z ~ 1, locations = ~x + y, dummy = TRUE, beta = 0, model = vgm_model, nmax = 20)
sim_result <- predict(sim_gstat, newdata = waterholes_sf, nsim = 1)
waterholes_sf$new_var <- sim_result$sim1 #create new waterhole variable

#let's look at the spatial pattern
mapview::mapview(waterholes_sf, zcol = "new_var")

#we can spatially create an area of influence of our VAR based on the spatial locations of our waterholes
#we use a function from spatstat r package and some object manipulation to get it into a usable format
tess  <- dirichlet(as.ppp(waterholes_sf)) %>% st_as_sfc() %>% st_as_sf()
st_crs(tess) <- st_crs(waterholes_sf) #reassign crs
tess <- tess %>% st_join(waterholes_sf, fn=mean) %>% st_intersection(st_as_sf(Hwange)) #rejoin attributes and clip to Hwange NP
#let's see what it looks like
#what data type is the final output?
mapview(tess, zcol = "new_var")

#first for this next approach, we need a uniform grid
#let's use our elevation raster but resample to 4km pixels
elev_crop_4km <- aggregate(elev_crop_UTM, 16)
xy <- terra::xyFromCell(elev_crop_4km, 1:ncell(elev_crop_4km))
grid <- st_as_sf(as.data.frame(xy), coords = c("x", "y"), crs = crs(elev_crop_UTM))

#take a look at our grid
mapview(grid)

#inverse distance weighted (idw) example
#unsampled locations are estimated as the weighted average of values from the rest of locations (inversely proportional to distance)
idw <- gstat::idw(new_var ~ 1, waterholes_sf, newdata=grid, idp = 2.0)

# Convert to raster object then clip to Texas
new_var_idw <- rasterize(vect(idw), elev_crop_4km, field = "var1.pred")
new_var_idw <- mask(new_var_idw, Hwange)
mapview(new_var_idw)

#kriging
v <- variogram(new_var ~ 1, data = waterholes_sf, cutoff = 100000, width = 5000) #sample data
plot(v)

#we now need to fit a variogram model to our sample variogram
vinitial <- vgm(psill = 1.2, model = "Exp", range = 10000, nugget = 0.01)
plot(v, vinitial, cutoff = 1000, cex = 1.5)
fv <- fit.variogram(object = v, model = vinitial)
k <- gstat(formula = new_var ~ 1, data = waterholes_sf, model = fv)

#once we have our model accounting for spatial autocorrelation we can predict to unsampled locations
kpred <- predict(k, grid)

#we will make two rasters the prediction and variance 
new_var_krig <- rasterize(vect(kpred), elev_crop_4km, field = c("var1.pred", "var1.var"))
new_var_krig <- mask(new_var_krig, Hwange)

#let's plot our results
ggplot() + geom_spatraster(data = new_var_krig) +
  geom_sf(data = waterholes_sf) +
  scale_fill_viridis(name = "new_var", na.value = "transparent") + theme_bw() +
  facet_wrap(~lyr)

#please see links in slides for how to do "other" tasks that we don't have enough time to cover
#(1) merging rasters together
#(2) basic raster calculations (adding, subtracting)
#(3) convert polygon to raster
#(4) calculating more patch, class, and landscape-level metrics a la FRAGSTATS
#(5) calculating proportion of discrete land cover types within polygons (can be grids or
#buffers around points)