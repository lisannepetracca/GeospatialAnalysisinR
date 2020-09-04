
# ---- VECTOR OPERATIONS ----

# ---- EXAMPLE: PROTECTED AREAS IN HONDURAS ----

#In this example, we will create buffers of 50 m around a series of camera traps in 
#Honduras, and then clip those buffers to Honduras land area

library(sf)
library(ggplot2)
library(dplyr)
library(raster)
#read in the shapefile with st_read
PAs <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/Honduras_Protected_Areas_2007.shp")

#let's see what the columns of the attribute table are
names(PAs)

#alternatively, to see the top rows of data
head(PAs)

#I wonder what the coordinate system is?
st_crs(PAs)

#What is our extent?
st_bbox(PAs) 

#how many features are in the object?
nrow(PAs)

#can also get all this information just by running the name of the multipolygon
PAs


#let's plot this multipolygon
library(ggplot2)
ggplot() + 
  geom_sf(data = PAs, size = 1, color = "black", fill = "darkgreen") + 
  ggtitle("PAs in Honduras")

#alternatively
plot(PAs)

#NOTE add labels for "nombre" panel

#let's explore the different names of PAs in Honduras
names(PAs)
PAs$NOMBRE

#let's say we want to extract only those PAs that are National Parks (Parque Nacional)
library(dplyr)
PAs$CATEGORIA

NationalParks <- PAs %>% 
  filter(CATEGORIA == "Parque Nacional")

#how many PAs are NPs?
nrow(NationalParks)

#what if there is a numerical condition? such as, what PAs are >2000 km2?
#for this we need to calculate geometry
#let's do area first
PAs$area_m2 <- st_area(PAs)
#these are really big numbers though. to make it km2, try
library(units)
PAs$area_km2 <- as.numeric(set_units(PAs$area_m2, km^2))

#you can do the same for perimeter using st_perimeter!

#if we want to save this super-cool geometry, we can do
st_write(PAs, "test.csv") #geometry as xy?

#now, the question is: how do we subset to only those PAs that are >500 km2?

library(dplyr)
BigPAs <- PAs %>% 
  filter(area_km2 > 500)
#how many PAs are greater than 500 km2 in area?
nrow(BigPAs)

#We can also adjust our colors to give each PA a different color
ggplot() + 
  geom_sf(data = BigPAs, aes(color = factor(NOMBRE)), size = 1.5) +
  labs(color = 'NOMBRE') +
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#now let's add an outline of honduras, shall we?
#fun little preview of using online data to get boundaries of countries (can do US states too!)
library(rnaturalearth)
countries <- ne_download(scale = "large", type = 'countries', returnclass="sf" )
names(countries)
honduras <- countries %>% filter(NAME == "Honduras")
ggplot() + 
  geom_sf(data = BigPAs, aes(color = factor(NOMBRE)), size = 1.5) +
  geom_sf(data = honduras, fill=NA, size = 1) +
  labs(color = 'NOMBRE') +
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#what if we are interested in selecting only those large PAs that intersect Honduras roads?
#read in the shapefile with st_read
honduras_roads <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/Honduras_Roads_1999_CCAD.shp")

#first, let's use st_perimeter() to see how long these roads are
honduras_roads$length <- st_length(honduras_roads)
head(honduras_roads)

#let's see what happens if we try to intersect them
PAs_road_isect <- PAs[honduras_roads,]

#oh man! coordinate systems aren't the same. we need to project the roads to the same coord system
honduras_roads_UTM <- st_transform(honduras_roads, crs = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
PAs_road_isect <- PAs[honduras_roads_UTM,]

#let's see what we get
ggplot() + 
  geom_sf(data = PAs_road_isect, colour= "darkgreen",size = 1.5) +
  geom_sf(data = honduras_roads_UTM, lwd = 1) +
  ggtitle("PAs that intersect roads in Honduras", subtitle = "Subtitle option if you want it!")

#let's do one last thing with these PAs - create centroids and save them as .csv

PA_centroids <- st_centroid(PAs)
#this creates a whole new geometry type (points)

#let's see what this looks like
ggplot() + 
  geom_sf(data = PAs, colour="darkgreen",fill="lightgreen", size = 1) +
  geom_sf(data = honduras, fill=NA, size = 1) +
  geom_sf(data=centroids, colour="yellow", size=2)+
  ggtitle("Centroids of PAs in Honduras")

#and then save the centroid coordinates as .csv
st_write(PA_centroids, "test2.csv", layer_options = "GEOMETRY=AS_XY")

# ---- EXAMPLE: CAMERA TRAP LOCATIONS IN HONDURAS ----

#let's import the csv of camera trap locations like any other .csv

camlocs <- read.csv("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/Camera_Coordinates_JeannetteKawas.csv")

#let's see what's in this table
head(camlocs)

#we know from our field staff that the coordinate system is WGS 1984 UTM Zone 16N
#this corresponds to EPSG number 32616 on spatialreference.org

#let's transfer to an sf object and assign a coordinate system
camlocs_sf <- st_as_sf(camlocs, coords = 
                                     c("x", "y"), crs = 32616)

#let's make sure the coordinate system is right
st_crs(camlocs_sf)

#let's plot the locations to see where they are in honduras
ggplot() +
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

#now let's save this to a .shp if we want to use it in ArcMap
st_write(camlocs_sf,
         "G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/camlocs.shp", driver = "ESRI Shapefile")

#let's first get a distance matrix between points
head(camlocs_sf)
dist_matrix <- st_distance(camlocs_sf, camlocs_sf)


#then let's create a buffer of 500 m around the camera trap locations
cam_500m_buffer <- st_buffer(camlocs_sf, dist = 500)

#did it work? let's see by making a map
#ensure that you plot buffers first so that the points can go over them
ggplot() +
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

test <- st_union(camlocs_sf)
#experimenting w mcp (convex hull?)
library
cam_convexhull <- st_convex_hull(st_union(camlocs_sf)) 
ggplot() +
  geom_sf(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

#pop quiz: how would we then get area of that convex hull polygon?
area <- st_area(cam_convexhull) 

#ok so let's see how this looks when we want to display this polygon over the border of Honduras
#let's read in a more detailed version of Honduras boundary
Honduras <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/Honduras_Border.shp")

ggplot() +
  geom_sf(data = Honduras) +
  geom_sf(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

#i am not happy with this map extent. how can we change it?

extent <- st_bbox(cam_convexhull)
ggplot() +
  geom_sf(data = Honduras) +
  geom_sf(data=cam_convexhull, fill=NA, color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  coord_sf(crs=32616, xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))+
  ggtitle("Map of Camera Trap Locations")

#if we want a map with meters rather than lat/long, add datum=st_crs(xxxx) argument
ggplot() +
  geom_sf(data = Honduras) +
  geom_sf(data=cam_convexhull, fill=NA, color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  coord_sf(datum=st_crs(32616), xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))+
  ggtitle("Map of Camera Trap Locations")

#oh no! the convex hull polygon includes the ocean! how can we crop to the Honduras boundary?

#first, let's check the Honduras polygon to see what coordinate system is is using
st_crs(Honduras)

#ok, it is NAD 1927 UTM Zone 16N; this is very close to WGS 1984 UTM Zone 16N but isn't exact
#let's transform the boundary to the same projection as the convex hull polygon

honduras_UTM <- st_transform(Honduras, crs =32616)

#let's check to make sure the new polygon is in the correct projection
st_crs(honduras_UTM)

#yay! it worked! now we can proceed with cropping
cam_convexhull_land <- st_intersection(cam_convexhull, honduras_UTM)
plot(cam_convexhull_land)
ggplot() +
  geom_sf(data = honduras_UTM) +
  geom_sf(data=cam_convexhull_land, fill=NA, color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  coord_sf(crs=32616, xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))+
  ggtitle("Map of Camera Trap Locations")

#how can we get the area (in square meters) of this new polygon?
st_area(cam_convexhull_land)

#moving ahead to SAMPLING

#let's select the PA in Honduras called "Pico Bonito - Zona Nucleo"
PAs$NOMBRE

#first we will use dplyr package to select the PA by name from the greater multipolygon object
PicoBonito <- PAs %>% 
  filter(NOMBRE == "Pico Bonito-Zona Nucleo")

#let's create 100 random points within the PA for vegetation sampling
random_points <- st_sample (PicoBonito, 100, type="random", exact=T)

#what does this look like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=random_points, color = "black", size=2)+
  labs(title=expression(paste("100 Random Points in Pico Bonito NP")))

#let's try again with 100 regular points
regular_points <- st_sample (PicoBonito, 100, type="regular", exact=T)

#what does this look like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=regular_points, color = "black", size=2)+
  labs(title=expression(paste("100 Regular Points in Pico Bonito NP")))

#then we will create a 16 km2 grid (4 km x 4 km) over the PA
pico_16km2_grid <- st_make_grid(
  PicoBonito,
  cellsize = 4000,
  crs = 32616,
  what = "polygons",
  square = TRUE
)

#how can we see what this looks like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen",size=1.5) +
  geom_sf(data=pico_16km2_grid, fill=NA, color = "black", size=2)+
  labs(title=expression(paste("16 km" ^{2}," Grid in Pico Bonito NP")))

#what if we wanted to do hexagons instead?
pico_16km2_grid_hex <- st_make_grid(
  PicoBonito,
  cellsize = 4000,
  crs = 32616,
  what = "polygons",
  square = FALSE,
  flat_topped = TRUE
)

#how can we see what this looks like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=pico_16km2_grid_hex, fill=NA, color = "darkblue", size=2)+
  labs(title=expression(paste("16 km" ^{2}," Hexagon Grid in Pico Bonito NP")))

#I like the way the hexagon grid looks. What if we want to put in two camera traps at 
#random locations within each hexagon?

random_points <- st_sample (pico_16km2_grid_hex, size=2, type="random", exact=T)

#what does it look like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=pico_16km2_grid_hex, fill=NA, color = "darkblue", size=2)+
  geom_sf(data=random_points, color = "black", size=2)+
  labs(title=expression(paste("16 km" ^{2}," Hexagon Grid in Pico Bonito NP")))

#oh no. what happened? we only have two points for the whole entire grid
#we need to explicitly tell st_sample to sample within each of the 63 hexagons within the grid

#let's see how many grids we have; yep, there are 63
pico_16km2_grid_hex

#let's try again, supplying a vector such that it knows to sample 2 points
#in each of the 63 polygons 
random_points <- st_sample (pico_16km2_grid_hex, size=rep(2,63), type="random", exact=T)

ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=pico_16km2_grid_hex, fill=NA, color = "darkblue", size=2)+
  geom_sf(data=random_points, color = "purple", size=2)+
  labs(title=expression(paste("Two Random Pts Per Hexagon Cell in Pico Bonito NP")))


#let's save these points to a .csv
st_write(random_points, "G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Honduras/camlocs.csv", layer_options = "GEOMETRY=AS_XY")


####---- RASTER TOOLS IN R ----####

library(sf)
library(ggplot2)
library(dplyr)
library(raster)

setwd("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe")
#first, let's read in our shapefile of Hwange NP
Hwange <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe/Hwange_NP.shp")
plot(Hwange[c("NAME")])
#or
plot(Hwange[1])

#create random points
#let's create 100 random points within the PA for vegetation sampling
Hwange_pts <- st_sample(Hwange, 1000, type="random", exact=T)

#what does this look like?
ggplot() +
  geom_sf(data = Hwange, color = "darkgreen", size=1.5) +
  geom_sf(data=Hwange_pts, color = "black", size=2)+
  labs(title=expression(paste("1000 Random Points in Hwange NP")))

#now let's bring in our waterholes and roads (again using package sf)

roads <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe/ZWE_roads.shp")
waterholes <- st_read("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe/waterholes.shp")

ggplot() +
  geom_sf(data = Hwange, color = "darkgreen", size=1.5) +
  geom_sf(data=roads, color = "black", size=1)+
  geom_sf(data=waterholes, color= "blue", size=3)+
  labs(title=expression(paste("Roads and Waterholes in Hwange NP")))

#man, this map looks terrible. how can we change the extent?

#first, let's get the bounding box for the park
park_extent <- st_bbox(Hwange)

#now we can add this to our map using coord_sf
#here, our coordinate system is WGS 1984 UTM Zone 35S (EPSG 32735)
ggplot() +
  geom_sf(data = Hwange, color = "green", fill = "white", size=2) +
  geom_sf(data=roads, color = "black", size=1)+
  geom_sf(data=waterholes, color= "blue", size=3)+
  labs(title=expression(paste("Roads and Waterholes in Hwange NP")))+
  coord_sf(crs=32735, xlim=c(park_extent[[1]], park_extent[[3]]), ylim=c(park_extent[[2]], park_extent[[4]]))

#that's better!

#checking the coordinate systems reveals our "roads" layer is WGS 1984. How can we convert to WGS 1984 UTM Zone 35S?
roads_UTM <- st_transform(roads, crs = 32735)


#now let's read in the elevation (it's an aster image of 15 m resolution)
elev <- raster("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe/aster_image_20160624.tif") 

#plotting a raster of this size is kinda slow and i don't love it, but it works
plot(elev)

#what is the coordinate system? 
crs(elev)

#let's use package velox to make raster processing a bit faster
library(velox)

#let's add Hwange to the elevation tile (needs to be converted to WGS84 first)
Hwange_WGS <- st_transform(Hwange, crs=4326)
plot(Hwange_WGS, add=T)

#ok, so there is a lot of extra raster that we don't want to work with
#let's crop to hwange extent to make things like reprojecting go faster
#let's proceed with the extent for Hwange_WGS 
extent(Hwange_WGS)
#this line creates an object from the four numbers within the extent of Hwange_WGS
cropext <- c(extent(Hwange_WGS)[1:4]) 
elev_vx <- velox(elev)
elev_vx$crop(cropext)
elev_crop <- elev_vx$as.RasterLayer(band=1)

#let's see what it looks like now!
plot(elev_crop)
plot(Hwange_WGS, border="black",col=NA,lwd=2,add=T)

#what's the coordinate system of the elevation raster again?
crs(elev_crop)

#oh man, this doesn't match the other layers (which are in WGS 1984 UTM Zone 10N)
#let's project using projectRaster
#annoyingly, package raster doesn't use the numeric EPSG format like package sf does, so we need to use the proj.4 format
#this is also easily found on spatialreference.org

#goes really fast! this resolution will match our resolution for percent veg cover
elev_crop_UTM <- projectRaster(elev_crop, res=250, crs="+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#let's make sure it looks ok with our Hwange shapefile in UTM coordinates
plot(elev_crop_UTM)
plot(Hwange[1], border="black",col=NA, lwd=2,add=T)
#ok, we are good!

#now let's read in our MODIS data
#we are using the 44B product, or Vegetation Continuous Fields; 250-m resolution
#the data are provided to you as an hdf4 file

###### IMPORTANT ######
#For those of you working with MODIS data, and remote sensing data in general, it is wise to 
#install GDAL for batch tasks, particularly with odd file extensions such as hdf4 (a common
#file format used by NASA, and in my experience with MODIS data)

#We will not be running GDAL in this workshop, but the below lines will work to successfully import
#an .hdf4 file after downloading GDAL (with hdf4 support) through this link:
#https://trac.osgeo.org/osgeo4w/

library(gdalUtils)

#creates a list of the subdatasets within the hdf4 MODIS files 
subdata <- get_subdatasets("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe/MOD44B.A2016065.h20v10.006.2017081121817.hdf")

#ok, let's see what those subdatasets are
subdata
subdata[1]

#i am only interested in percent tree cover, which is the first subdataset
#now let's use GDAL to convert to a tif!
#this is the .tif that we will use in the workshop

gdal_translate(subdata[1], dst_dataset = "PercVegCover_2016.tif")

#now let's read in this .tif as a raster

percveg <- raster("G:/My Drive/GitHub/GeospatialAnalysisinR/Data/Example_Zimbabwe/PercVegCover_2016.tif")

#it doesn't read in the coordinate system, which is annoying
#i know google uses a sinusoidal projection that can be found here:
#https://spatialreference.org/ref/sr-org/modis-sinusoidal/
crs(percveg) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "

plot(percveg)

#let's set all values >100 to NA
percveg[percveg > 100] <- NA
plot(percveg)

#ok so now we have to reproject to WGS 1984 UTM Zone 35S, like the other layers
#no use cropping here because the resolution is coarser (250 m)

#takes <1 minute
percveg_UTM_S <- projectRaster(percveg, res=250, crs="+proj=utm +zone=35 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

#let's see what it looks like with Hwange NP
plot(percveg_UTM_S)
plot(Hwange[1], border="black",col=NA, lwd=2,add=T)

#let's crop so it looks nice
cropext <- c(extent(Hwange)[1:4]) 
veg_vx <- velox(percveg_UTM_S)
veg_vx$crop(cropext)
veg_crop <- veg_vx$as.RasterLayer(band=1)

#let's see what it looks like now!
plot(veg_crop)
plot(Hwange[1], border="black",col=NA,lwd=2,add=T)

#make raster stack
library(gdata)
elev <- resample(elev_crop_UTM, veg_crop)
stack <- stack(veg_crop, elev)

#RECLASSIFY (use perc veg)
# all values > 0 and <= 0.25 become 1, etc.
m <- c(0:100, 0:100,  0.25, 0.5, 2,  0.5, 1, 3)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
rc <- reclassify(r, rclmat)

