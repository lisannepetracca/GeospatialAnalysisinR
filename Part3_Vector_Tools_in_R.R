# ---- VECTOR OPERATIONS ----

#let's set our working directory first
setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

#and let's load all the libraries we need
library(sf)
library(ggplot2)
library(dplyr)
library(raster)
library(units)
library(rnaturalearth)

# ---- EXAMPLE: PROTECTED AREAS IN HONDURAS ----

#in this example, we will explore protected areas in Honduras and subset by name and size

#read in the shapefile with st_read
PAs <- st_read("Example_Honduras/Honduras_Protected_Areas_2007.shp")

#let's see what the columns of the attribute table are
names(PAs)

#alternatively, to see the top six rows of data
head(PAs)

#I wonder what the coordinate system is?
st_crs(PAs)

#a means of getting the most basic info on the coordinate system
crs(PAs)

#What is our extent?
st_bbox(PAs) 

#how many features are in the object?
nrow(PAs)

#can also get all this information just by running the name of the multipolygon
PAs


#let's plot this multipolygon

ggplot() + 
  geom_sf(data = PAs, size = 1, color = "black", fill = "darkgreen") + 
  ggtitle("PAs in Honduras")

#alternatively
plot(PAs)
#however, this will produce a panel for each column in the attribute table

#this line display a single panel, using the first attribute
plot(PAs[1])

#let's explore the different names of PAs in Honduras
#first, let's see what the column names are so we know which one to select
names(PAs)
#now we can inspect that column
PAs$NOMBRE

#let's say we want to extract only those PAs that are National Parks (Parque Nacional)
#to see unique values within a certain column, we can use "unique" argument
unique(PAs$CATEGORIA)

#now let's use bracketing to subset only those PAs that are national parks
NationalParks <- PAs[PAs$CATEGORIA == "Parque Nacional",]

#how many PAs are NPs?
nrow(NationalParks)

#what if there is a numerical condition? such as, what PAs are >2000 km2?
#for this we need to calculate geometry
#let's do area 
PAs$area_m2 <- st_area(PAs)

#these are really big numbers though. to make it km2, try
PAs$area_km2 <- as.numeric(set_units(PAs$area_m2, km^2))

#if we want to save this super-cool geometry, we can do
#append=F means we will overwrite this .csv if we already run this line of code but want to run it again
st_write(PAs, "Honduras_PA_areas.csv", append=F) 

#now, the question is: how do we subset to only those PAs that are >500 km2?
#let's use subsetting with brackets again
BigPAs <- PAs[PAs$area_km2 > 500,]
#how many PAs are greater than 500 km2 in area?
nrow(BigPAs)

#We can also adjust our colors to give each PA a different color
ggplot() + 
  geom_sf(data = BigPAs, aes(color = factor(NOMBRE)), size = 1.5) +
  labs(color = "Name") + #this gives name to legend
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#now let's add an outline of honduras, shall we?
#fun little preview of using online data to get boundaries of countries (can do US states too!)
countries <- ne_download(scale = "large", type = 'countries', returnclass="sf" )
#if this line DOES NOT WORK, skip to L. 106, remove the #, and run that line

names(countries)
#let's grab honduras from this sf object
honduras <- countries %>% filter(NAME == "Honduras")

#the line to run if ne_download does not work
# honduras <- st_read("Example_Honduras/Honduras_Border.shp")

#and let's plot!
ggplot() + 
  geom_sf(data = BigPAs, aes(color = factor(NOMBRE)), size = 1.5) +
  geom_sf(data = honduras, fill=NA, size = 1) +
  labs(color = 'Name') +
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#what if we are interested in selecting only those large PAs that intersect Honduras roads?
#read in the shapefile with st_read
honduras_roads <- st_read("Example_Honduras/Honduras_Roads_1999_CCAD.shp")

#first, let's use st_length() to see how long these roads are
honduras_roads$length <- st_length(honduras_roads)
head(honduras_roads)

#let's see what happens if we try to intersect them
PAs_road_isect <- PAs[honduras_roads,]

#oh man! coordinate systems aren't the same. we need to project the roads to the same coord system
honduras_roads_UTM <- st_transform(honduras_roads, crs =32616)
#alternatively, we could have used "crs = crs(PAs)" to import the CRS from "PAs"

#let's try that intersect again
PAs_road_isect <- PAs[honduras_roads_UTM,]

#let's see what we get
#while the new projection was necessary for the intersection, ggplot2 does not require vector data to be in the 
#same projection; ggplot automatically converts all objects to the same CRS before plotting
ggplot() + 
  geom_sf(data = honduras, fill=NA, color="purple",size=1)+
  geom_sf(data = PAs_road_isect, color= "darkgreen",size = 1.5) +
  geom_sf(data = honduras_roads_UTM, lwd = 1) +
  ggtitle("PAs that intersect roads in Honduras", subtitle = "Subtitle option if you want it!")

#let's do one last thing with these PAs - create centroids and save them as .csv
#centroids give us the central points of polygon features
PA_centroids <- st_centroid(PAs)
#ignore warning message
#this creates a whole new geometry type (points)

#let's see what this looks like
ggplot() + 
  geom_sf(data = PAs, colour="darkgreen",fill="lightgreen", size = 1) +
  geom_sf(data = honduras, fill=NA, size = 1) +
  geom_sf(data=PA_centroids, colour="yellow", size=2)+
  ggtitle("Centroids of PAs in Honduras")

#and then save the centroid coordinates as .csv
st_write(PA_centroids, "PA_centroids.csv", layer_options = "GEOMETRY=AS_XY", append=F)



# ---- EXAMPLE: CAMERA TRAP LOCATIONS IN HONDURAS ---- ####


#In this example, we will create buffers of 500 m around a series of camera traps in 
#Honduras, and then clip those buffers to Honduras land area
#we may want buffers of 500-m if we are looking to calculate percent canopy cover within that radius


#let's import the csv of camera trap locations like any other .csv

camlocs <- read.csv("Example_Honduras/Camera_Coordinates_JeannetteKawas.csv")

#let's see what's in this table
head(camlocs)

#we know from our field staff that the coordinate system is WGS 1984 UTM Zone 16N
#this corresponds to EPSG number 32616 on spatialreference.org

#let's transfer to an sf object and assign a coordinate system
camlocs_sf <- st_as_sf(camlocs, coords = 
                         c("x", "y"), crs = 32616)

#let's make sure the coordinate system is right
st_crs(camlocs_sf)

#or
crs(camlocs_sf)

#let's plot the locations to see where they are
ggplot() +
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

#now let's save this to a .shp if we want to use it in ArcMap
st_write(camlocs_sf,
         "camera_locations.shp", driver = "ESRI Shapefile", append=F)

#let's first get a distance matrix between points
#we supply the camlocs_sf twice so that we are getting a pairwise distance matrix from each
#camera to all other cameras, including itself
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

#creating a convex hull polygon around the camera trap locations
#need to create a multipoint feature from the camlocs_sf (a point feature) and then create the convex hull polygon
cam_convexhull <- st_convex_hull(st_union(camlocs_sf)) 

#let's plot!
ggplot() +
  geom_sf(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

#pop quiz: how would we then get area of that convex hull polygon?
(area <- st_area(cam_convexhull))

#ok so let's see how this looks when we want to display this polygon over the border of Honduras
#let's read in a more detailed version of Honduras boundary
honduras_detailed <- st_read("Example_Honduras/Honduras_Border.shp")

#let's plot where the camera traps are within the country
ggplot() +
  geom_sf(data = honduras_detailed) +
  geom_sf(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  ggtitle("Map of Camera Trap Locations")

#i am not happy with this map extent. how can we change it?

#let's get the extent of that convex hull polygon first,
extent <- st_bbox(cam_convexhull)

#and then supply this extent to the coord_sf argument in ggplot to reduce the extent
ggplot() +
  geom_sf(data = honduras_detailed) +
  geom_sf(data=cam_convexhull, fill=NA, color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  coord_sf(crs=32616, xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))+
  ggtitle("Map of Camera Trap Locations")

#if we want a map with meters rather than lat/long along the two axes, add datum=st_crs(xxxx) argument
ggplot() +
  geom_sf(data = honduras_detailed) +
  geom_sf(data=cam_convexhull, fill=NA, color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  coord_sf(datum=st_crs(32616), xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))+
  ggtitle("Map of Camera Trap Locations")

#oh no! the convex hull polygon includes the ocean! how can we crop to the honduras_detailed boundary?

#first, let's check the honduras_detailed polygon to see what coordinate system is is using
crs(honduras_detailed)

#ok, it is NAD 1927 UTM Zone 16N; this is very close to WGS 1984 UTM Zone 16N but isn't exact
#let's transform the boundary to the same projection as the convex hull polygon

honduras_detailed_UTM <- st_transform(honduras_detailed, crs =32616)

#let's check to make sure the new polygon is in the correct projection
crs(honduras_detailed_UTM)

#yay! it worked! now we can proceed with cropping using st_intersection
cam_convexhull_land <- st_intersection(cam_convexhull, honduras_detailed_UTM)
#quick plot
plot(cam_convexhull_land)

#better plot
ggplot() +
  geom_sf(data = honduras_detailed_UTM) +
  geom_sf(data=cam_convexhull_land, fill=NA, color = "blue", size=2)+
  geom_sf(data=cam_500m_buffer, fill="red", color = "black")+
  geom_sf(data = camlocs_sf) +
  coord_sf(crs=32616, xlim=c(extent[[1]], extent[[3]]), ylim=c(extent[[2]], extent[[4]]))+
  ggtitle("Map of Camera Trap Locations")

#how can we get the area (in square meters) of this new polygon?
(st_area(cam_convexhull_land))



####---- SAMPLING TOOLS ----####



#let's select the PA in honduras_detailed called "Pico Bonito - Zona Nucleo"
PAs$NOMBRE

#first we will use dplyr package to select the PA by name from the greater multipolygon object
PicoBonito <- PAs[PAs$NOMBRE == "Pico Bonito-Zona Nucleo",]

#let's create 100 random points within the PA for vegetation sampling
random_points <- st_sample (PicoBonito, 100, type="random")

#what does this look like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=random_points, color = "black", size=2)+
  ggtitle("100 Random Points in Pico Bonito NP")

#let's try again with 100 regular points
regular_points <- st_sample (PicoBonito, 100, type="regular")

#what does this look like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=regular_points, color = "black", size=2)+
  ggtitle("100 Regular Points in Pico Bonito NP")

#then we will create a 16 km2 grid (4 km x 4 km) over the PA
pico_16km2_grid <- st_make_grid(
  PicoBonito,
  cellsize = 4000,
  crs = 32616,
  what = "polygons",
  square = TRUE
)

#let's clip this grid to the boundary of Pico Bonito if we aren't interested in the area outside
pico_16km2_grid_isect <- st_intersection(pico_16km2_grid, PicoBonito)

#how can we see what this looks like?
ggplot() +
  geom_sf(data=pico_16km2_grid_isect, fill=NA, color = "black", size=2)+
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

#I like the way the hexagon grid looks. You are welcome to clip it if you'd like
#What if we want to put in two camera traps at random locations within each hexagon?

random_points <- st_sample(pico_16km2_grid_hex, size=2, type="random")

#what does it look like?
ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=pico_16km2_grid_hex, fill=NA, color = "darkblue", size=2)+
  geom_sf(data=random_points, color = "red", size=2)+
  labs(title=expression(paste("16 km" ^{2}," Hexagon Grid in Pico Bonito NP")))

#oh no. what happened? we only have two points for the whole entire grid
#we need to explicitly tell st_sample to sample within each of the 63 hexagons within the grid

#let's see how many grids we have; yep, there are 63
pico_16km2_grid_hex

#let's try again, supplying a vector such that it knows to sample 2 points
#in each of the 63 polygons 
random_points <- st_sample (pico_16km2_grid_hex, size=rep(2,63), type="random")

ggplot() +
  geom_sf(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_sf(data=pico_16km2_grid_hex, fill=NA, color = "darkblue", size=2)+
  geom_sf(data=random_points, color = "purple", size=2)+
  ggtitle("Two Random Pts Per Hexagon Cell in Pico Bonito NP")

#let's save these points to a .csv
st_write(random_points, "RandomPoints_PicoBonito.csv", layer_options = "GEOMETRY=AS_XY", append=F)
