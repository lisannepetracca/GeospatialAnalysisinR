# ---- PART 3: VECTOR TOOLS IN R ----

#let's set our working directory first
#this working directory will link to where you have stored the workshop data on your PC

setwd("ADD DIRECTORY NAME HERE")
#setwd("C:/Users/lisan/OneDrive - Texas A&M University - Kingsville/Geospatial_Analysis_in_R/Geospatial_Analysis_in_R")

#and let's load all the libraries we need
library(terra)
library(ggplot2)
library(rnaturalearth)
library(tidyterra)
library(tidyverse)
library(viridis)
library(mapview)


# ---- EXAMPLE: PROTECTED AREAS IN HONDURAS ----

#in this example, we will explore protected areas in Honduras and subset by name and size

#read in the shapefile with vect()
PAs <- vect("Example_Honduras/Honduras_Protected_Areas_2007.shp")

#let's inspect the attribute table
head(PAs)

#let's fix wonky characters real quick
PA_tibble <- PAs %>% as_tibble() %>% tidyterra::select(1,2) %>% mutate_if(is.character, 
            function(col) iconv(col, from="ISO-8859-1")) 
PAs[,1:2] <- PA_tibble

#let's look at it interactively
mapview(PAs)

#let's see what the columns of the attribute table are
names(PAs)

#alternatively, to see the top six rows of data (same as L. 27)
head(PAs)

#getting the coordinate system
crs(PAs, describe=T)

#What is our extent?
ext(PAs) 

#how many features are in the object?
nrow(PAs)

#let's plot this SpatVector
ggplot() + 
  geom_spatvector(data = PAs, size = 0.5, color = "black", fill = "darkgreen") + 
  ggtitle("PAs in Honduras")

#alternatively
plot(PAs)

#this line will display the first row in the attribute table
#this employs subsetting using base R
plot(PAs[115])

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
(PAs$area_m2 <- expanse(PAs))

#these are really big numbers though. to make it km2, try
(PAs$area_km2 <- expanse(PAs, unit="km"))

#if we want to save this super-cool geometry, we can do
PAs$NOMBRE
write.csv(PAs, "Honduras_PA_areas.csv", row.names=F, fileEncoding = "ISO-8859-1") 

#now, the question is: how do we subset to only those PAs that are >500 km2?
#let's use subsetting with brackets again
BigPAs <- PAs[PAs$area_km2 > 500,]
#how many PAs are greater than 500 km2 in area?
nrow(BigPAs)

#We can also adjust our colors to give each PA a different color
ggplot() + 
  geom_spatvector(data = BigPAs, aes(color = factor(NOMBRE)), linewidth = 1) +
  labs(color = "Name") + #this gives name to legend
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#now let's add an outline of honduras, shall we?
#fun little preview of using online data to get boundaries of countries (can do US states too!)
countries <- ne_download(scale = "large", type = 'countries', returnclass="sf" )
#if this line DOES NOT WORK, skip to L. 120, remove the #, and run that line
#countries<- vect("Example_Honduras/Countries.shp")

#let's look at the names of columns
names(countries)
#this looks like it has names 
countries$NAME
#how many countries are there?
nrow(countries)
#let's grab honduras from this sf object
honduras <- countries[countries$NAME == "Honduras",]

#the line to run if ne_download does not work
# honduras <- vect("Example_Honduras/Honduras_Border.shp")

#and let's plot!
ggplot() + 
  #adds protected areas, with color by name
  geom_spatvector(data = BigPAs, aes(color = factor(NOMBRE)), lwd = 1) +
  #adds border of Honduras
  geom_spatvector(data = honduras, fill=NA, lwd = 1) +
  #label protected areas in legend
  labs(color = 'Name') +
  #add title and subtitle
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#what if we wanted to display PAs as a function of PA size?
ggplot() + 
  #adds protected areas, with fill by AREA
  geom_spatvector(data = BigPAs, aes(fill = area_km2), lwd = 0) +
  #add color palette
  scale_fill_viridis()+
  #adds border of Honduras
  geom_spatvector(data = honduras, fill=NA, lwd = 0.5) +
  #label protected areas in legend
  #NOTE USE OF BQUOTE FOR SUPERSCRIPT
  labs(fill = bquote("Area (km"^2*")")) +
  #add title and subtitle
  ggtitle("Large PAs in Honduras", subtitle = "Subtitle option if you want it!")

#what if we are interested in selecting only those large PAs that intersect Honduras roads?
#read in the shapefile with vect()
honduras_roads <- vect("Example_Honduras/Honduras_Roads_1999_CCAD.shp")

#let's see what these roads look like
mapview(honduras_roads)

#let's use perim() to see how long these roads are
honduras_roads$length <- perim(honduras_roads)
head(honduras_roads)

#let's see what happens if we try to intersect them
PAs_road_isect <- PAs[honduras_roads,]
head(PAs_road_isect)

#this actually works, even though they are in different coordinate systems
#however, many times we will want to ensure that coordinate systems of objects are the same

#these two lines do the same thing
honduras_roads_UTM <- project(honduras_roads, "EPSG:32616")
honduras_roads_UTM <- project(honduras_roads, PAs)

#getting the coordinate system
crs(honduras_roads_UTM, describe=T)
crs(PAs, describe=T)
#yay, they match!

#let's try that intersect again
PAs_road_isect <- PAs[honduras_roads_UTM]

#let's see what we get
#a note that ggplot2 does not require vector data to be in the 
#same projection; ggplot automatically converts all objects to the same CRS before plotting
ggplot() + 
  #plot Honduras boundary in dark grey
  geom_spatvector(data = honduras, fill=NA, color="darkgrey",lwd=1)+
  #plot PAs in dark green
  geom_spatvector(data = PAs_road_isect, color= "darkgreen",lwd = 1) +
  #plot Honduras roads
  geom_spatvector(data = honduras_roads_UTM, lwd = 0.75) +
  #add title and subtitle
  ggtitle("PAs that intersect roads in Honduras", subtitle = "Subtitle option if you want it!")

#let's do one last thing with these PAs - create centroids and save them as .csv
#centroids give us the central points of polygon features
PA_centroids <- centroids(PAs, inside=F)
#this creates a whole new geometry type (points)

#let's see what this looks like
ggplot() + 
  geom_spatvector(data = PAs, colour="darkgreen",fill="lightgreen", lwd = 1) +
  geom_spatvector(data = honduras, fill=NA, lwd = 1) +
  geom_spatvector(data=PA_centroids, colour="yellow", lwd=2)+
  ggtitle("Centroids of PAs in Honduras")

#let's convert it to a data frame and then save as .csv
#the geom="XY" is important because it preserves the X and Y coordinates
PA_centroids_df <- as.data.frame(PA_centroids, geom="XY")
write.csv(PA_centroids_df, "PA_centroids.csv", fileEncoding = "ISO-8859-1", row.names=F)



# ---- EXAMPLE: CAMERA TRAP LOCATIONS IN HONDURAS ----


#In this example, we will create buffers of 500 m around a series of camera traps in 
#Honduras, and then clip those buffers to Honduras land area
#we may want buffers of 500-m if we are looking to calculate percent canopy cover within that radius


#let's import the csv of camera trap locations like any other .csv

camlocs <- read.csv("Example_Honduras/Camera_Coordinates_JeannetteKawas.csv")
class(camlocs)

#let's see what's in this table
head(camlocs)

#we know from our field staff that the coordinate system is WGS 1984 UTM Zone 16N
#this corresponds to EPSG number 32616 on spatialreference.org

#let's transfer to an SpatVector object and assign a coordinate system
camlocs_vec <- vect(camlocs, geom=c("x", "y"), crs="EPSG:32616")

#let's make sure the coordinate system is right
crs(camlocs_vec, describe=T)

#let's see where these camera traps are
mapview(camlocs_vec)

#now let's save this to a .shp if we want to use it in ArcMap 
writeVector(camlocs_vec,
         "camera_locations.shp", filetype = "ESRI Shapefile", overwrite=T) 

#let's first get a distance matrix between points
#pairs = T such that we are getting distances for all pairs of points
#symmetrical = T so that we only get each pairwise distance once
head(camlocs_vec)

dist_matrix <- distance(camlocs_vec, unit="km", pairs=T, symmetrical=T)

#then let's create a buffer of 500 m around the camera trap locations
cam_500m_buffer <- buffer(camlocs_vec, width = 500)

#did it work? let's see by making a map
#ensure that you plot buffers first so that the points can go over them
ggplot() +
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations")

#creating a convex hull polygon around the camera trap locations
cam_convexhull <- convHull(camlocs_vec) 

#let's plot!
ggplot() +
  geom_spatvector(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations")

#pop quiz: how would we then get area of that convex hull polygon?
(area <- expanse(cam_convexhull, unit="km"))

#ok so let's see how this looks when we want to display this polygon over the border of Honduras
#let's read in a more detailed version of Honduras boundary
honduras_detailed <- vect("Example_Honduras/Honduras_Border.shp")

#let's plot where the camera traps are within the country
ggplot() +
  geom_spatvector(data = honduras_detailed) +
  geom_spatvector(data=cam_convexhull, fill="white", color = "blue", size=2)+
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  ggtitle("Map of Camera Trap Locations")

#i am not happy with this map extent. how can we change it?

#let's get the extent of that convex hull polygon first,
extent <- ext(cam_convexhull)

#and then supply this extent to the coord_sf argument in ggplot to reduce the extent
ggplot() +
  geom_spatvector(data = honduras_detailed) +
  geom_spatvector(data=cam_convexhull, fill=NA, color = "blue", size=2)+
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  coord_sf(datum=crs("EPSG:32616"), xlim=c(extent[1], extent[2]), ylim=c(extent[3], extent[4]))+
  ggtitle("Map of Camera Trap Locations")

#if we want a map with meters rather than lat/long along the two axes, add datum=st_crs(xxxx) argument
ggplot() +
  geom_spatvector(data = honduras_detailed) +
  geom_spatvector(data=cam_convexhull, fill=NA, color = "blue", size=2)+
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  coord_sf(datum=crs("EPSG:32616"), xlim=c(extent[1], extent[2]), ylim=c(extent[3], extent[4]))+
  ggtitle("Map of Camera Trap Locations")

#oh no! the convex hull polygon includes the ocean! how can we crop to the honduras_detailed boundary?

#first, let's check the honduras_detailed polygon to see what coordinate system is is using
crs(honduras_detailed, describe=T)

#ok, it is NAD 1927 UTM Zone 16N; this is very close to WGS 1984 UTM Zone 16N but isn't exact
#let's transform the boundary to the same projection as the convex hull polygon

honduras_detailed_UTM <- project(honduras_detailed, "EPSG:32616")

#let's check to make sure the new polygon is in the correct projection
crs(honduras_detailed_UTM, describe=T)

#yay! it worked! now we can proceed with cropping using crop()
cam_convexhull_land <- crop(cam_convexhull, honduras_detailed_UTM)
#quick plot to see if cropping worked
plot(cam_convexhull_land)

#better plot
ggplot() +
  geom_spatvector(data = honduras_detailed_UTM) +
  geom_spatvector(data=cam_convexhull_land, fill=NA, color = "blue", size=2)+
  geom_spatvector(data=cam_500m_buffer, fill="red", color = "black")+
  geom_spatvector(data = camlocs_vec) +
  coord_sf(datum=crs("EPSG:32616"), xlim=c(extent[1], extent[2]), ylim=c(extent[3], extent[4]))+
  ggtitle("Map of Camera Trap Locations")

#how can we get the area (in square km) of this new polygon?
(expanse(cam_convexhull_land, unit="km"))





####---- SAMPLING TOOLS ----####



#let's select the PA in honduras_detailed called "Pico Bonito - Zona Nucleo"
PAs$NOMBRE

#first we will use tidyverse to select the PA by name from the greater multipolygon object
PicoBonito <- PAs %>% filter(NOMBRE == "Pico Bonito-Zona Nucleo")

#let's see where Pico Bonito is
mapview (PicoBonito)

#let's create 100 random points within the PA for vegetation sampling
random_points <- spatSample(PicoBonito, size=100, method="random")

#what does this look like?
ggplot() +
  geom_spatvector(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_spatvector(data=random_points, color = "black", size=2)+
  ggtitle("100 Random Points in Pico Bonito NP")

#let's try again with 100 regular points
regular_points <- spatSample (PicoBonito, size=100, method="regular")

#what does this look like?
ggplot() +
  geom_spatvector(data = PicoBonito, color = "darkgreen", size=1.5) +
  geom_spatvector(data=regular_points, color = "black", size=2)+
  ggtitle("100 Regular Points in Pico Bonito NP")

#then we will create a 16 km2 grid (4 km x 4 km) over the PA
#first we need to create a template and add values
template <- rast(PicoBonito, resolution = c(4000,4000))
values(template) <- 1:ncell(template)

#then transform the raster template to polygons
pico_16km2_grid <- as.polygons(template)
#as an alternative, see st_make_grid() in package sf

#let's check it out -looks good!
plot(pico_16km2_grid)

#let's clip this grid to the boundary of Pico Bonito if we aren't interested in the area outside
pico_16km2_grid_isect <- crop(pico_16km2_grid, PicoBonito)

#how can we see what this looks like?
ggplot() +
  geom_spatvector(data=pico_16km2_grid_isect, fill=NA, color = "black", lwd=1)+
  geom_spatvector(data = PicoBonito, color = "darkgreen", fill=NA, lwd=.75) +
  #use 'expression' and 'paste' to create superscripts, subscripts etc.
  labs(title=expression(paste("16 km" ^{2}," Grid in Pico Bonito NP")))

#What if we want to put in two camera traps at random locations within each grid?

random_points <- spatSample(pico_16km2_grid_isect, size=2, method="random")

#what does it look like?
ggplot() +
  geom_spatvector(data=pico_16km2_grid_isect, fill=NA, color = "darkblue", size=1)+
  geom_spatvector(data = PicoBonito, color = "darkgreen", fill=NA, size=0.75) +
  geom_spatvector(data=random_points, color = "purple", size=2)+
  labs(title=expression(paste("16 km" ^{2}," Hexagon Grid in Pico Bonito NP")))

#oh no. what happened? we only have two points for the whole entire grid
#we need to explicitly tell spatSample to sample within each of the cells within the grid

#let's see how many grids we have
#we have 53!
dim(pico_16km2_grid_isect)   

#let's try again, supplying a vector such that it knows to sample 2 points
#in each of the 53 polygons 
random_points <- spatSample (pico_16km2_grid_isect, size=rep(2,53), method="random")

ggplot() +
  geom_spatvector(data=pico_16km2_grid_isect, fill=NA, color = "darkblue", size=1)+
  geom_spatvector(data = PicoBonito, fill=NA, color = "darkgreen", size=0.75) +
  geom_spatvector(data=random_points, color = "purple", size=2)+
  ggtitle("Two Random Pts Per Grid Cell in Pico Bonito NP")

#let's save these points to a .csv
random_points_df <- as.data.frame(random_points, geom="XY") #remember, geom=XY retains the coordinates
write.csv(random_points_df, "RandomPoints_PicoBonito.csv", row.names=F)

#check it!
head(read.csv("RandomPoints_PicoBonito.csv"))
