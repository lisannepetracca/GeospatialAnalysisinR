#for this exercise, we will
#(1) select "La Tigra-nucleo" from the Honduras PAs shapefile
#(2) determine area of that PA in km2
#(3) create a 1 km x 1 km grid over the PA
#(4) clip that grid to the PA boundary
#(5) create 2 random points in each of those grid cells
#(6) plot it!

#change this to your working directory
setwd("YOUR WORKING DIRECTORY HERE")

library(terra)
library(ggplot2)
library(mapview)

#read in PAs
PAs <- vect("Example_Honduras/Honduras_Protected_Areas_2007.shp")

#select "La Tigra-nucleo" in Honduras
Tigra <- PAs[PAs$NOMBRE=="La Tigra-nucleo",]

#visualize it interactively
mapview(Tigra)

#determine area 
area_km2 <- expanse(Tigra, unit="km")
area_km2

#let's create an empty raster first that will become our 1 km2 grid
template <- rast(Tigra, resolution = c(1000,1000))
values(template) <- 1:ncell(template)

#then transform the raster template to polygons
tigra_1km2_grid <- as.polygons(template)

#crop this grid to the PA
tigra_1km2_grid_isect <- crop(tigra_1km2_grid, Tigra)

#determine number of grids (there are 105) 
tigra_1km2_grid_isect

#create stratified random points
random_points <- spatSample (tigra_1km2_grid_isect, size=rep(2,105), method="random")

#plot!
ggplot() +
  geom_spatvector(data=tigra_1km2_grid_isect, fill=NA, color = "black", size=1)+
  geom_spatvector(data=random_points, color = "blue", size=2)+
  labs(title="Desired Output")


