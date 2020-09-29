#for this exercise, we will
#(1) select "La Tigra-nucleo" from the Honduras PAs shapefile
#(2) determine area of that PA in km2
#(3) create a 1 km x 1 km grid over the PA
#(4) clip that grid to the PA boundary
#(5) create 2 random points in each of those grid cells
#(6) plot it!


setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

library(sf)
library(units)
library(ggplot2)

#read in PAs
PAs <- st_read("Example_Honduras/Honduras_Protected_Areas_2007.shp")

#select "La Tigra-nucleo" in Honduras
Tigra <- PAs[PAs$NOMBRE=="La Tigra-nucleo",]

#determine area 
area_m2 <- st_area(Tigra)
#convert to km2 & print value
(area_km2 <- as.numeric(set_units(area_m2, km^2)))

#then we will create a 1 km2 grid (1 km x 1 km) over the PA
tigra_1km2_grid <- st_make_grid(
  Tigra,
  cellsize = 1000,
  crs = 32616,
  what = "polygons",
  square = TRUE
)

#intersect this grid with the PA
tigra_1km2_grid_isect <- st_intersection(tigra_1km2_grid, Tigra)

#determine number of grids (there are 106) 
tigra_1km2_grid_isect

#create stratified random points
random_points <- st_sample (tigra_1km2_grid_isect, size=rep(2,106), type="random")

#plot!
ggplot() +
  geom_sf(data=tigra_1km2_grid_isect, fill=NA, color = "black", size=2)+
  geom_sf(data=random_points, color = "blue", size=2)+
  labs(title="Desired Output")


