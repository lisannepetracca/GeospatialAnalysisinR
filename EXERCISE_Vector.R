setwd("C:\\Users\\lspetrac\\Desktop\\Geospatial_Analysis_in_R")

library(sf)
library(units)
library(ggplot2)

#read in PAs
PAs <- st_read("Example_Honduras\\Honduras_Protected_Areas_2007.shp")

#select "La Tigra-nucleo" in Honduras
Tigra <- PAs[PAs$NOMBRE=="La Tigra-nucleo",]

#determine area and perimeter
st_area(Tigra)

#convert the areas in m2 to km2
Tigra$area_m2 <- st_area(Tigra)
#convert to km2
Tigra$area_km2 <- as.numeric(set_units(Tigra$area_m2, km^2))
#print the area
Tigra$area_km2

#then we will create a 1 km2 grid (1 km x 1 km) over the PA
tigra_1km2_grid <- st_make_grid(
  Tigra,
  cellsize = 1000,
  crs = 32616,
  what = "polygons",
  square = TRUE
)

#determine number of grids (ok, so there are 33)
tigra_1km2_grid

#create stratified random points
random_points <- st_sample (tigra_1km2_grid, size=rep(2,106), type="random", exact=T)

#plot!
ggplot() +
  geom_sf(data = Tigra, color = "purple", size=1.5) +
  geom_sf(data=tigra_1km2_grid, fill=NA, color = "black", size=2)+
  geom_sf(data=random_points, color = "blue", size=2)+
  labs(title="Desired Output")


