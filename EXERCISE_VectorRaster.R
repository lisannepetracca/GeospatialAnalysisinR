setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

library(sf)
library(raster)
library(units)
library(ggplot2)

#read in Hwange NP 
Hwange <- st_read("Example_Zimbabwe/Hwange_NP.shp")

#let's check out the coordinate system
crs(Hwange)
#yay, already in UTM! no need to project

area_m2 <- st_area(Hwange)
#convert to km2
area_km2 <- as.numeric(set_units(area_m2, km^2))
#print the area
(area_km2)

#let's generate 100 random pts within the park
random_points <- st_sample(Hwange, size=100, type="random", exact=T)

#read in elevation & check out CRS
elev <- raster("Example_Zimbabwe/elev_Hwange.tif")
crs(elev)

#let's plot it
plot(elev)
#elev looks good & is already in UTM 35S, yay!

#read in distance to waterhole & check out CRS
distwater <- raster("Example_Zimbabwe/Dist_Waterhole_Hwange.tif")
crs(distwater)

#let's plot it
plot(distwater)
#looks good & already in UTM 35S, yay!

#let's ensure the rasters align
distwater_align <- resample(distwater, elev, method="bilinear")
stack <- stack(elev, distwater_align)

#will need to make the random points a spatial object to use extract() function
random_pts_sp <- as(random_points,"Spatial")

#let's extract mean values for elevation and distance to waterhole for these buffers
#note that you do not use a "method" argument here, only a "FUN" argument to take the mean
raster_values <- extract(stack, random_pts_sp, buffer=1000,
                         FUN = mean, df=T)

#write these values to a .csv
write.csv(raster_values, "elev_distwater_hwange.csv")
