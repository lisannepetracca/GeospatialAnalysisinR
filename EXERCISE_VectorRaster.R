#for this exercise, we will
#(1) determine the area of Hwange NP in sq km
#(2) create 100 random points within Hwange NP
#(3) use "resample" to align distance to waterhole (Dist_Waterhole_Hwange.tif)
#    and elevation (elev_Hwange.tif)
#(4) create raster stack from these two rasters
#(5) extract mean values from raster stack for 1000-m buffers around the 100 random points
#(6) save as .csv

#change this to your working directory
setwd("C:/Users/lspetrac/Desktop/Geospatial_Analysis_in_R")

library(terra)
library(tidyterra)
library(ggplot2)

#read in Hwange NP 
Hwange <- vect("Example_Zimbabwe/Hwange_NP.shp")

#let's check out the coordinate system
crs(Hwange, describe=T)
#yay, already in UTM! no need to project

area_km2 <- expanse(Hwange, unit="km")
area_km2

#let's generate 100 random pts within the park
random_points <- spatSample(Hwange, size=100, method="random")

#read in elevation & check out CRS
elev <- rast("Example_Zimbabwe/elev_Hwange.tif")
crs(elev, describe=T)

#let's plot it
plot(elev)
#elev looks good & is already in UTM 35S, yay!

#read in distance to waterhole & check out CRS
distwater <- rast("Example_Zimbabwe/Dist_Waterhole_Hwange.tif")
crs(distwater, describe=T)

#let's plot it
plot(distwater)
#looks good & already in UTM 35S, yay!

#let's ensure the rasters align
distwater_align <- resample(distwater, elev, method="bilinear")
stack <- c(elev, distwater_align)

#let's extract mean values for elevation and distance to waterhole for these buffers
#note that we are using a "fun" argument to take the mean
raster_values <- extract(stack, random_points, buffer=1000, fun = mean, df=T)

#write these values to a .csv
write.csv(raster_values, "elev_distwater_hwange.csv")
