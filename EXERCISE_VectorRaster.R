#for this exercise, we will
#(1) determine the area of Hwange NP in sq km
#(2) create 100 random points within Hwange NP
#(3) use "resample" to align distance to waterhole (Dist_Waterhole_Hwange.tif)
#    and elevation (elev_Hwange.tif)
#(4) create raster stack from these two rasters
#(5) extract mean values from raster stack for 1000-m  buffers around the 100 random points
#(6) save as .csv

setwd("YOUR WORKING DIRECTORY HERE")

library(terra)
library(ggplot2)
library(tidyterra)

#read in Hwange NP 
Hwange <- vect("Example_Zimbabwe/Hwange_NP.shp")

#let's check out the coordinate system
crs(Hwange,describe=T)
#yay, already in UTM! no need to project

area_m2 <- expanse(Hwange)
#convert to km2
(area_km2 <- expanse(Hwange,"km"))

#let's generate 100 random pts within the park
random_points <- spatSample(Hwange, size=100, "random")

#read in elevation & check out CRS
elev <- rast("Example_Zimbabwe/elev_Hwange.tif")
crs(elev,describe=T)

#let's plot it
plot(elev)
#elev looks good & is already in UTM 35S, yay!

#read in distance to waterhole & check out CRS
distwater <- rast("Example_Zimbabwe/Dist_Waterhole_Hwange.tif")
crs(distwater,describe=T)

#let's plot it
plot(distwater)

#looks good & already in UTM 35S, yay!

#let's ensure the rasters align
distwater_align <- resample(distwater, elev, method="bilinear")

#and stack 'em!
stack <- c(elev, distwater_align)

plot(stack)

#create a buffer around each point to extract
random_pts_buf<-buffer(random_points,1000)

#let's extract mean values for elevation and distance to waterhole for these buffers
#note that you do not use a "method" argument here, only a "fun" argument to take the mean
#another note that I am using "raster::extract" here to pull the extract function specifically
#from the raster package, as this is a common package function and I don't want to confuse R
raster_values <- terra::extract(stack, random_pts_buf, fun=mean)

#write these values to a .csv
write.csv(raster_values, "elev_distwater_hwange.csv")
