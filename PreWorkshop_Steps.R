

# ---- WELCOME TO THE GEOSPATIAL ANALYSIS IN R WORKSHOP! ----

# ---- (it will be a good time!) ----


#first we will set our working directory
#don't forget to keep the / syntax in the directory location (\ does not work)

setwd("C:/PASTE THE LOCATION OF WHERE YOUR Geospatial_Analysis_in_R FOLDER IS")
#Hint: you can find the directory in file explorer and copy/paste 

#Let's run through some steps to make sure we are good to go for the workshop

#First we need to install some packages

#*IF IT ASKS TO RESTART R, YOU CAN SAY YES*** 
#*IF IT ASKS FOR YOUR CRAN MIRROR YOU CAN SAY CLOUD
install.packages(c("terra", "ggplot2", "rnaturalearth", "tidyterra", "viridis",
                   "ggspatial", "cowplot", "sf", "tmap", "rgbif","keyring","ggmap","move2",
                   "adehabitatHR","FedData","raster","tidyverse"),dependencies=T)

#*RED TEXT DOES NOT MEAN IT DIDN'T WORK. THERE IS ONLY AN ISSUE IF YOU SEE SOMETHING LIKE "Error in install.packages>"***

#now let's get these packages loaded into R
library(terra)
library(ggplot2)
library(rnaturalearth)
library(tidyterra)
library(tidyverse)
library(viridis)
library(ggspatial)
library(sf)
library(tmap)
library(cowplot)
library(rgbif)
library(sf)
library(ggmap)
library(move2)
library(adehabitatHR)
library(FedData)
library(raster)

#we are ALL GOOD on packages if you do not get any error messages after running these "library" lines

#and then read in two types of geospatial data
elev <- rast("PreWorkShop Steps/aster_image_20160624.tif")
honduras_boundary <- vect("PreWorkShop Steps/Honduras_Border.shp")

#if no errors, then WHOOO HOOO! WE'RE DONE!
