install.packages(c("sp", "sf", "raster", "rgeos"))
install.packages(c("move",))
devtools::install_github("ropensci/FedData")

library(sp)
library(sf)
library(raster)
library(rgeos)
library(move)
library(FedData)
setwd("C:\\Users\\acheesem\\Desktop\\ESF\\Classes Taught\\GIS in R workshop")#Change to your working directory path

##download gbif or movebank data
getMovebankStudies()
getMovebankData(study, animalName, login, ...)

marten<-read.csv('Martes pennanti LaPoint New York_121719\\Martes pennanti LaPoint New York.csv')
head(marten)
names(marten)
str(marten)
summary(marten)
marten<-marten[is.na(marten$location.lat)==F,]
str(marten)
marten<-st_as_sf(marten, coords = c("location.long", "location.lat"), crs = 4326)
marten
plot(st_geometry(marten),col=marten$individual.local.identifier)


#add weather data
names(marten)

#get landscape data
nlcd<-get_nlcd(template=polygon_from_extent(
  raster::extent(-112, -111, 33, 34),
  proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
), label="LaPoint", year = 2016, dataset = "landcover", force.redo = T)
nlcd<-get_nlcd(template = as_Spatial(marten), year = 2011, dataset = "canopy", label = "NLCD2011can")

#calculate home ranges for individuals


#resources LP came across, ditch em or not
#import .tifs in single step and plot time series in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/12-time-series-raster/index.html
#time series aesthetics in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/13-plot-time-series-rasters-in-r/index.html