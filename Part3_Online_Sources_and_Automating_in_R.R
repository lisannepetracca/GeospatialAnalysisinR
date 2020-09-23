devtools::install_github("ropensci/FedData")
install.packages(c("sp", "sf", "raster", "rgeos"))
install.packages(c("move","adehabitatHR"))

library(sp)
library(sf)
library(raster)
library(rgeos)
library(move)
library(FedData)
library(adehabitatHR)


setwd("C:\\Users\\acheesem\\Desktop\\ESF\\Classes Taught\\GIS in R workshop")#Change to your working directory path

##GBIF Exercise ##Map multiple species






key<-read.csv("C:\\Users\\acheesem\\Desktop\\Mammal Atlas\\Data\\Movebank_API.csv")

##download gbif or movebank data
log<-movebankLogin(key$username,key$password)
getMovebankStudies(login=log)
getMovebankID(study="Martes pennanti LaPoint New York",login=log)
data<-getMovebankData(study=6925808,login=log)



marten<-read.csv('Martes pennanti LaPoint New York_121719\\Martes pennanti LaPoint New York.csv')
head(marten)
names(marten)
str(marten)
summary(marten)
marten<-marten[is.na(marten$location.lat)==F,]
marten$timestamp<-as.POSIXct(marten$timestamp)


#Convert to spatial object
marten<-st_as_sf(marten, coords = c("location.long", "location.lat"), crs = 4326)
marten
plot(st_geometry(marten),col=as.factor(marten$individual.local.identifier))
  


#calculate home ranges for individuals
i=1
names(marten)
marten.drop<-marten[,-c(1:21,23,24)]

l<-NA
for (i in 1:length(unique(marten.drop$individual.local.identifier))) {
  sub<-marten.drop[marten.drop$individual.local.identifier==unique(marten.drop$individual.local.identifier)[i],]
  mcp<-mcp(as_Spatial(sub), percent=95, unin = c("m"),unout = c( "m2"))
  assign(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""),mcp)
  shapefile(get(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep="")),
            paste("95p_MCP_",unique(marten.drop$individual.local.identifier)[i],".shp",sep=""),overwrite=TRUE)
  print(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""))
  l[i]<-c(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""))
}


#set bounding box for downloading imagery
ext<-extent(as_Spatial(marten))+c(-0.25,0.25,-0.25,0.25)


#get landscape data

nlcd<-get_nlcd(template = polygon_from_extent(ext,
  proj4string=paste(crs(marten))), year = 2016, dataset = "Land_Cover", label = "Marten Land", force.redo = T)
nlcd
plot(nlcd)

#Transform points to raster CRS 
marten<-st_transform(marten,crs(nlcd))
plot(st_geometry(marten),add=T,pch=16)


####Transform home range CRS
for (i in 1 :length(l)){
  out<-st_as_sf(get(l[i]))
  out<-st_transform(out,crs(nlcd))
  assign(paste(l[i],sep=""),out)
  plot(st_geometry(out),add=T,col=i)
}




#resources LP came across, ditch em or not
#import .tifs in single step and plot time series in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/12-time-series-raster/index.html
#time series aesthetics in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/13-plot-time-series-rasters-in-r/index.html