install.packages("devtools")
devtools::install_github("ropensci/FedData")##Probably need to install this first - may need to close instances of R / restart computer
install.packages(c("sp", "sf", "raster", "rgeos"))
install.packages(c("move","adehabitatHR","lme4","ggmap"))

library(sp)
library(sf)
library(raster)
library(move)
library(FedData)
library(adehabitatHR)
library(lme4)
library(rgbif)
library(ggmap)
wd<-setwd("C:\\Users\\acheesem\\Desktop\\ESF\\Classes Taught\\GIS in R workshop")#Change to your working directory path

###Fun with loops

#make a simple loop printing every instance in 1:10
for (i in 1:10){
  print(i)
}


#make loop that prints 'yay!' if the instance = b
x<-c("a","b","c","b","d")
for (i in x){
  if(i=="b"){
    print("Yay!")
  }
}

#make loop that prints 'yay!' if the instance = b and ':(' if the instance does not =b
for (i in x){
  if(i=="b"){
    print("Yay!") 
  }else {print(":(")}
}


##Downloading shapefiles from URL
#Create pathways for each zip file to be downloaded to
ny<-("\\ny")
ele<-("\\ele")
root<- c(ny,ele)

#get URLS for zip files
files<-c(
  "http://gis.ny.gov/gisdata/fileserver/?DSID=927&file=NYS_Civil_Boundaries.shp.zip",
  "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/topo/downloads/GMTED/Grid_ZipFiles/mn30_grd.zip")

#write a loop to batch download URLS
for (i in 1:length(files)){
  download.file(files[i],paste(wd,root[i],sep=""))#this might take a few minutes to run
}

#unzip each file
for (i in 1:length(files)){
  unzip(paste(wd, root[i], sep=""))}

#read in NY boundary shaprfile & elevation raster
NY<-st_read("NYS_Civil_Boundaries_SHP\\Counties_Shoreline.shp")#this one is nested in another folder
plot(st_geometry(NY))
ele<-raster("mn30_grd")#DONT PLOT,load raster file-this is for the entire US, LARGE FILE!!



######################GBIF Exercise ###Map multiple species
#establish search criteria - searching for genus panthera
key <- name_backbone(name = 'Panthera', rank='genus')$usageKey

#run search and download 2000 records with coordinates -->
###Skip to line 60 if upload csv
panthera<-occ_search(taxonKey=key,limit=2000,hasCoordinate = TRUE)#this takes a bit of time


#save data as csv in working directory
write.csv(panthera$data,"panthera_occ.csv")

#covert tibble to data frame
pandat<-data.frame(panthera$data)


#pandat<-read.csv("panthera_occ.csv") #OR JUST READ IN CSV

#look at data
summary(pandat)
names(pandat)
str(pandat)

#convert data frame to simple feature
pandat<-st_as_sf(pandat, coords = c("decimalLongitude" ,"decimalLatitude"), crs = CRS("+proj=longlat +datum=WGS84"))


#lets look at the data colored by species
plot(st_geometry(pandat),col=as.factor(pandat$species),pch=16)

#Lets make maps for each species 

#I put my key in a saved file that can be easily referenced
key<-read.csv("C:\\Users\\acheesem\\Desktop\\Mammal Atlas\\Data\\Google_API.csv")
register_google(key = key$Key)  #need your google key, will probably need to register for a google API project using static maps https://console.developers.google.com


#load the background map outside the loop if it does not change
bgmaps <- get_map(location=c(lon=mean(st_coordinates(pandat)[,2]),lat=mean(st_coordinates(pandat)[,1])), zoom=1, scale=2,maptype="satellite") 

#loop over each species in pandat, subset data for that species, then map it and print pdf saved to wd
for (i in 1:length(unique(pandat$species))) { 
  sub<-pandat[pandat$species==unique(pandat$species)[i],]
  map<-ggmap(bgmaps) +
    geom_point(aes(x = st_coordinates(sub)[,1], y = st_coordinates(sub)[,2]) ,data = sub, colour="yellow",alpha = .5)+
    labs(x = 'latitude', y = 'longitude') + ggtitle(paste((sub$species[1]))) 
  plot(map)
  
  
  pdf(paste(unique(sub$species)[1],Sys.Date(),".pdf",sep=""),height=6,width=6)
  plot(map)
  dev.off()
}



###########################  MOVEBANK #################################################
#Neat, okay lets try GPS movement data from movebank, this is marten data from NY

#register for movebank account and save your username and pw in a csv file that can be accessed 
key<-read.csv("C:\\Users\\acheesem\\Desktop\\Mammal Atlas\\Data\\Movebank_API.csv")

##download gbif or movebank data using API
#get login credentails from file
log<-movebankLogin(key$username,key$password) 

#lists all movebank studies
getMovebankStudies(login=log)

#gets ID for study
getMovebankID(study="Martes pennanti LaPoint New York",login=log)

#download movebank data
data<-getMovebankData(6925808,login=log) ### function is not working - for now download csv


#read in CSV instead of downloading data
marten<-read.csv('Martes pennanti LaPoint New York_121719\\Martes pennanti LaPoint New York.csv')

#look at data
head(marten)
names(marten)
str(marten)
summary(marten)

#remove failed GPS fixes
marten<-marten[is.na(marten$location.lat)==F,]

#convert timestamp to date
marten$timestamp<-as.POSIXct(marten$timestamp)

#look at how many points per individual
table(marten$individual.local.identifier)


#Convert to spatial object
marten<-st_as_sf(marten, coords = c("location.long", "location.lat"), crs = 4326)

#look at the marten data
plot(st_geometry(marten),col=as.factor(marten$individual.local.identifier))
  


#calculate home ranges for individuals

#to run home range we need only the individual id and coords in a spatial points class
names(marten)
marten.drop<-marten[,-c(1:21,23,24)]

#initalize list to store names of home range shapefiles
l<-NA

#loop through all unique individuals, calculate 95% Minimum convex polygon, rename shapefile by individual, save as shapefile,
  #plot the MCP and add the name of the shapefile to l
for (i in 1:length(unique(marten.drop$individual.local.identifier))) {
  sub<-marten.drop[marten.drop$individual.local.identifier==unique(marten.drop$individual.local.identifier)[i],]
  mcp<-mcp(as_Spatial(sub), percent=95, unin = c("m"),unout = c( "m2"))
  assign(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""),mcp)
  shapefile(get(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep="")),
            paste("95p_MCP_",unique(marten.drop$individual.local.identifier)[i],".shp",sep=""),overwrite=TRUE)
  print(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""))
  l[i]<-c(paste("MCP.",unique(marten.drop$individual.local.identifier)[i],sep=""))
}


#set bounding box for downloading & working with rasters as covariates
ext<-extent(as_Spatial(marten))+c(-0.25,0.25,-0.25,0.25)

#lets crop that earlier elevation layer
ele.p<-crop(ele, ext)
plot(ele.p)

#get landscape data - Tree canopy cover from NLCD and the FedData package

nlcd<-get_nlcd(template = polygon_from_extent(ext,
  proj4string=paste(crs(marten))), year = 2016, dataset = "Tree_Canopy", label = "Marten Land", force.redo = T)

#look at it
nlcd

plot(nlcd)


#poject elevation to nlcd
ele.p<-projectRaster(ele.p,nlcd)
ele
nlcd

#Transform points to raster CRS 
marten<-st_transform(marten,crs(nlcd),silent=T)

#look at them
plot(st_geometry(marten),add=T,pch=16)



####Transform home range CRS and plot
for (i in 1 :length(l)){
  out<-st_as_sf(get(l[i]))
  out<-st_transform(out,crs(nlcd))
  assign(paste(l[i],sep=""),out)
  plot(st_geometry(out),add=T,col=i)
}

############### BASIC RESOURSE SELECTION FUCTION (RSF)  EXERCISE  ##############
#add new column to marte to name used points with 1
marten$used<-1

#look at marten
names(marten)

#subset marten to just used and individual columns, call it mart
mart<-marten[,c(22,25)]

#loop through each home range and sample as many points as there are points for that individual, 
  #add a column used with the label 0, bind to the used GPS points
for (i in 1:length(l)){
out <- st_sample(get(l[i]), nrow(marten[marten$individual.local.identifier==
                unique(marten.drop$individual.local.identifier)[i],]), type="random", exact=T)
out<-st_sf(individual.local.identifier=unique(marten.drop$individual.local.identifier)[i],used=rep(0,length(out)),geometry=out)
mart<-rbind(mart,out)
}

#check that coord systems align
crs(mart)
crs(nlcd)

#extract raster data to points
mart$LC<-extract(nlcd,mart)
mart$ele<-extract(ele.p,mart)

#run rsf
model<-glm(used~-1+(LC)+scale(ele), data=mart,family='binomial') #takes a few moments to run 
summary(model)

#scale elevation with nlcd data
ele<-resample(ele.p,nlcd)

#create grid of points across landscape
samp<-st_sample (st_as_sf(polygon_from_extent(extent(nlcd),
                                     proj4string=paste(crs(nlcd)))), 105625, type="regular", exact=T)

#make grid of points an sf object
samp<-st_sf(samp)

#extract raster data to background points
samp$ele<-extract(ele.p,samp)
samp$LC<-extract(nlcd,samp)



#create predictions across surface
samp$pred<-as.numeric(predict(model,newdata=samp,type='response'))

#scale predictions
samp$pred<-samp$pred/max(samp$pred,na.rm=T)
min(samp$pred,na.rm=T)

#need to lower resolution of raster for processing
p.ras<-aggregate(nlcd, fact=12)

#rasterize the background point predictions
d<-rasterize(as_Spatial(samp),p.ras,"pred",na.rm=F)
plot(d)


#add the home ranges to the map
for (i in 1:length(l)){
plot(st_geometry(get(l[i])),lwd=3,add=T)
}



#resources LP came across, ditch em or not
#import .tifs in single step and plot time series in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/12-time-series-raster/index.html
#time series aesthetics in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/13-plot-time-series-rasters-in-r/index.html