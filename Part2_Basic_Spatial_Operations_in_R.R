install.packages(c("sp", "sf", "raster"))
library(sp)
library(sf)
library(raster)
setwd("C:\\Users\\acheesem\\Desktop\\ESF\\Classes Taught\\GIS in R workshop")#Change to your working directory path

##part 1
#Make an object
Obj<-"x"
Obj

#Make a list
lis<-c(1,5,7)

#select the second item in the list
lis[2]

#select items 2 through 3 in the list
lis[c(2:3)]

#select items 1 and 3 in the list
lis[c(1,3)]

#remove item 2 from the list, store in new object called 'lis2'
lis2<-lis[-2]

lis2# look at lis2

#replace item 2 in lis with the number 25
lis[2]<-25
lis#look at lis

#make a data frame
df<-data.frame(letters=c("a","b","c"),numbers=lis)

#inspect data
head(df) #look at first few rows
tail(df)#look at last few rows
str(df)#look at data structure
summary(df)#look at data summaries

#look at numbers column
df$numbers

#look at numbers column using index
df[,2]

#look at row 3 in numbers column
df[3,2]

#Take the mean of the numbers column
mean(df$numbers)

##create unprojected spatial data
data<-data.frame(long=c(-76.13332,-76.86515,-76.851651),
                 lat=c(42.85632,42.65465,42.51311))
plot(data)#plot spatial data


##Create projected spatial data with sp
#define coordinate system using EPSG code
crdref <- crs("+init=epsg:4326")
#inspect the CRS
crdref

#create spatial points object
pts <- SpatialPoints(cbind(data$long,data$lat), proj4string=crdref)

#inspect pts
pts
plot(pts)

##Create spatialpointsdataframe
#Create attributes

att<-data.frame(site=c("Pond","River","Forest"),ID=1:nrow(data))
spdf<-SpatialPointsDataFrame(pts,data=att,proj4string = crdref)
#str(spdf)#look at structure
spdf
#plot(spdf)

#write spdf to a shapefile
shapefile(spdf,"myshapefile.shp",overwrite=T)

#read in myshapefile
shp<-shapefile("myshapefile.shp")

#Inspect and check loaded shapefile
class(shp)#look at class
head(shp)#look at data
str(shp)#look at structure
crs(shp)#look at coordinate reference system
plot(shp)#plot shapefile


geo_data<-data.frame(shp)

### sf package
sf.pts<-st_as_sf(geo_data, coords = c("coords.x1", "coords.x2"), crs = crs(shp))

#inspect
sf.pts

#read in sf
nc <- st_read("myshapefile.shp")

#write with sf
st_write(nc, "myshapefile.shp", delete_layer = TRUE)



#####Raster package
#define raster 
r <- raster(ncol=13, nrow=10,crs=crs(shp))

#assign values to raster
values(r) <- c(rep(0,16),1,rep(0,5),1,rep(0,7),1,0,0,
               0,1,rep(0,7),rep(1,7),0,0,0,0,0,1,1,0,1,1,1,0,1,1,0,0,0,rep(1,11),
               0,0,1,0,rep(1,7),0,1,0,0,1,0,1,rep(0,5),1,0,1,0,0,0,0,0,1,1,0,
               1,1,rep(0,17))
#inspect raster
r

#plot raster
plot(r,col=c("black","green"))

#save raster to file
writeRaster(r,"myraster.tif",overwrite=T)

#load raster from file
r2 <- raster("myraster.tif")

#plot raster to inspect
plot(r2,col=c("black","green"))

