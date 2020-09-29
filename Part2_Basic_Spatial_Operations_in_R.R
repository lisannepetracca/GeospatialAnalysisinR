#install.packages(c("sp", "sf", "raster"))
library(sp)
library(sf)
library(raster)
setwd("C:/Users/acheesem/Desktop/ESF/Classes Taught/GIS in R workshop")#Change to your working directory path

##part 1
#Make an object called obj with 1 element 'x'
Obj<-"x"

#run obj it will return 'x'
Obj

#Make a vector (NOTE this is not the same as a spatial vector data type)
lis<-c(1,5,7)

#inspect 'lis'
lis

#select the second item in the vector lis
lis[2]

#select items 2 through 3 in lis
lis[c(2:3)]

#select items 1 and 3 in the list
lis[c(1,3)]

#remove item 2 from lis, store in new object called 'lis2'
lis2<-lis[-2]

lis2# look at lis2

#replace item 2 in lis with the number 25
lis[2]<-25
lis#look at lis

#make a data frame called df
df<-data.frame(letters=c("a","b","c"),numbers=lis)

#inspect df
df

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

#Run calculations or operations on data
#Take the mean of the numbers column
mean(df$numbers)


###CAN WORK WITH SPATIAL DATA AS DATA FRAME 
##create unprojected spatial data 
  #NO PROJECTION - NOT GOOD
data<-data.frame(long=c(-76.13332,-76.86515,-76.851651),
                 lat=c(42.85632,42.65465,42.51311))

#plot spatial data
plot(data)

#########################################################################
##### SPATIAL DATA TYPES SP, SF AND RASTER

##Create projected spatial data with sp
#define coordinate system using EPSG code
crdref <- crs("+init=epsg:4326")
#inspect the CRS
crdref

#create spatial points class object names pts from data
data#remember what data looks like?
pts <- SpatialPoints(cbind(data$long,data$lat), proj4string=crdref)

#inspect pts
pts
plot(pts)

##Create spatialpointsdataframe
#Create attributes corresponding to the row from data 
  #(alternitively you would pull from your database/csv etc.)
  # here creating sites pond, river, and forest, and ID for each row in data
att<-data.frame(site=c("Pond","River","Forest"),ID=1:nrow(data))
#look at att
att

#use SpatialPOintsDataFrame() function to add attributes to points
spdf<-SpatialPointsDataFrame(pts,data=att,proj4string = crdref)


#look at spdf
spdf
#plot(spdf)

#write spdf to a shapefile using function shapefile () in the raster package
shapefile(spdf,"myshapefile.shp",overwrite=T)

#read in myshapefile using the shapefile() function in the raster package
shp<-shapefile("myshapefile.shp")

#Inspect and check loaded shapefile
class(shp)#look at class
head(shp)#look at data
str(shp)#look at structure
crs(shp)#look at coordinate reference system - note can be saved to object & applied to other datasets
plot(shp)#plot shapefile


##Convert the shp to data frame
geo_data<-data.frame(shp)

#look at geo_data
geo_data

### create spatial class using sf package and geo_data
sf.pts<-st_as_sf(geo_data, coords = c("coords.x1", "coords.x2"), crs = crs(shp))

#inspect
sf.pts


#alternatively we could convert directly from sp object
st_as_sf(shp)

#read .shp with as sf using st_read() function
nc <- st_read("myshapefile.shp")

#write  sf to .shp with st_write() function
st_write(nc, "myshapefile.shp", append=F)



#####Raster package
#create raster - define columns, rows, and crs 
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
plot(r2,col=c("black","purple"))

