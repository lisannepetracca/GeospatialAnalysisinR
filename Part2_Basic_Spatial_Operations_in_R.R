# ---- PART 2: BASIC SPATIAL OPERATIONS IN R ----

#let's set our working directory first
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")

#install.packages(c("terra"))
library(terra)

###CAN WORK WITH SPATIAL DATA AS DATA FRAME 
##create unprojected spatial data 
#NO PROJECTION - NOT GOOD
data<-data.frame(long=c(-76.13332,-76.86515,-76.851651), # c() concatenates values separated by commas 
                 lat=c(42.85632,42.65465,42.51311))
data #Inspect to see what data looks like

#plot spatial data
plot(data)
#note if "Error in plot.new() : figure margins too large" resize the plot window 
#in the (default lower right window) to be larger


#########################################################################
##### SPATIAL DATA TYPES in terra

##Create projected spatial data from data.frame with terra
#define coordinate system using EPSG code
crdref <- "EPSG:4326"

#create SpatVector class object names pts from data
?vect# use the ? before a function to see which arguments are needed and their format!! 
pts <-  vect(data,geom = c("long", "lat"), crs = crdref)

#inspect pts
pts
plot(pts)

####Create an spatVector object with data######

#Create attributes corresponding to the rows from data 
#(alternatively you would pull from your database/csv etc.)
# here creating sites pond, river, and forest, and ID for each row in data
att<-data.frame(site=c("Pond","River","Forest"),ID=1:nrow(data))

#look at att
att

#use good old cbind() function to add attributes to points
sv.df<-cbind(pts,att)

#look at sf.df
sv.df

#write sv.df to a shapefile using function writeVector() 
writeVector(sv.df,"myshapefile.shp",overwrite=T)

#read in myshapefile using the vect() function 
shp<-vect("myshapefile.shp")

#Inspect and check loaded simple features object created from the shapefile
shp#look shp
str(shp)#look at data

crs(shp)#look at coordinate reference system - note can be saved to object & applied to other datasets

#wow thats a lot -how about we try this
crs(shp,describe=T)
#much better!!

#lets plot it
plot(shp,col=as.factor(shp$site))#where 1 specifies the data column number to plot

##Convert the spatVector object to data frame
geo_data<-data.frame(shp)

#look at geo_data
geo_data

#aww no coordinates -what if we want them??
#get coordinates
coords<-data.frame(crds(shp))

#and combine/view
(geo_dat_coords<-cbind(geo_data,coords))

###What about the other vector packages 
#sp is out of date but sf still good!

#convert sp to spatVector object using vect()

#convert from spatVector to simplefeatures using st_as_sf()
library(sf)#load the sf library

#Check your spatVector object
shp

#and convert & inspect!
(simple_features<-st_as_sf(shp))

#convert a simple features (back) to a spatVector using vect()
(spat_vector<-vect(simple_features))
#seem familiar?

#####Raster 

# Create a matrix of values using the terra package

?rast #see what arguments are required to make a raster note :: calls package terra

#let's copy our crs from 'shp'
crs.shp<-crs(shp)

#let's inspect
crs.shp

#okay lets make our raster from scratch & give it 10 rows and 13 columns 
#plotted in WGS84
r <- rast(nrows=10, ncols=13, crs=crs.shp)

#create 10 x 13 =130 values and assign values to raster
values(r)<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,
              0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,1,1,1,0,1,1,0,0,0,1,1,1,1,
              1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,
              0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#inspect raster
r

#look at its crs
crs(r,describe=T)

#plot raster
plot(r,col=c("black","green"))

#save raster to file
writeRaster(r,"myraster.tif",overwrite=T)

#load raster from file
r2 <- rast("myraster.tif")

#plot raster to inspect
plot(r2,col=c("black","purple"))

