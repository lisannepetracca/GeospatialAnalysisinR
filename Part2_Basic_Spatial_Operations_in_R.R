# ---- PART 2: BASIC SPATIAL OPERATIONS IN R ----

#let's set our working directory first
#this is where outputs from this section will be written
setwd("C:/PASTE YOUR WORKING DIRECTORY HERE")
#setwd("C:/Users/lisan/OneDrive - Texas A&M University - Kingsville/Geospatial_Analysis_in_R")

#install.packages(c("terra"))
library(terra)

###YOU CAN WORK WITH SPATIAL DATA AS DATA FRAME 
##let's create unprojected spatial data 
data <- data.frame(long=c(-76.13332,-76.86515,-76.851651), # c() concatenates values separated by commas 
                 lat=c(42.85632,42.65465,42.51311))
data #Inspect to see what data looks like

#plot spatial data
plot(data)
#note if "Error in plot.new() : figure margins too large" resize the plot window 
#in the (default lower right window) to be larger
#Here we are using data created in R but could also create spatial points from any .csv, .txt, etc. with coordinates

# ---- SPATIAL DATA TYPES IN TERRA ----

##Create projected spatial data from data.frame with terra
#define coordinate system using EPSG code
coord_ref <- "EPSG:4326"

#create SpatVector class object named pts from data
?vect  #use the ? before a function to see which arguments are needed and their format!! 
pts <-  vect(data,geom = c("long", "lat"), crs = coord_ref)

#inspect pts
pts
plot(pts)

# ---- CREATE A spatVector OBJECT WITH DATA ----

# Create attributes corresponding to the rows from data 
# (alternatively you would pull from your database/csv etc.)
# here creating sites pond, river, and forest, and ID for each row in data
attributes <-data.frame(site=c("Pond","River","Forest"),ID=1:nrow(data))

#look at attributes
attributes

#use good old cbind() function to add attributes to points
spatvector.df<-cbind(pts,attributes)

#look at spatvector.df
spatvector.df

#write sv.df to a shapefile using function writeVector() 
writeVector(spatvector.df,"Part2_Basic_Spatial_Operations/myshapefile.shp",overwrite=T)
#Does it have to be in shapefile format? NO!! https://gdal.org/drivers/vector/index.html 75+ drivers! 
  
#read in myshapefile using the vect() function 
shp <- vect("Part2_Basic_Spatial_Operations/myshapefile.shp")

#Inspect and check loaded simple features object created from the shapefile
shp #look at shp
str(shp) #look at data structure
crs(shp) #look at coordinate reference system - can be saved to object & applied to other datasets

#wow thats a lot -how about we try this
crs(shp,describe=T)
#much better!!

#lets plot it
plot(shp,col=as.factor(shp$site))#make each site a different color

##Convert the spatVector object to data frame
shp_df <-data.frame(shp)

#look at shp_df
shp_df

#aww no coordinates -what if we want them??
#get coordinates
coords<-data.frame(crds(shp))

#and combine/view
(shp_df_coords<-cbind(shp_df,coords))


# ---- WHAT ABOUT OTHER VECTOR PACKAGES? ----

#sp is out of date but sf still good!

#convert sp/sf to spatVector object using vect()
#Create a sp object using as(shp,"Spatial")
#(need to load both packages sp and raster)

#convert from spatVector to simplefeatures using st_as_sf()
library(sf)#load the sf library

#Check your spatVector object
shp

#and convert & inspect!
(simple_features<-st_as_sf(shp))

#convert a simple features (back) to a spatVector using vect()
(spat_vector<-vect(simple_features))
#seem familiar?


# ---- WHAT ABOUT CREATING A RASTER? ----

# Create a matrix of values using the terra package

?rast #see what arguments are required to make a raster note :: calls package terra

#let's copy our crs from 'shp'
crs.shp<-crs(shp)

#let's inspect
crs.shp

#okay lets make our raster from scratch & give it 10 rows and 13 columns 
#plotted in WGS84
raster <- rast(nrows=10, ncols=13, crs=crs.shp)

#create 10 x 13 =130 values and assign values to raster
values(raster)<- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,1,0,0,0,0,0,0,0,1,0,0,0,1,
              0,0,0,0,0,0,0,1,1,1,1,1,1,1,0,0,0,0,0,1,1,0,1,1,1,0,1,1,0,0,0,1,1,1,1,
              1,1,1,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,0,0,1,0,1,0,0,0,0,0,1,0,1,0,0,
              0,0,0,1,1,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#inspect raster
raster

#look at its crs
crs(raster,describe=T)

#plot raster
plot(raster,col=c("black","green"))

#save raster to file
writeRaster(raster,"Part2_Basic_Spatial_Operations/myraster.tif",overwrite=T)

#load raster from file
raster_2 <- rast("Part2_Basic_Spatial_Operations/myraster.tif")

#plot raster to inspect
plot(raster_2,col=c("black","purple"))

