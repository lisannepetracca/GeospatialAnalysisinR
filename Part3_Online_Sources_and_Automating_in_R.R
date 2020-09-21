#### AMANDAS CODE
install.packages(c("sp", "sf", "raster", "rgeos"))
library(sp)
library(sf)
library(raster)
library(rgeos)

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
crdref <- CRS("+init=epsg:4326")
crdref
pts <- SpatialPoints(data, proj4string=crdref)
plot(pts)


#resources LP came across, ditch em or not
#import .tifs in single step and plot time series in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/12-time-series-raster/index.html
#time series aesthetics in ggplot: https://datacarpentry.org/r-raster-vector-geospatial/13-plot-time-series-rasters-in-r/index.html