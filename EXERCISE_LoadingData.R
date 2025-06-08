library(terra)
#Change to your working directory path where the .tif is located
setwd("YOUR WORKING DIRECTORY HERE")
#Example: setwd("C:/Users/KUJNH012/Documents/OcelotPhd/Rcode/GeospatialAnalysisinR")

#load in a .tif image
ras <- rast("TorresDelPaine.tif")
plot(ras)

#inspect ras to get the min and max x and y values
ext <- ext(ras)
ext

#create x coordinates
x <- seq(ext[1], ext[2], length.out = 25)

# create y coordinares
y <- seq(ext[3], ext[4], length = 25)

#create grid of x and y points
grid <- expand.grid(x = x, y = y)

# convert to SpatVector
points<-vect(grid,geom=c("x","y"), crs=crs(ras))
plot(ras, add = F)
plot(points,col="red",pch=16,add=T)