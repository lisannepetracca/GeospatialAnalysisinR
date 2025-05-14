library(terra)
#Change to your working directory path where the .tif is located
setwd("YOUR WORKING DIRECTORY HERE")
#Example: setwd("C:/Users/KUJNH012/Documents/OcelotPhd/Rcode/GeospatialAnalysisinR")

#load in a .tif image
ras <- rast("moon.tif")
plot(ras)

#inspect ras to get the min and max x and y values
ras
x<-runif(25,1,358) #this is generating 25 random values between 1 and 358
y<-runif(25,1,500) #this is generating 25 random values between 1 and 500
df<-data.frame(x=x,y=y)
points<-vect(df,geom=c("x","y"), crs=crs(ras))

plot(ras,add=F)
plot(points,col="red",pch=16,add=T)


