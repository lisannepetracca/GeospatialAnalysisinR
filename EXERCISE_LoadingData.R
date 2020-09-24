library(raster)
setwd("C:/Users/acheesem/Desktop/ESF/Classes Taught/GIS in R workshop/AM Exercise")
ras<-raster("moon.tif")


x<-runif(25,0,350)
y<-runif(25,0,500)
df<-data.frame(x=x,y=y)
points<-st_as_sf(df, coords = c("x", "y"), crs = crs(ras))


plot(ras)
plot(points,add=T,pch=16)


